module Category.Transaction.Hom

import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import JSON

--import Category.Transaction.Qty
import Category.Transaction.Types
import Data.Ratio

%language ElabReflection


public export
get_path : Term -> TermPath   -- user binary tree instead of list
--get_path (Ref j1) = [j1]
--get_path (Ch t1 h1) = [(Acc a1 a2)]
--get_path (Lst xs) = []
--get_path (Pro j1 t1 t2) = [j1] ++ (get_path t1) ++ (get_path t2)
--get_path (Co j1 t1 t2) = [j1] ++ (get_path t1) ++ (get_path t2)
--get_path (Co j1 t1 t2) = [j1] ++ (get_path t1) ++ (get_path t2)


public export
get_hom1 : Term -> List Hom1
get_hom1 (Ref j1) = []
get_hom1 (Ch j1  h1) = [h1]
get_hom1 (Pro j1 t1 t2) = (get_hom1 t1) ++ (get_hom1 t2)
get_hom1 (Co j1 t1 t2) = (get_hom1 t1) ++ (get_hom1 t2)


--partial
--evTerm : Term  -> Term
--evTerm (Co (Ch j1 a1 a2 h1)  (Ch j2 b1 b2 h2)  ) = ?xss


public export
th11 : Hom1
th11 = [ Debit ("p1",4), Debit ("p2",3), Debit ("p1", 9) ]

public export
th11L : List TProduct
th11L = th11


public export
th11' : Hom1
th11' = [ Debit ("p1",3), Debit ("p1", 6) ]


public export
th12 : Hom1
th12 = [ Debit ("GBP",38) ]


merge_item_into : (SortedMap ProdKey TQty) -> (ProdKey, TQty) -> (SortedMap ProdKey TQty)
merge_item_into acc x = mergeWith (+) acc (fromList [x])

fromProductList : List Product -> SortedMap ProdKey TQty
fromProductList xs = foldl merge_item_into empty xs

evalProductList : List Product -> List Product
evalProductList xs = toList $ fromProductList xs 


public export
getKey : TProduct -> ProdKey
getKey (Debit (k,v)) = k
getKey (Credit (k,v)) = k

public export
getVal : TProduct -> TQty
getVal (Debit (k,v)) = v
getVal (Credit (k,v)) = v



unTQty : TQty -> TQty -> TQty 
unTQty (Debit a) (Debit b) = Debit (min a b)
unTQty (Credit a) (Credit b) = Credit (min a b)
unTQty (Debit a) (Credit b) = Debit 0
unTQty (Credit a) (Debit b) = Debit 0


partial
public export
merge_item_into2 : (SortedMap ProdKey TProduct) -> TProduct -> (SortedMap ProdKey TProduct)
merge_item_into2 acc x = case (lookup (getKey x) acc) of
                             Nothing => (insert (getKey x) x acc)
                             Just v => case ( (getVal v)+(getVal x) ) of 
                                           Debit 0 => (delete (getKey x) acc)
                                           Debit nv => (insert (getKey x) (Debit (getKey x, (Debit nv)) )  acc)
                                           
                                           Credit nv => (insert (getKey x) (Credit (getKey x,(Credit nv) ) )  acc)

{-
partial
public export
merge_item_into3 : (SortedMap ProdKey Product) -> Product -> (SortedMap ProdKey Product)
merge_item_into3 acc x = case (lookup (getKey x) acc) of
                             Nothing => (insert (getKey x) x acc)
                             Just v => case ( (getVal v)+(getVal x) ) of 
                                           Debit 0 => (delete (getKey x) acc)
                                           Debit nv => (insert (getKey x) (Debit (getKey x,nv) )  acc)
                                           Credit nv => (insert (getKey x) (Credit (getKey x,nv) )  acc)
-}

partial
public export
merge_as_union : (SortedMap ProdKey TProduct) -> TProduct -> (SortedMap ProdKey TProduct)
merge_as_union acc x = case (lookup (getKey x) acc) of
                             Nothing => acc
                             Just v => case (unTQty (getVal v) (getVal x) ) of 
                                           Debit 0 => (delete (getKey x) acc)
                                           Debit nv => (insert (getKey x) (Debit (getKey x, Debit nv) )  acc)
                                           Credit nv => (insert (getKey x) (Credit (getKey x,Credit nv) )  acc)

public export                             
fromTProductList : List TProduct -> SortedMap ProdKey TProduct
fromTProductList xs = foldl merge_item_into2 empty xs


public export 
unionHom1' : Hom1 -> Hom1 -> SortedMap ProdKey TProduct
unionHom1' a b = foldl merge_as_union (fromTProductList a) b

public export
unionHom1 : Hom1 -> Hom1 -> Hom1
unionHom1 a b = toList $ unionHom1' a b

public export
evalTProductList : List TProduct -> List TProduct
evalTProductList xs = toList $ fromTProductList xs 


public export
id_hom1 : Hom1
id_hom1 = []

public export
invHom1 : Hom1 -> Hom1
invHom1 [] = []
invHom1 (Debit x::xs) = [Credit x] ++ (invHom1 xs)
invHom1 (Credit x::xs) = [Debit x] ++ (invHom1 xs)

public export
addHom1 : Hom1 -> Hom1 -> Hom1 --optimize later
addHom1 x y = x ++ y

public export
debitsHom1 : Hom1 -> List Product
debitsHom1 (Debit x::xs) = [x] ++ (debitsHom1 xs)
debitsHom1 (Credit x::xs) = debitsHom1 xs --drop the credit
debitsHom1 [] = []

public export
creditsHom1 : Hom1 -> List Product
creditsHom1 (Debit x::xs) = creditsHom1 xs
creditsHom1 (Credit x::xs) = [x] ++ creditsHom1 xs
creditsHom1 [] = []

public export
evalHom1 : Hom1 -> Hom1
evalHom1 xs =  evalTProductList xs  --[Debit x | x <- (evalProductList (debitsHom1 xs))] ++ [Credit x | x <- (evalProductList (creditsHom1 xs))] 

public export
diffHom1 : Hom1 -> Hom1 -> Hom1
diffHom1 x y = addHom1 x (invHom1 y)


public export
evDiffHom1 : Hom1 -> Hom1 -> Hom1
evDiffHom1 x y = (evalHom1 (diffHom1 x y))

public export
eqHom1 : Hom1 -> Hom1 -> Bool
eqHom1 x y = (evDiffHom1 x y) == id_hom1




public export
th3 : Hom1
th3 = diffHom1 th11 th12

{-
partial
monoTerm : Term -> Term -> Term
monoTerm ID (Ch a1 a2 h1) = Ch a1 a2 h1
monoTerm (Ch a1 a2 h1) ID = Ch a1 a2 h1
monoTerm (Ch a1 a2 h1) (Ch b1 b2 h2) = if (a2==b1) then (Ch a1 b2 (unionHom1 h1 h2 ) ) else ID


eq_accounts: Term -> Term -> Bool
eq_accounts (Ch j1 a1 a2 h1) (Ch j2 b1 b2 h2) = ( (a1==b1) && (a2==b2) && (j1==j2) )
eq_accounts _ _ = False

-}


--partial
--monoTerm : Term -> Term -> Term
--monoTerm t1@(Ch j1 a1 a2 h1) t2@(Ch j2 b1 b2 h2) = if (eq_accounts t1 t2) then (Ch j1 a1 a2 (unionHom1 h1 h2 ) ) else Lst [t1,t2]
--monoTerm (Co t11 t12) (Co t21 t22) = ?monoid_for_composition

--monoTerm (Lst xs) (Lst []) = Lst xs
--monoTerm (Lst []) (Lst xs) = Lst xs
--monoTerm (Lst xs) (Lst ys) = Lst (xs++ys)
--monoTerm (Lst xs) t = Lst (xs ++ [t])
--monoTerm t (Lst xs) = Lst ([t] ++ xs)

--monoTerm t1 t2 = Lst ([t1] ++ [t2]) 

--monoTerm t1@(Ch j1 a1 a2 h1) t2 = Lst [t1,t2]
--monoTerm t1 t2@(Ch j1 a1 a2 h1) t2 = Lst [t1,t2]

--monoTerm (Ch a1 a2 h1) ID = Ch a1 a2 h1

