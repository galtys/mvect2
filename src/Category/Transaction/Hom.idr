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
get_hom1 : Term -> List Hom1
get_hom1 (Ref j1) = []
get_hom1 (Ch j1  h1) = [h1]
get_hom1 (Pro j1 t1 t2) = (get_hom1 t1) ++ (get_hom1 t2)
get_hom1 (Co j1 t1 t2) = (get_hom1 t1) ++ (get_hom1 t2)


public export
th11 : Hom1
th11 = [ ("p1",4), ("p2",3), ("p1", 9) ]

public export
th11L : List Product
th11L = th11


public export
th11' : Hom1
th11' = [ ("p1",3), ("p1", 6) ]


public export
th12 : Hom1
th12 = [ ("GBP",38) ]


unTQty : TQty -> TQty -> TQty 
unTQty (Debit a) (Debit b) = Debit (min a b)
unTQty (Credit a) (Credit b) = Credit (min a b)
unTQty (Debit a) (Credit b) = Debit 0
unTQty (Credit a) (Debit b) = Debit 0

partial
public export
merge_as_union : (SortedMap ProdKey TQty) -> Product -> (SortedMap ProdKey TQty)
merge_as_union acc x = case (lookup (fst x) acc) of
                             Nothing => acc
                             Just v => case (unTQty v (snd x) ) of 
                                           0 => (delete (fst x) acc)
                                           nv => (insert (fst x) nv acc)                                           

merge_item_into : (SortedMap ProdKey TQty) -> (ProdKey, TQty) -> (SortedMap ProdKey TQty)
merge_item_into acc x = mergeWith (+) acc (fromList [x])

fromProductList : List Product -> SortedMap ProdKey TQty
fromProductList xs = foldl merge_item_into empty xs

evalProductList : List Product -> List Product
evalProductList xs = toList $ fromProductList xs 


public export 
unionHom1' : Hom1 -> Hom1 -> SortedMap ProdKey TQty
unionHom1' a b = foldl merge_as_union (fromProductList a) b

public export
unionHom1 : Hom1 -> Hom1 -> Hom1
unionHom1 a b = toList $ unionHom1' a b

public export
id_hom1 : Hom1
id_hom1 = []

public export
invHom1 : Hom1 -> Hom1
invHom1 [] = []
invHom1 ((k,v)::xs) = [(k, negate v)] ++ (invHom1 xs)


public export
addHom1 : Hom1 -> Hom1 -> Hom1
addHom1 x y = x ++ y

public export
evalHom1 : Hom1 -> Hom1
evalHom1 = evalProductList

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

