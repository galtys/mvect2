module Category.Transaction.Hom

import Generics.Derive
import Data.SortedMap
import Control.Monad.State

%language ElabReflection

public export
record Location where
  constructor MkL
  name : String
%runElab derive "Location" [Generic, Meta, Eq, Ord, Show]

data Country = UK | CZ | US | DE | FR
%runElab derive "Country" [Generic, Meta, Eq, Ord, Show]

public export
record Address where
  constructor MkA
  street : String
  street2 : String
  city : String
  zip : String
  country_id : Country
  
%runElab derive "Address" [Generic, Meta, Eq, Ord, Show]

public export
record Contact where
  constructor MkC
  name : String

%runElab derive "Contact" [Generic, Meta, Eq, Ord,Show]

public export
data Account = L Location | A Address | C Contact

public export
Qty : Type
Qty = Integer

public export
q1 : Qty
q1 = 1

public export
q7 : Qty
q7 = 7

public export
data T a = Debit a | Credit a

public export
TQty : Type
TQty = T Qty

%runElab derive "T" [Generic, Meta, Eq, Show]

public export
ProdKey : Type
ProdKey = String

public export
Product : Type
Product = (ProdKey, Qty)

public export
TProduct : Type
TProduct = T Product

public export
Hom1 : Type
Hom1 = List TProduct





add : (Qty,Qty) -> (Qty,Qty) -> (Qty,Qty)
add (a,b) (c,d) = (a+c, b+d)


merge_item_into : (SortedMap ProdKey Qty) -> (ProdKey, Qty) -> (SortedMap ProdKey Qty)
merge_item_into acc x = mergeWith (+) acc (fromList [x])


fromProductList : List Product -> SortedMap ProdKey Qty
fromProductList xs = foldl merge_item_into empty xs

evalProductList : List Product -> List Product
evalProductList xs = toList $ fromProductList xs 


public export
add_tproducts : TProduct -> TProduct -> Either (TProduct,TProduct) TProduct 
add_tproducts x y = ?u

public export
getKey : TProduct -> ProdKey
getKey (Debit (k,v)) = k
getKey (Credit (k,v)) = k

public export
getVal : TProduct -> TQty
getVal (Debit (k,v)) = Debit v
getVal (Credit (k,v)) = Credit v


addTQty : TQty -> TQty -> TQty 
addTQty (Debit a) (Debit b) = Debit (a+b)
addTQty (Credit a) (Credit b) = Credit (a+b)
addTQty (Debit a) (Credit b) = if (a<b) then Credit (b-a)
                                  else if (a>b) then Debit (a-b) 
                                       else Debit 0
addTQty (Credit a) (Debit b) = if (a<b) then Debit (b-a)
                                  else if (a>b) then Credit (a-b) 
                                       else Debit 0


--diffNat : Nat -> Nat -> Nat
--diffNat a b = (cast 

partial
public export
merge_item_into2 : (SortedMap ProdKey TProduct) -> TProduct -> (SortedMap ProdKey TProduct)
merge_item_into2 acc x = case (lookup (getKey x) acc) of
                             Nothing => (insert (getKey x) x acc)
                             Just v => case (addTQty (getVal v) (getVal x) ) of 
                                           Debit 0 => (delete (getKey x) acc)
                                           Debit nv => (insert (getKey x) (Debit (getKey x,nv) )  acc)
                                           Credit nv => (insert (getKey x) (Credit (getKey x,nv) )  acc)

public export                             
fromTProductList : List TProduct -> SortedMap ProdKey TProduct
fromTProductList xs = foldl merge_item_into2 empty xs

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
th1 : Hom1
th1 = [ Debit ("a1",4), Debit ("a2",3), Debit ("a1", 9) ]

public export
th2 : Hom1
th2 = [ Debit ("a1",4), Debit ("a2",3), Credit ("a1", 9) ]

public export
th3 : Hom1
th3 = diffHom1 th1 th2


