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
Qty : Type
Qty = Nat

public export
q1 : Nat
q1 = 1

public export
q7 : Nat
q7 = 7

{-
public export
record Product where
  constructor MkP
  name : String
  qty : Qty
%runElab derive "Product" [Generic, Meta, Eq, Ord, Show]
-}

ProdKey : Type
ProdKey = String

Product : Type
Product = (ProdKey, Qty)

add : (Qty,Qty) -> (Qty,Qty) -> (Qty,Qty)
add (a,b) (c,d) = (a+c, b+d)


merge_item_into : (SortedMap ProdKey Qty) -> (ProdKey, Qty) -> (SortedMap ProdKey Qty)
merge_item_into acc x = mergeWith (+) acc (fromList [x])


fromProductList : List Product -> SortedMap ProdKey Qty
fromProductList xs = foldl merge_item_into empty xs

evalProductList : List Product -> List Product
evalProductList xs = toList $ fromProductList xs 

public export
data T a = Debit a | Credit a

%runElab derive "T" [Generic, Meta, Eq, Show]

public export
TProduct : Type
TProduct = T Product

public export
add_tproducts : TProduct -> TProduct -> Either (TProduct,TProduct) TProduct 
add_tproducts x y = ?u

public export
getKey : TProduct -> ProdKey
getKey (Debit (k,v)) = k
getKey (Credit (k,v)) = k

public export
merge_item_into2 : (SortedMap ProdKey TProduct) -> TProduct -> (SortedMap ProdKey TProduct)
merge_item_into2 acc x = case (lookup (getKey x) acc) of
                             Nothing => (insert (getKey x) x acc)
                             Just v => case (x,v) of 
                                           (Debit a,Debit b) => (insert (getKey x)  (Debit (getKey x, (snd a) + (snd b)) )  acc)
                                           (Credit a, Debit b) => if (snd a == snd b) then (delete (getKey x) acc) else (insert (getKey x) x acc)
                                           (Debit a, Credit b) => if (snd a == snd b) then (delete (getKey x) acc) else (insert (getKey x) x acc)
                                           (Credit a, Credit b) => (insert (getKey x) (Credit (getKey x, (snd a) + (snd b)) ) acc)

public export                             
fromTProductList : List TProduct -> SortedMap ProdKey TProduct
fromTProductList xs = foldl merge_item_into2 empty xs

public export
evalTProductList : List TProduct -> List TProduct
evalTProductList xs = toList $ fromTProductList xs 
                                                                                       
public export
Hom1 : Type
Hom1 = List TProduct

public export
Hom1Pro : Type
Hom1Pro = List (Product,Product)

public export
id_hom1 : Hom1
id_hom1 = []

public export
invHom1 : Hom1 -> Hom1
invHom1 [] = []
invHom1 (Debit x::xs) = [(Credit x)] ++ xs
invHom1 (Credit x::xs) = [(Debit x)] ++ xs

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
evalHom1 xs = [Debit x | x <- (evalProductList (debitsHom1 xs))] ++ [Credit x | x <- (evalProductList (creditsHom1 xs))] 

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
