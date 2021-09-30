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

{-
public export
record Product where
  constructor MkP
  name : String
  qty : Qty
%runElab derive "Product" [Generic, Meta, Eq, Ord, Show]
-}

Product : Type
Product = (String, Qty)

add : Qty -> Qty -> Qty
add a b = a+b

merge_item_into : (SortedMap String Qty) -> (String, Qty) -> (SortedMap String Qty)
merge_item_into acc x = mergeWith add acc (fromList [x])


fromProductList : List Product -> SortedMap String Qty
fromProductList xs = foldl merge_item_into empty xs

evalProductList : List Product -> List Product --(SortedMap String Qty)
evalProductList xs = toList $ fromProductList xs 


eqProductList : List Product -> List Product -> Bool



{-
evalListProduct : List (String,Qty) -> State ( SortedMap String Qty ) Qty
evalListProduct (x::xs) = do
        sm <- get 
        let sm_x = fromList [x]
        put (mergeWith add sm sm_x)
public export
add_products : Product -> Product -> Either (Product,Product) Product 
add_products (n1, q1) (n2, q2) = if (n1==n2) then Right ( n1, (q1+q2))
                                                   else Left ( (n1, q1),  (n2, q2) )
-}



public export
data T a = Debit a | Credit a

%runElab derive "T" [Generic, Meta, Eq, Show]





public export
TProduct : Type
TProduct = T Product




public export
Hom1 : Type
Hom1 = List TProduct

--public export
--data Hom1 = MkH1 (List TProduct)

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
