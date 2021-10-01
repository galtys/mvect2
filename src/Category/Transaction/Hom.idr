module Category.Transaction.Hom

import Generics.Derive
import Data.SortedMap
import Control.Monad.State

import JSON

%language ElabReflection

public export
record Location where
  constructor MkL
  name : String
%runElab derive "Location" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]

public export
data Country = UK | CZ | US | DE | FR
%runElab derive "Country" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

public export
record Address where
  constructor MkA
  street : String
  street2 : String
  city : String
  zip : String
  country_id : Country
  
%runElab derive "Address" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]

public export
record Contact where
  constructor MkC
  name : String

%runElab derive "Contact" [Generic, Meta, Eq, Ord,Show, RecordToJSON,RecordFromJSON]

public export
data Account = L Location | A Address | C Contact

%runElab derive "Account" [Generic, Meta, Eq, Ord,Show]

public export
ToJSON Account where
  toJSON = genToJSON' id toLower TwoElemArray

public export
FromJSON Account where

  fromJSON = genFromJSON' id toLower TwoElemArray

public export
pjb : Account
pjb = C (MkC "P.J.Bridgman")

public export
hilton : Account
hilton = C (MkC "Hilton")

public export
pjb_loc : Account
pjb_loc = L (MkL "Enfield")

public export
pjb_r : Account
pjb_r = L (MkL "Reservation")

public export
hilton_loc : Account
hilton_loc = L (MkL "Bristol")

data Journal = MkDate Integer | MkDoc Integer | NoJn | Acc Account Account

%runElab derive "Journal" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]




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
ToJSON TQty where
  toJSON = genToJSON' id toLower TwoElemArray

public export
FromJSON TQty where
  fromJSON = genFromJSON' id toLower TwoElemArray

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
ToJSON TProduct where
  toJSON = genToJSON' id toLower TwoElemArray

public export
FromJSON TProduct where
  fromJSON = genFromJSON' id toLower TwoElemArray

public export
Hom1 : Type
Hom1 = List TProduct

{-
public export
ToJSON Hom1 where
  toJSON = genToJSON --' id toLower TwoElemArray
public export
FromJSON Hom1 where
  fromJSON = genFromJSON --' id toLower TwoElemArray
-}


public export
data Term : Type where

     Ch : Journal -> Account -> Account -> Hom1 -> Term 
     --Jn : Journal -> Term -> Term
     Lst : List Term -> Term
     Pro : Journal -> Term -> Term -> Term
     Co : Journal -> Term -> Term -> Term
     --Adj : Journal -> Term -> Term -> Term

--myToJSON : Encoder v => Meta a code => POP ToJSON code => a -> v
--myToJSON = genToJSON' (take 3) toLower (TaggedObject "t" "c")

--myFromJSON : Encoder v => Meta a code => POP FromJSON code => a -> v
--myFromJSON = genFromJSON' (take 3) toLower (TaggedObject "t" "c")

--MyToJSON : DeriveUtil -> InterfaceImpl
--MyToJSON = customToJSON `(myToJSON)

--%runElab derive "Term" [Generic,Meta,Eq]
%runElab derive "Term" [Generic, Meta, Eq, Show, ToJSON,FromJSON]

TermPath : Type
TermPath = List Journal

public export
get_path : Term -> TermPath
get_path (Ch j1 a1 a2 h1) = [j1,(Acc a1 a2)]
get_path (Lst xs) = []
get_path (Pro j1 t1 t2) = [j1] ++ (get_path t1) ++ (get_path t2)
get_path (Co j1 t1 t2) = [j1] ++ (get_path t1) ++ (get_path t2)
--get_path (Co j1 t1 t2) = [j1] ++ (get_path t1) ++ (get_path t2)

--partial
--evTerm : Term  -> Term
--evTerm (Co (Ch j1 a1 a2 h1)  (Ch j2 b1 b2 h2)  ) = ?xss

public export
th11 : Hom1
th11 = [ Debit ("a1",4), Debit ("a2",3), Debit ("a1", 9) ]

public export
th11' : Hom1
th11' = [ Debit ("a1",3), Debit ("a1", 6) ]


public export
th12 : Hom1
th12 = [ Debit ("GBP",38) ]


public export
t1_r : Term
t1_r = Ch NoJn pjb_loc pjb_r th11 --reservation

public export
t1_d : Term
t1_d = Ch NoJn pjb_r hilton_loc th11' --delivery

public export
t1 : Term
t1 = Co NoJn t1_r t1_d 

public export
encode_x : String
encode_x = encode t1

public export
term_x : Either JSONErr Term
term_x = decode encode_x

add : (Qty,Qty) -> (Qty,Qty) -> (Qty,Qty)
add (a,b) (c,d) = (a+c, b+d)


merge_item_into : (SortedMap ProdKey Qty) -> (ProdKey, Qty) -> (SortedMap ProdKey Qty)
merge_item_into acc x = mergeWith (+) acc (fromList [x])

fromProductList : List Product -> SortedMap ProdKey Qty
fromProductList xs = foldl merge_item_into empty xs

evalProductList : List Product -> List Product
evalProductList xs = toList $ fromProductList xs 


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
                             Just v => case (addTQty (getVal v) (getVal x) ) of 
                                           Debit 0 => (delete (getKey x) acc)
                                           Debit nv => (insert (getKey x) (Debit (getKey x,nv) )  acc)
                                           Credit nv => (insert (getKey x) (Credit (getKey x,nv) )  acc)
partial
public export
merge_as_union : (SortedMap ProdKey TProduct) -> TProduct -> (SortedMap ProdKey TProduct)
merge_as_union acc x = case (lookup (getKey x) acc) of
                             Nothing => acc
                             Just v => case (unTQty (getVal v) (getVal x) ) of 
                                           Debit 0 => (delete (getKey x) acc)
                                           Debit nv => (insert (getKey x) (Debit (getKey x,nv) )  acc)
                                           Credit nv => (insert (getKey x) (Credit (getKey x,nv) )  acc)

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

-}

eq_accounts: Term -> Term -> Bool
eq_accounts (Ch j1 a1 a2 h1) (Ch j2 b1 b2 h2) = ( (a1==b1) && (a2==b2) && (j1==j2) )
eq_accounts _ _ = False


partial
monoTerm : Term -> Term -> Term
--monoTerm t1@(Ch j1 a1 a2 h1) t2@(Ch j2 b1 b2 h2) = if (eq_accounts t1 t2) then (Ch j1 a1 a2 (unionHom1 h1 h2 ) ) else Lst [t1,t2]
--monoTerm (Co t11 t12) (Co t21 t22) = ?monoid_for_composition

monoTerm (Lst xs) (Lst []) = Lst xs
monoTerm (Lst []) (Lst xs) = Lst xs
monoTerm (Lst xs) (Lst ys) = Lst (xs++ys)
monoTerm (Lst xs) t = Lst (xs ++ [t])
monoTerm t (Lst xs) = Lst ([t] ++ xs)

monoTerm t1 t2 = Lst ([t1] ++ [t2]) 

--monoTerm t1@(Ch j1 a1 a2 h1) t2 = Lst [t1,t2]
--monoTerm t1 t2@(Ch j1 a1 a2 h1) t2 = Lst [t1,t2]

--monoTerm (Ch a1 a2 h1) ID = Ch a1 a2 h1

