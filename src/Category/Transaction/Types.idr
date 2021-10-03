module Category.Transaction.Types

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
data TaxCode = ZeroVAT | INC20 | EX20

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

CalcSource : Type
CalcSource = String --sha256 of the source journal, and calc method?

public export
data Journal = MkDate Integer | MkDoc String | Acc Account Account | MkCalc CalcSource

%runElab derive "Journal" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]

public export
Qty : Type
Qty = Integer

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

public export
Hom2_f : Type
Hom2_f = (TProduct -> TProduct)

public export
Hom2 : Type
Hom2 = (Hom1 -> Hom1)

public export
record Line where
  constructor MkLine
  sku : ProdKey
  qty : Qty
  --Unit of Measure
  --company pricelist is input  , "price_unit" modifies it, as a multiple
  currency : ProdKey   
  tax_code : TaxCode
  --List Price  
  price_unit : Qty --together with discount,turn it into a function Qty->Qty
  discount : Qty   --idea, in amendments, fix price_unit and let the user change the discount 
  --SubTotal ... calculated

public export
TermPath : Type
TermPath = List Journal
  
public export
data Term : Type where

     Ch : Journal -> Account -> Account -> Hom1 -> Term 
     --Jn : Journal -> Term -> Term
--     Lst : List Term -> Term , instead of Lst, use something like merge_item_into2.. for Term , with get_path, get_hom1
     Pro : Journal -> Term -> Term -> Term
     Co : Journal ->  Term -> Term -> Term
     --Adj : Journal -> Term -> Term -> Term

--%runElab derive "Term" [Generic,Meta,Eq]
%runElab derive "Term" [Generic, Meta, Eq, Show, ToJSON,FromJSON]

