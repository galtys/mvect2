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
data DocType = SaleOrder | PurchaseOrder | Delivery | Return |  Reservation | InternalMovement | Payment | Refund | Other |PriceList

%runElab derive "DocType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]

public export
data Journal : Type where 
 Order : Account -> Account -> Account -> Account -> Journal
 Acc :    Account -> Account -> Journal
-- MkCalc : CalcSource -> Journal
 MkDate:  Integer -> Journal -> DocType -> Journal
 MkDoc :  String -> Journal

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
Hom2_f' : Type
Hom2_f' = List (ProdKey,Qty)

public export
Hom2 : Type
Hom2 = (Hom1 -> Hom1)

public export
Hom2' : Type
Hom2' = (Hom1,Hom1)


public export
TermPath : Type
TermPath = List Journal
  
public export
data Term : Type where
     Ref : Journal -> Term
     Ch : Term -> Hom1 -> Term 
     --Jn : Journal -> Term -> Term
--     Lst : List Term -> Term , instead of Lst, use something like merge_item_into2.. for Term , with get_path, get_hom1
     Pro : Journal -> Term -> Term -> Term
     Co : Journal ->  Term -> Term -> Term
     --Adj : Journal -> Term -> Term -> Term

--%runElab derive "Term" [Generic,Meta,Eq]
%runElab derive "Term" [Generic, Meta, Eq, Show, ToJSON,FromJSON]



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
data LineTerm : Type where
     LRef : Journal -> LineTerm --DocType Pricelist
     PList : Hom2_f' -> LineTerm -> LineTerm --use Hom2' instead of Hom2, elab will work better?, 
     
     ChL : Journal -> Line -> LineTerm -> LineTerm --To be able to express dependence on base pricelist, user can alter price this way
     LTax : Journal -> LineTerm -> LineTerm -- calc tax based on order lines
     
     
     
--With Ref erence at the bottom of the recursive structure, each LineTerm can be referenced by that journal, used for pricelist
     
--, and delivery cost that depend on subtotals     
public export
data OrderTerm : Type where
     ChO : Journal -> (List LineTerm) -> OrderTerm
     Sub : Journal -> OrderTerm -> OrderTerm
--     DeliveryLine : Journal -> LineTerm -> OrderTerm -> OrderTerm 
-- delivery line pricelist can depend on subtotals, which is in OrderTerm 
-- in this case, delivery line is just an DeliveryOption selector
     Tax : Journal -> OrderTerm -> OrderTerm
