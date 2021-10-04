module Category.Transaction.Types


import Generics.Derive
import Data.SortedMap
import Control.Monad.State

--import Category.Transaction.Qty
import Crypto.Hash.SHA256
import Data.Ratio

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

%runElab derive "TaxCode" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]


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
data DocType = SaleOrder | PurchaseOrder | Delivery | Return |  Reservation | Internal | Payment | Refund | Other |PriceList

%runElab derive "DocType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]

public export
data Journal : Type where 
 JOrder : Account -> Account -> Account -> Account -> Journal
 JAcc :    Account -> Account -> Journal
-- MkCalc : CalcSource -> Journal
 JDate:  Integer -> Journal -> DocType -> Journal
 JDoc :  String -> Journal
 JRef :  H256 -> Journal
 
%runElab derive "Journal" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]

public export
data T a = Debit a | Credit a

public export
TQty : Type
TQty = T QtyRatio

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

--%runElab derive "ProdKey" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]

public export
Product : Type
Product = (ProdKey, QtyRatio)

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
Hom2_f' = List Product   --(ProdKey,QtyRatio)

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

%runElab derive "Term" [Generic, Meta, Eq, Show, ToJSON,FromJSON]

public export
record Line where
  constructor MkLine
  sku : ProdKey
  qty : QtyRatio
  --Unit of Measure
  --company pricelist is input  , "price_unit" modifies it, as a multiple
  currency : ProdKey   
  tax_code : TaxCode
  --reference to List Price  
  price_unit : QtyRatio --together with discount,turn it into a function Qty->Qty
  discount : QtyRatio   --idea, in amendments, fix price_unit and let the user change the discount 
  --SubTotal ... calculated

%runElab derive "Line" [Generic, Meta, Show, Eq,RecordToJSON,RecordFromJSON]


public export
data LineTerm : Type where
     LHom1 : (qty:TProduct) -> LineTerm
     --LRef : Journal -> LineTerm --DocType Pricelist
     --LPList : Hom2_f' -> LineTerm -> LineTerm --use Hom2' instead of Hom2, elab will work better?, 
     LHom2 : (price_unit:TProduct) -> LineTerm -> LineTerm -- (currency and unit price)
     LDiscount :  (discount:QtyRatio) -> LineTerm -> LineTerm     
     
     --LCh : Hom1 -> LineTerm -> LineTerm --To be able to express dependence on base pricelist, user can alter price this way
     
     --LTax : LineTerm -> LineTerm -- calc tax based on order lines   
     
--With Ref erence at the bottom of the recursive structure, each LineTerm can be referenced by that journal, used for pricelist
     
%runElab derive "LineTerm" [Generic, Meta, Eq, Show, ToJSON,FromJSON]     
     
--, and delivery cost that depend on subtotals     
public export
data OrderTerm : Type where
     ChO : Journal -> (List LineTerm) -> OrderTerm
     Sub : Journal -> OrderTerm -> OrderTerm
--     DeliveryLine : Journal -> LineTerm -> OrderTerm -> OrderTerm 
-- delivery line pricelist can depend on subtotals, which is in OrderTerm 
-- in this case, delivery line is just an DeliveryOption selector
     Tax : Journal -> OrderTerm -> OrderTerm

%runElab derive "OrderTerm" [Generic, Meta, Eq, Show, ToJSON,FromJSON]     
