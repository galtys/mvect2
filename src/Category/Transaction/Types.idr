module Category.Transaction.Types

import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import Category.Transaction.Qty
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
data LineTermMultType = UnitPrice | Discount | MultQty | TaxMul

%runElab derive "LineTermMultType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]


{-
--, and delivery cost that depend on subtotals     
public export
data MoveType = Delivery  | Return | Reservation | Payment | Refund 

%runElab derive "MoveType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]
-}


public export
data DocType =  Order | PriceList | Delivery  | Return | Reservation | Payment | Refund 

%runElab derive "DocType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]

public export
Date : Type
Date = Integer


public export
data Journal : Type where 
-- JOrder : Account -> Account ->  Journal
 JAcc :  (type:DocType) -> (date:Date) ->  (a1:Account) -> (a2:Account) ->  Journal
 JSeq : List Journal -> Journal
-- MkCalc : CalcSource -> Journal
-- JDate:  Date -> Journal -> DocType -> Journal
-- JDoc :  String -> Journal
 JRef :  H256 -> Journal
 
%runElab derive "Journal" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]


public export
ToJSON TQty where
  toJSON = genToJSON' id toLower TwoElemArray

public export
FromJSON TQty where
  fromJSON = genFromJSON' id toLower TwoElemArray

{-
public export
TProduct : Type
TProduct = T Product

public export
ToJSON TProduct where
  toJSON = genToJSON' id toLower TwoElemArray

public export
FromJSON TProduct where
  fromJSON = genFromJSON' id toLower TwoElemArray
-}

{-
public export
Hom2_f : Type
Hom2_f = (Product -> Product)  --was TProduct?

public export
Hom2_f' : Type
Hom2_f' = List Product   --(ProdKey,TQty), was TProduct?


public export
Hom2' : Type
Hom2' = (Hom1,Hom1)

public export
TermPath : Type
TermPath = List Journal
-}

{-
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
-}
public export
ProdKey : Type
ProdKey = String

public export
record Line where
  constructor MkLine
  sku : ProdKey
  qty : TQty
  --Unit of Measure
  --company pricelist is input  , "price_unit" modifies it, as a multiple
  currency : ProdKey   
  price_unit : TQty --together with discount,turn it into a function Qty->Qty
  discount : TQty   --idea, in amendments, fix price_unit and let the user change the discount   
  tax_code : TaxCode
  --reference to List Price  
                      --SubTotal ... calculated

%runElab derive "Line" [Generic, Meta, Show, Eq,RecordToJSON,RecordFromJSON]


public export
record LineExt where
  constructor MkLineExt
  sku : ProdKey
  qty : TQty
  currency : ProdKey   
  price_unit : TQty --together with discount,turn it into a function Qty->Qty
  discount : TQty   --idea, in amendments, fix price_unit and let the user change the discount   
  tax_code : List TaxCode

%runElab derive "LineExt" [Generic, Meta, Show, Eq]
--%runElab derive "LineExt" [Generic, Meta, Show, Eq,RecordToJSON,RecordFromJSON]

public export
data LineTerm : Type where
     LEHom1 : (qty:TQty) -> LineTerm
     LETaxCode : (taxcode:TaxCode) -> LineTerm -> LineTerm
     LEAdd : (l1:LineTerm) -> (l2:LineTerm) -> LineTerm
     LEMul : (u:TQty) -> (mu:LineTermMultType) -> (l:LineTerm) -> LineTerm

%runElab derive "LineTerm" [Generic, Meta, Eq, Show, ToJSON,FromJSON]     

public export
record ProdKey2 where
    constructor MkProdK2
    keyfrom : ProdKey
    keyto : ProdKey

%runElab derive "ProdKey2" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]

public export
Product : Type
Product = (ProdKey, TQty)
    
public export
Product2 : Type
Product2 = (ProdKey2, LineTerm)

public export
Hom1 : Type
Hom1 = List Product

public export
Hom2 : Type
Hom2 = List Product2 --was (Hom1->Hom1)

public export
record STD where
  constructor MkSTD
  sub : Hom2
  tax : Hom2
  dline : Hom2

%runElab derive "STD" [Generic, Meta, Eq, Show, RecordToJSON,RecordFromJSON]

public export
data OrderEvent : Type where
     WHom2 : (date:Date) -> (std:STD) -> OrderEvent
     --?WDLine : (delivery:Hom2) -> (sub:OrderEvent) -> OrderEvent     
--     WSub : (sub:OrderEvent) -> OrderEvent
         
     LHom1 : (date:Date) -> (mv:DocType) -> (h1:Hom1) -> OrderEvent
--     LCo : OrderEvent -> OrderEvent -> OrderEvent
--     LPro :OrderEvent -> OrderEvent -> OrderEvent
     
--     Add : (own:OrderEvent) -> (loc:OrderEvent) -> OrderEvent
          
--     WDeliveryLine : (delivery:LineTerm) -> (subtotal:OrderEvent) -> OrderEvent  --delivery line is calculated, merging is not needed
--     WSub : Journal -> OrderEvent -> OrderEvent
{-          
     WDeliveryLine : Journal -> LineTerm -> OrderEvent -> OrderEvent
     WTax : Journal -> OrderEvent -> OrderEvent
     LMove : Journal -> OrderEvent -> OrderEvent
     LCo : Journal -> OrderEvent -> OrderEvent -> OrderEvent
     LPro : Journal -> OrderEvent -> OrderEvent -> OrderEvent
     Adj : OrderEvent -> OrderEvent -> OrderEvent
-}

%runElab derive "OrderEvent" [Generic, Meta, Eq, Show, ToJSON,FromJSON]     

public export
record OrderState where
   constructor MkOrderState
   events : List OrderEvent
   totals : List STD
   invoiced : List STD
   backorder : Hom1
   due : Hom1
   
   
%runElab derive "OrderState" [Generic, Meta, Eq, Show, RecordToJSON,RecordFromJSON]   

public export
JournalOrderState : Type
JournalOrderState = (Journal, OrderState)
