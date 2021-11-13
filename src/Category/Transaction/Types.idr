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
data EQtyType = EValue | EPercent
%runElab derive "EQtyType" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

public export
data EQty : Type where
     EQVal : (et:EQtyType) -> (x:TQty) -> EQty
     EQAdd : (x:EQty) -> (y:EQty) -> EQty
     EQMul : (x:EQty) -> (y:EQty) -> EQty     
     EQNegate : (x:EQty) -> EQty
     EQSub : (x:EQty) -> (y:EQty) -> EQty
     EQDiv : (x:EQty) -> (y:EQty) -> EQty
     EQRecip : (x:EQty) -> EQty

public export
eval : EQty -> TQty
eval (EQVal EValue x) = x
eval (EQVal EPercent x) = (100-x)/100
eval (EQAdd x y) = (eval x) + (eval y)
eval (EQMul x y) = (eval x) * (eval y)
eval (EQNegate x) = negate (eval x)
eval (EQSub x y) = (eval x)-(eval y)
eval (EQDiv x y) = (eval x)/(eval y)
eval (EQRecip x) = recip (eval x)

public export
Cast Double EQty where
  cast = cast

public export
Cast EQty Double where
  cast = cast 

public export
Num EQty where
     (+) = EQAdd
     (*) = EQMul
     fromInteger x = (EQVal EValue (fromInteger x))

public export
Neg EQty where
     (-) = EQSub
     negate = EQNegate

public export     
Fractional EQty where
     (/) = EQDiv
     recip = EQRecip
               
public export
Eq EQty where
     (==) x y = ( (eval x) == (eval y) )

public export
Ord EQty where
     compare x y = compare (eval x) (eval y)

public export
Show EQty where
     show = show . eval
               
%runElab derive "EQty" [Generic, Meta, Show, ToJSON,FromJSON]     


public export
record Location where
  constructor MkL
  name : String
%runElab derive "Location" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]

public export
data Country = UK | CZ | US | DE | FR
%runElab derive "Country" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

public export
data TaxCode = ZeroVAT | INC20 | EX20 | TAXAMOUNT

%runElab derive "TaxCode" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

public export
record Price where
  constructor MkPrice
  price : EQty
  tax : TaxCode
%runElab derive "Price" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]

export  
toINC20 : Double -> Price
toINC20 x = MkPrice (cast x) INC20

export
fromPrice : Price -> Double
fromPrice (MkPrice x tax) = (cast x)

export
toEX20 : Double -> Price
toEX20 x = MkPrice (cast x) EX20

export
toTaxA : Double -> Price
toTaxA x = MkPrice (cast x) TAXAMOUNT


public export
Cast Price Double where
  cast = fromPrice

  
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
--data DocType =  Order | PriceList | Delivery  | Return | Reservation | Payment | Refund 
data DocType =  WOrder | WInvoice  |  LRes  | LDel --oWnerOrder | oWnerInvoice | LocationReservation | LocationDelivery


%runElab derive "DocType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]

public export
Date : Type
Date = String

public export
record Acc where
   constructor MkAcc
   fro : Account
   to : Account
%runElab derive "Acc" [Generic, Meta, Eq, Ord,Show, RecordToJSON,RecordFromJSON]


public export
record Wx where
   constructor MkWx
   w1 : Account
   w2 : Account
%runElab derive "Wx" [Generic, Meta, Eq, Ord,Show, RecordToJSON,RecordFromJSON]

public export
record Lx where
   constructor MkLx
   l1 : Account
   l2 : Account
%runElab derive "Lx" [Generic, Meta, Eq, Ord,Show, RecordToJSON,RecordFromJSON]

data Wtype = Order | Invoice

%runElab derive "Wtype" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]

data Ltype = Reservation | Delivery
%runElab derive "Ltype" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]

public export
data Journal : Type where 
-- JOrder : Account -> Account ->  Journal
 Jstart : Journal
 --JAcc :  (a1:Account) -> (a2:Account) -> Journal --make it separate type
 JPro : (type:DocType) -> (date:Date) -> (origin:Journal) -> Journal
 JWx : Wtype -> (date:Date) -> (from:Wx) -> (to:Wx) -> (origin:Journal) -> Journal
 JLx : Ltype -> (date:Date) -> (from:Lx) -> (to:Lx) -> (origin:Journal) -> Journal 
 JProAcc :  (type:DocType) -> (date:Date) ->  (p1:Acc) -> (p2:Acc) -> (origin:Journal)-> Journal

-- JSeq : List Journal -> Journal
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
--public export
--ProdKey : Type
--ProdKey = String

public export
data ProdKey = PKUser String | PK32 Bits32 | PKTax String

public export
FromString ProdKey where
   fromString s = PKUser s

%runElab derive "ProdKey" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]     



public export
data BoM32 : Type where  
   --Node32 : (qty:TQty) -> (sku:Bits32) -> (bid:Bits32)->(bom_id:Maybe Bits32)->(components:List BoM32) -> BoM32   
   Node32 : (qty:TQty) -> (sku:Bits32) ->(components:List BoM32) -> BoM32   
%runElab derive "BoM32" [Generic, Meta, Show, Eq,ToJSON,FromJSON]

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
