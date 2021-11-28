module Category.Transaction.Types

import Category.Transaction.Qty
import Data.SortedMap
--import Control.Monad.State
import Crypto.Hash.SHA256
import Data.Ratio
import Generics.Derive
import JSON

%language ElabReflection

public export
data Country = UK | CZ | US | DE | FR
%runElab derive "Country" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

public export
record Contact where
  constructor MkC
  name : String
%runElab derive "Contact" [Generic, Meta, Eq, Ord,Show, RecordToJSON,RecordFromJSON]

public export
record Address where
  constructor MkA
  street : String
  street2 : String
  city : String
  zip : String
  country_id : Country
  contact: Contact
%runElab derive "Address" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]


data ControlTag =  Control | Partner
%runElab derive "ControlTag" [Generic, Meta, Eq, Show,EnumToJSON,EnumFromJSON]

data DirectionTag = Sale | Purchase
%runElab derive "DirectionTag" [Generic, Meta, Eq, Show,EnumToJSON,EnumFromJSON]     

public export
data OrderDirection a = MkOD DirectionTag a
%runElab derive "OrderDirection" [Generic, Meta, Eq, Show,ToJSON,FromJSON]     

public export
data Ledger = OnHand | Forecast
%runElab derive "Ledger" [Generic, Meta, Eq, Ord, Show,EnumToJSON,EnumFromJSON]

||| Locatin structure
-- For Self: OnHand | Forecast

-- For Control Sales, Purchasae

-- For Destination sales, Source Purchase


public export
record Location where
  constructor MkL
  name : String
  directionTag: DirectionTag
  ledger: Ledger
  address : Address
    
%runElab derive "Location" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
-- Location (OnHand | Forecast) (


public export
data TaxCode = ZeroVAT | INC20 | EX20 | TAXAMOUNT
%runElab derive "TaxCode" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

public export
record Price where
  constructor MkPrice
  tax : TaxCode
  price : EQty
%runElab derive "Price" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]

export  
toINC20 : Double -> Price
toINC20 x = MkPrice INC20 (cast x) 

export
fromPrice : Price -> Double
fromPrice (MkPrice tax x) = (cast x)

export
toEX20 : Double -> Price
toEX20 x = MkPrice EX20 (cast x) 

export
toTaxA : Double -> Price
toTaxA x = MkPrice TAXAMOUNT (cast x) 


public export
Cast Price Double where
  cast = fromPrice

  


{-
public export
data Account = L Location | A Address | C Contact
%runElab derive "Account" [Generic, Meta, Eq, Ord,Show]

public export
ToJSON Account where
  toJSON = genToJSON' id toLower TwoElemArray

public export
FromJSON Account where
  fromJSON = genFromJSON' id toLower TwoElemArray
-}

CalcSource : Type
CalcSource = String --sha256 of the source journal, and calc method?

public export
data LineTermMultType = UnitPrice | Discount | MultQty | TaxMul
%runElab derive "LineTermMultType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]


public export
--data DocType =  Order | PriceList | Delivery  | Return | Reservation | Payment | Refund 
data DocType =  WOrder | WInvoice  |  LRes  | LDel --oWnerOrder | oWnerInvoice | LocationReservation | LocationDelivery
%runElab derive "DocType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]

public export
Date : Type
Date = String

{-
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

data Wtype = EnumOrder | EnumInvoice

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
-}

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
Hom1 : Type
Hom1 = List Product

public export
Product2 : Type
Product2 = (ProdKey2, LineTerm)

public export
Hom2 : Type
Hom2 = List Product2 --was (Hom1->Hom1)

public export
record Hom3 where
   constructor MkHom3
   from:Product
   price_unit:Price
   to:Product

public export
record FxData where
   constructor MkFxData
   reqested:Date
   l:Location
   h3:Hom3
   
     
data FxRef : Type where --order reference used in warehouse

public export
data OrderEvent : Type -> Type where
     Init : FxData -> OrderEvent FxRef --asset spring into existence, does not verify
     MoveTo : (p:Product)->(dst:Location)-> OrderEvent ()
     MoveFrom : (p:Product)->(src:Location)-> OrderEvent ()
     Move : (p:Product)->(from:Location)->(to:Location) -> OrderEvent ()
     
       
     Confirm : OrderDirection FxData -> OrderEvent ()
     Invoice : FxData -> OrderEvent (OrderDirection FxData)
     
     Log : String -> OrderEvent ()
     Show : (Show ty) => ty -> OrderEvent ()
     --Invoice : FxRef -> OrderEvent FxData --query whs what can be invoiced
     --Deliver :
     --Pay : wq1 
     Pure : ty -> OrderEvent ty
     Bind : OrderEvent a -> (a -> OrderEvent b) -> OrderEvent b

namespace OrderEventDo
  public export
  (>>=) : OrderEvent a -> (a -> OrderEvent b) -> OrderEvent b
  (>>=) = Bind

  public export
  (>>) : OrderEvent () -> OrderEvent b -> OrderEvent b
  ma >> mb = Bind ma (\ _ => mb)



{-
public export
record STD where
  constructor MkSTD
  sub : Hom2
  tax : Hom2
  dline : Hom2

%runElab derive "STD" [Generic, Meta, Eq, Show, RecordToJSON,RecordFromJSON]
-}
{-
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
   
  -} 
   

{-
data RunIO : Type -> Type where
     Quit : a -> RunIO a
     Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b
     Seq : IO () -> Inf (RunIO b) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

(>>) : IO () -> Inf (RunIO b) -> RunIO b
(>>) = Seq

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run (More fuel) (Seq io k) = do io; run fuel k
run Dry p = pure Nothing


partial
forever : Fuel
forever = More forever

greet : RunIO ()
greet = do putStr "Enter your name: "
           name <- getLine
           if name == ""
              then do putStrLn "Bye bye!"
                      Quit ()
              else do putStrLn ("Hello " ++ name)
                      greet

inf_loop2 : (Ptr MG_MGR) -> Int -> RunIO ()
inf_loop2 p_mgr time_out = do
  mg_mgr_poll p_mgr time_out
  inf_loop2 p_mgr time_out

-}

