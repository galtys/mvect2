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
Date : Type
Date = String

public export
data TreeB = Leaf String | Node TreeB String TreeB
%runElab derive "TreeB" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]

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

public export
data DirectionTag = Sale | Purchase
%runElab derive "DirectionTag" [Generic, Meta, Eq,Ord, Show,EnumToJSON,EnumFromJSON]     

public export
data Location =  Self | Control DirectionTag Address |Partner DirectionTag Address | Init
%runElab derive "Location" [Generic, Meta, Eq, Ord,Show,ToJSON,FromJSON]


public export
data Ledger = OnHand | Forecast
%runElab derive "Ledger" [Generic, Meta, Eq, Ord, Show,EnumToJSON,EnumFromJSON]

public export
data TaxCode =  INC20 | EX20 | TAXAMOUNT |ERROR
%runElab derive "TaxCode" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

public export
data LineTermMultType = UnitPrice | Discount | MultQty | TaxMul
%runElab derive "LineTermMultType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]


public export
data LineTerm : Type where
     LEHom1 : (qty:EQty) -> LineTerm
     LETaxCode : (taxcode:TaxCode) -> LineTerm -> LineTerm
     LEAdd : (l1:LineTerm) -> (l2:LineTerm) -> LineTerm
     LEMul : (u:EQty) -> (mu:LineTermMultType) -> (l:LineTerm) -> LineTerm
%runElab derive "LineTerm" [Generic, Meta, Eq, Show, ToJSON,FromJSON]     


public export
data BoM32 : Type where  
  --Node32 : (qty:TQty) -> (sku:Bits32) -> (bid:Bits32)->(bom_id:Maybe Bits32)->(components:List BoM32) -> BoM32   
   Node32 : (qty:TQty) -> (sku:Bits32) ->(components:List BoM32) -> BoM32   
%runElab derive "BoM32" [Generic, Meta, Show, Eq,ToJSON,FromJSON]


public export
record Price where
  constructor MkPrice
  tax : TaxCode
  price : EQty
%runElab derive "Price" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]

price_mult : Price -> Price -> Price



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
public export
Cast Price EQty where
  cast (MkPrice tax x) = x
  
public export
data ProdKey = PKUser String | PK32 Bits32 | PKTax String
%runElab derive "ProdKey" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]

public export
FromString ProdKey where
   fromString s = PKUser s

public export
record ProdKey2 where
    constructor MkProdK2
    keyfrom : ProdKey
    keyto : ProdKey
%runElab derive "ProdKey2" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
{-
public export
Product : Type
Product = (ProdKey, EQty)
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
   constructor MkH
   from:Product
   price_unit:Price
   to:Product
%runElab derive "Hom3" [Generic, Meta, RecordToJSON,RecordFromJSON]
-}


--CalcSource : Type
--CalcSource = String --sha256 of the source journal, and calc method?
{-

public export
data DocType =  WOrder | WInvoice  |  LRes  | LDel 
%runElab derive "DocType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]


public export
record Acc where
   constructor MkAcc
   fro : Location
   to : Location
%runElab derive "Acc" [Generic, Meta, Eq, Ord,Show, RecordToJSON,RecordFromJSON]


public export
record Wx where
   constructor MkWx
   w1 : Location
   w2 : Location
%runElab derive "Wx" [Generic, Meta, Eq, Ord,Show, RecordToJSON,RecordFromJSON]

public export
record Lx where
   constructor MkLx
   l1 : Location
   l2 : Location
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
-- <<<<<<< HEAD
{-
public export
data LineTermMultType = UnitPrice | Discount | MultQty | TaxMul
%runElab derive "LineTermMultType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]

public export
data LineTerm : Type where
     LEHom1 : (qty:EQty) -> LineTerm
     LETaxCode : (taxcode:TaxCode) -> LineTerm -> LineTerm
     LEAdd : (l1:LineTerm) -> (l2:LineTerm) -> LineTerm
     LEMul : (u:EQty) -> (mu:LineTermMultType) -> (l:LineTerm) -> LineTerm
%runElab derive "LineTerm" [Generic, Meta, Eq, Show, ToJSON,FromJSON]     


public export
data BoM32 : Type where  
   --Node32 : (qty:TQty) -> (sku:Bits32) -> (bid:Bits32)->(bom_id:Maybe Bits32)->(components:List BoM32) -> BoM32   
   Node32 : (qty:TQty) -> (sku:Bits32) ->(components:List BoM32) -> BoM32   
%runElab derive "BoM32" [Generic, Meta, Show, Eq,ToJSON,FromJSON]
-}
{-
public export
record ProdKey2 where
    constructor MkProdK2
    key : ProdKey
    val : Price --keyfrom : ProdKey    
%runElab derive "ProdKey2" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
-}



public export
Product : Type
Product = (ProdKey, EQty)

public export
Currency : Type
Currency = (ProdKey, Price)

public export
Hom1 : Type
Hom1 = List Product

public export
Product2 : Type
Product2 = (ProdKey, Currency)

public export
Hom2 : Type
Hom2 = List Product2 --was (Hom1->Hom1)



public export
record Hom121 where
   constructor MkH121
   dx:Hom1
   appl:Hom2
   cx : Hom1
%runElab derive "Hom121" [Generic, Meta, RecordToJSON,RecordFromJSON]

public export
record Hom11 where
   constructor MkH11
   dx:Hom1
   cx:Hom1
%runElab derive "Hom11" [Generic, Meta, RecordToJSON,RecordFromJSON]


export
fromH121 : Hom121 -> Hom11
fromH121 h121 = (MkH11 (dx h121) (cx h121))

{-
public export
record Location where
  constructor MkL
  --name : String
  directionTag: DirectionTag -- Sale | Purchase
  controlTag:   Location   -- Self | Control |Partner
  ledger:       Ledger       -- OnHand | Forecast
  --address : Address  
%runElab derive "Location" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
-}

public export     
FxRef : Type 
FxRef = String --where --order reference used in warehouse

public export     
RouteRef : Type 
RouteRef = String --where --order reference used in warehouse

public export
record FxData where
   constructor MkFx
   date:Date
   direction: DirectionTag
   delivery:Address -- Delivery
   invoice:Address -- Invoice
   h3: Hom121
   origin : Maybe FxRef --list of origins, a PO can have multiple origins
%runElab derive "FxData" [Generic, Meta, RecordToJSON,RecordFromJSON]   

public export
data JournalEvent = Fx121 (Date, Hom121) | Fx11 (Date, Hom11) --FxData 
%runElab derive "JournalEvent" [Generic, Meta, ToJSON,FromJSON]

public export
data WhsEventError = Put11Error

public export
Route : Type
Route = List Location

public export
data RouteState = New | Progress | Completed

namespace WhsEventDo
  public export
  data WhsEvent : Type -> Type where
       --New : Order FxData -> WhsEvent ()
       --Move : (date:Date)->(h:Hom121)->(from:Location)->(to:Location)->WhsEvent ()     
       --Put121 : (from:Location)->(to:Location)->DirectionTag->Ledger->Hom121 -> WhsEvent ()
       --Init : Hom11 -> WhsEvent RouteRef
       NewRoute : Date -> Route -> WhsEvent RouteRef
       Put   : (from:Location)->(to:Location)->Ledger -> JournalEvent -> WhsEvent ()
       --Put11 : (date:Date)->(from:Location)->(to:Location)->Ledger -> Hom11 -> WhsEvent ()
       --Put121 :(date:Date)->(from:Location)->(to:Location)->Ledger -> Hom121 -> WhsEvent ()       

       Log : String -> WhsEvent ()
       Show : (Show ty) => ty -> WhsEvent ()
       Pure : ty -> WhsEvent ty
       Bind : WhsEvent a -> (a -> WhsEvent b) -> WhsEvent b


  public export
  (>>=) : WhsEvent a -> (a -> WhsEvent b) -> WhsEvent b
  (>>=) = WhsEventDo.Bind

  public export
  (>>) : WhsEvent () -> WhsEvent b -> WhsEvent b
  ma >> mb = WhsEventDo.Bind ma (\ _ => mb)

namespace OwnerEventDo
  public export
  data OwnerEvent : Type -> Type where
       --New : Order FxData -> OwnerEvent ()
       --Move : (date:Date)->(h:Hom121)->(from:Location)->(to:Location)->OwnerEvent ()     
       --Put121 : (from:Location)->(to:Location)->DirectionTag->Ledger->Hom121 -> OwnerEvent ()
       --Init : Hom11 -> OwnerEvent RouteRef
       Init : Date -> Route ->  Hom121 -> OwnerEvent RouteRef
       
       --Open : (fx:FxData) -> OwnerEvent FxRef
       --Close : (fx:FxData) -> OwnerEvent ()

--       Confirm : (fx:FxData) -> WhsEvent ()

       Log : String -> OwnerEvent ()
       Show : (Show ty) => ty -> OwnerEvent ()
       Pure : ty -> OwnerEvent ty
       Bind : OwnerEvent a -> (a -> OwnerEvent b) -> OwnerEvent b


  public export
  (>>=) : OwnerEvent a -> (a -> OwnerEvent b) -> OwnerEvent b
  (>>=) = OwnerEventDo.Bind

  public export
  (>>) : OwnerEvent () -> OwnerEvent b -> OwnerEvent b
  ma >> mb = OwnerEventDo.Bind ma (\ _ => mb)


