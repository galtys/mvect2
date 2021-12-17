module Category.Transaction.Types2

import Category.Transaction.Types
import Category.Transaction.Qty
import Data.SortedMap
--import Control.Monad.State
import Crypto.Hash.SHA256
import Data.Ratio
import Generics.Derive
import JSON

%language ElabReflection

public export
data ProdKey = PKUser String | PK32 Bits32 | PKTax String
%runElab derive "ProdKey" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]

public export
data BoM32 : Type where  
  --Node32 : (qty:TQty) -> (sku:Bits32) -> (bid:Bits32)->(bom_id:Maybe Bits32)->(components:List BoM32) -> BoM32   
   Node32 : (qty:EQty) -> (sku:Bits32) ->(components:List BoM32) -> BoM32   
%runElab derive "BoM32" [Generic, Meta, Show, Eq,ToJSON,FromJSON]


public export
FromString ProdKey where
   fromString s = PKUser s

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
   -- origin : Maybe FxRef --list of origins, a PO can have multiple origins
%runElab derive "FxData" [Generic, Meta, RecordToJSON,RecordFromJSON]   

public export
data JournalEvent = Fx121 (Date, Hom121) | Fx11 (Date, Hom11) --| Empty Date --FxData 
%runElab derive "JournalEvent" [Generic, Meta, ToJSON,FromJSON]

public export
data WhsEventError = Put11Error

public export
Route : Type
Route = List Location

public export
data RouteState = Progress | Completed

namespace WhsEventDo
  public export
  data WhsEvent : Type -> Type where
       NewRoute : Date -> Route -> WhsEvent RouteRef
       CloseRoute : (date:Date) -> (ref:RouteRef) -> WhsEvent ()       
       Put   : (from:Location)->(to:Location)->Ledger -> JournalEvent -> WhsEvent ()

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
       Init : Route ->  JournalEvent -> OwnerEvent RouteRef
       
       Open : (fx:FxData) -> OwnerEvent RouteRef
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


