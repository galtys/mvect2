module Category.Transaction.Types2

import Category.Transaction.Types
import Category.Transaction.Qty
import Data.SortedMap
--import Control.Monad.State
--import Crypto.Hash.SHA256
import Data.Ratio
import Generics.Derive
import JSON
import Odoo.Schema.PJBRecDef

%language ElabReflection
--import Odoo.PG.BoM
{-

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
-}
public export
data Location =  Self | In | Out | Init | Loss | Control DirectionTag BrowseResPartner.RecordModel |Partner DirectionTag BrowseResPartner.RecordModel | Transit DirectionTag BrowseResPartner.RecordModel
%runElab derive "Location" [Generic, Meta, Eq, Ord,Show,ToJSON,FromJSON]


public export     
RouteRef : Type 
RouteRef = String --where --order reference used in warehouse

public export
record FxData where
   constructor MkFx
   date:Date
   direction: DirectionTag
   delivery:BrowseResPartner.RecordModel -- Delivery
   invoice:BrowseResPartner.RecordModel  -- Invoice
   h3: Hom121
   -- origin : Maybe FxRef --list of origins, a PO can have multiple origins
%runElab derive "FxData" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
data FxEvent = Fx121 (Date, Hom121) | Fx11 (Date, Hom11)
%runElab derive "FxEvent" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]

public export
Route : Type
Route = List Location

public export
data RouteState = Progress | Completed
%runElab derive "RouteState" [Generic, Meta, Eq,Show,Ord,EnumToJSON,EnumFromJSON]

public export
data OwnerJournalEvent = MkNewRoute Route FxEvent | MkOpen FxData 
%runElab derive "OwnerJournalEvent" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]

namespace WhsEventDo
  public export
  data WhsEvent : Type -> Type where
       NewRoute : Date -> Route -> WhsEvent RouteRef
       CloseRoute : (date:Date) -> (ref:RouteRef) -> WhsEvent ()       
       Put   : (from:Location)->(to:Location)->Ledger -> FxEvent -> WhsEvent ()

       Log : OwnerJournalEvent -> WhsEvent ()
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
       Init : Route ->  FxEvent -> OwnerEvent RouteRef
       
       Open : (fx:FxData) -> OwnerEvent RouteRef
       Log : String  -> OwnerEvent ()
       Show : (Show ty) => ty -> OwnerEvent ()
       Pure : ty -> OwnerEvent ty
       Bind : OwnerEvent a -> (a -> OwnerEvent b) -> OwnerEvent b

  public export
  (>>=) : OwnerEvent a -> (a -> OwnerEvent b) -> OwnerEvent b
  (>>=) = OwnerEventDo.Bind

  public export
  (>>) : OwnerEvent () -> OwnerEvent b -> OwnerEvent b
  ma >> mb = OwnerEventDo.Bind ma (\ _ => mb)


public export
record RouteKey where
  constructor MkRK
  date : Date
  ref : RouteRef
  state : RouteState
%runElab derive "RouteKey" [Generic, Meta, Eq,Show,Ord, RecordToJSON,RecordFromJSON]  


public export
LocationMap  : Type
LocationMap = SortedMap (Location, Ledger, ProdKey) EQty
public export
RouteJournalMap  : Type
RouteJournalMap = SortedMap (Location, Location,Ledger) (List FxEvent)
{-
export
RouteMap : Type
RouteMap = SortedMap RouteKey Route --(Date,RouteRef,RouteState) Route
-}

public export
record SystemState where
   constructor MkSS
   routes : SortedMap RouteKey Route
   led_map : LocationMap
   jm   : RouteJournalMap
   journal : List OwnerJournalEvent
   
export
Show SystemState where
   show (MkSS routes led_map jm j) = "system state"

export
initState : SystemState --(RouteMap,LocationMap,RouteJournalMap)
initState = (MkSS empty empty empty [])



