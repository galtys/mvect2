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


public export
record UserData where
  constructor MkUD
  products : List BrowseProduct.RecordModel
  templates : List BrowseProductTemplate.RecordModel
  boms : List BrowseBoM.RecordModel
  taxes : List BrowseOrderTax.RecordModel
%runElab derive "UserData" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]  

public export
record UserDataMap where
  constructor MkUDMap
  products : SortedMap Bits32  BrowseProduct.RecordModel
  templates : SortedMap Bits32 BrowseProductTemplate.RecordModel
  boms : SortedMap ProdKey (List BrowseBoM.RecordModel)   --toBoM_map --SortedMap Bits32 BrowseBoM.RecordModel
  taxes : SortedMap Bits32 BrowseOrderTax.RecordModel




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
data Location =  Self | In BrowseResPartner.RecordModel | Out BrowseResPartner.RecordModel | Init | Loss | Control DirectionTag BrowseResPartner.RecordModel |Partner DirectionTag BrowseResPartner.RecordModel | Transit DirectionTag BrowseResPartner.RecordModel
%runElab derive "Location" [Generic, Meta, Eq, Ord,Show,ToJSON,FromJSON]



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
RouteRef : Type 
RouteRef = String --where --order reference used in warehouse

public export
AllocationRef : Type
AllocationRef = String



public export
--data FxEvent = Fx121 (Date, Hom121) | Fx11 (Date, Hom11) 
data FxEvent = Fx121 Date Hom121 | Fx11 Date Hom11 
%runElab derive "FxEvent" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]

public export
Route : Type
Route = List Location


public export
data RouteState = Progress | Completed
%runElab derive "RouteState" [Generic, Meta, Eq,Show,Ord,EnumToJSON,EnumFromJSON]
public export
record RouteKey where
  constructor MkRK
  date : Date
  ref : RouteRef
  state : RouteState
%runElab derive "RouteKey" [Generic, Meta, Eq,Show,Ord, RecordToJSON,RecordFromJSON]  

public export
data Ref = MkAllocationRef AllocationRef | MkRouteKeyRef RouteKey
%runElab derive "Ref" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]   


public export
record MoveKey where
  constructor MkMK
  from : Location
  to : Location
  ledger : Ledger
%runElab derive "MoveKey" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
record AllocationItem where
  constructor MkAI
  --key : MoveKey
  supplier : RouteKey
  customer: RouteKey
  --from : RouteKey
  --to : RouteKey
  fx : FxEvent
%runElab derive "AllocationItem" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   
{-
public export
record 
-}

public export
record AllocationEntry where
  constructor MkAE
  --date : Date
  ledger : Ledger  
  moves : List AllocationItem --(Route,Route,Ledger,FxEvent)
%runElab derive "AllocationEntry" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   


public export
data OwnerJournalEvent : Type where
     MkUserUpdate : UserData -> OwnerJournalEvent
     MkNewRoute : Route -> FxEvent -> OwnerJournalEvent
     MkOpen : FxData -> OwnerJournalEvent
     MkClose : RouteKey -> OwnerJournalEvent
     MkError : String -> OwnerJournalEvent
     MkAEntry : AllocationEntry -> OwnerJournalEvent
     MkPost : RouteKey -> MoveKey ->  FxEvent -> OwnerJournalEvent     
%runElab derive "OwnerJournalEvent" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]

namespace OwnerEventDo
  public export
  data OwnerEvent : Type -> Type where
       Init : Route ->  FxEvent -> UserData -> OwnerEvent RouteKey --just sugar post event
       UpdateUserData : UserData -> OwnerEvent ()       
       GetUserData : OwnerEvent UserDataMap
       
       Open : (fx:FxData) -> OwnerEvent RouteKey
       Post : RouteKey -> MoveKey -> FxEvent -> OwnerEvent ()  --post to rote       
       Close: (ref:RouteKey)  -> OwnerEvent ()       
       Allocate : AllocationEntry -> OwnerEvent Ref
       
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
record WhsEntry where
   constructor MkWE
   ref : Ref   
   fx : FxEvent
%runElab derive "WhsEntry" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

namespace WhsEventDo
  public export
  data WhsEvent : Type -> Type where
       NewRoute : Date -> Route -> WhsEvent RouteKey
       UpdateUserData : UserData -> WhsEvent ()
       GetUserDataW : WhsEvent UserDataMap
       
       CloseRoute : (ref:RouteKey) -> WhsEvent ()  
       GetRoute : (ref:RouteKey) -> WhsEvent (Maybe Route)

       Put   : Ref -> MoveKey -> FxEvent -> WhsEvent ()

       Get : MoveKey -> WhsEvent (List WhsEntry)
       --Get : MoveKey -> WhsEvent (List FxEnvent)
              
       Log : OwnerJournalEvent -> WhsEvent () --Log state affecting events
       Show : (Show ty) => ty -> WhsEvent ()
       Pure : ty -> WhsEvent ty
       Bind : WhsEvent a -> (a -> WhsEvent b) -> WhsEvent b

  public export
  (>>=) : WhsEvent a -> (a -> WhsEvent b) -> WhsEvent b
  (>>=) = WhsEventDo.Bind

  public export
  (>>) : WhsEvent () -> WhsEvent b -> WhsEvent b
  ma >> mb = WhsEventDo.Bind ma (\ _ => mb)


public export
LocationMap  : Type
LocationMap = SortedMap (Location, Ledger, ProdKey) EQty
public export
RouteJournalMap  : Type
RouteJournalMap = SortedMap MoveKey (List WhsEntry) --MoveKey   -- (Location, Location,Ledger)
{-
export
RouteMap : Type
RouteMap = SortedMap RouteKey Route --(Date,RouteKey,RouteState) Route
-}

public export
record SystemState where
   constructor MkSS
   routes : SortedMap RouteKey Route
   led_map : LocationMap
   jm   : RouteJournalMap
   
   journal : List OwnerJournalEvent
   user_data : UserDataMap
      
export
Show SystemState where
   show (MkSS routes led_map jm j user_data) = "system state"

