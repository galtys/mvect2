module Category.Transaction.Types2

import Category.Transaction.Types
import Category.Transaction.RouteTypes
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

public export
data OwnerJournalEvent : Type where
     MkUserUpdate : UserData -> OwnerJournalEvent
     MkNewRoute : RouteSumT -> FxEvent -> OwnerJournalEvent
     MkOpen : FxData -> OwnerJournalEvent
     MkClose : RouteKey -> OwnerJournalEvent
     MkError : String -> OwnerJournalEvent
     MkAEntry : AllocationEntry -> OwnerJournalEvent
     MkPost : RouteKey -> MoveKey ->  FxEvent -> OwnerJournalEvent     
%runElab derive "OwnerJournalEvent" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]

namespace OwnerEventDo
  public export
  data OwnerEvent : Type -> Type where
       Init : RouteSumT ->  FxEvent -> UserData -> OwnerEvent RouteKey --just sugar post event
       UpdateUserData : UserData -> OwnerEvent ()       
       GetUserData : OwnerEvent UserDataMap
       
       Open : (fx:FxData) -> OwnerEvent RouteKey
       GetFxData : (key:RouteKey) -> OwnerEvent (Maybe FxData)       
       GetRoute : (key:RouteKey) -> OwnerEvent (Maybe RouteSumT)
       Post : RouteKey -> MoveKey -> FxEvent -> OwnerEvent ()  --post to rote  
            
       Get : MoveKey -> OwnerEvent Hom11
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


namespace WhsEventDo
  public export
  record WhsEntry where
     constructor MkWE
     ref : Ref   
     fx : FxEvent
  %runElab derive "WhsEntry" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

  public export
  data WhsEvent : Type -> Type where
       NewRoute : FxData -> RouteSumT -> WhsEvent RouteKey
       UpdateUserData : UserData -> WhsEvent ()
       GetUserDataW : WhsEvent UserDataMap
       
       CloseRoute : (ref:RouteKey) -> WhsEvent () 
       GetFxData : (ref:RouteKey) -> WhsEvent (Maybe FxData) 
       GetRoute : (ref:RouteKey) -> WhsEvent (Maybe RouteSumT)

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


namespace SystemState
   public export
   LocationMap  : Type
   LocationMap = SortedMap (Location, Ledger, ProdKey) EQty
   public export
   RouteJournalMap  : Type
   RouteJournalMap = SortedMap MoveKey (List WhsEntry) --MoveKey   -- (Location, Location,Ledger)

   public export
   record SystemState where
      constructor MkSS
      fx_map : SortedMap RouteKey FxData
      routes : SortedMap RouteKey RouteSumT
      led_map : LocationMap
      jm   : RouteJournalMap

      journal : List OwnerJournalEvent
      user_data : UserDataMap

   export
   Show SystemState where
      show (MkSS fx_map routes led_map jm j user_data) = "system state"

