module Category.Transaction.Types2

import Category.Transaction.Types
import Category.Transaction.RouteTypes
import Category.Transaction.Qty
import Data.SortedMap
--import Control.Monad.State
import Crypto.Hash.SHA256
import Data.Ratio
import Generics.Derive
import JSON
import Odoo.Schema.PJBRecDef
import Libc.DateTypes
import Browser.WebSocket
import Browser.WS2

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
  products : SortedMap ProdKey  BrowseProduct.RecordModel
  templates : SortedMap ProdKey BrowseProductTemplate.RecordModel
  boms_m : SortedMap ProdKey (List BrowseBoM.RecordModel)  
  taxes : SortedMap Bits32 BrowseOrderTax.RecordModel


public export
data JournalLog : Type where
     MkUserUpdate : UserData -> JournalLog
     MkNewRoute : RouteSumT -> FxEvent -> JournalLog
     MkOpen : FxData -> JournalLog
     MkError : String -> JournalLog
     MkAEntry : AllocationEntry -> JournalLog
     MkPost : RouteKey -> MoveKey ->  FxEvent -> JournalLog     
%runElab derive "JournalLog" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]



namespace OwnerEventDo
  public export
  data OwnerEvent : Type -> Type where
       --Init : RouteSumT ->  FxEvent -> UserData -> OwnerEvent RouteKey --just sugar post event
       --Init : OwnerEvent ()
       NewRoute : Date -> RouteSumT -> OwnerEvent RouteKey
       --Close: (ref:RouteKey)  -> OwnerEvent ()              
       SetRouteNumber : DocumentNumber->RouteKey-> OwnerEvent ()
       
       UpdateUserData : UserData -> OwnerEvent ()       
       GetUserData : OwnerEvent UserDataMap
       
       ConfirmOrder : (fx:FxData) -> OwnerEvent RouteKey
       --GetFxData : (key:RouteKey) -> OwnerEvent (Maybe FxData)       
       GetRoute : (key:RouteKey) -> OwnerEvent (Maybe RouteSumT)
       Post : RouteKey -> MoveKey -> FxEvent -> OwnerEvent DocumentNumber
            
       --Get : MoveKey -> OwnerEvent Hom11
       --Get : MoveKey -> OwnerEvent (List WhsEntry)       
       GetWhs : (Maybe RouteKey) -> MoveKey -> OwnerEvent (List WhsEntry)       
              

       Allocate : Date->AllocationEntry -> OwnerEvent Bool 
       
       ListRefs : OwnerEvent (List RouteKey)
       ListDocs : OwnerEvent (List DocumentNumber)
       --SetAE : Ref -> AllocationEntry -> OwnerEvent ()
       --GetAE : Ref -> OwnerEvent (Maybe AllocationEntry)
       
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
  data WhsEvent : Type -> Type where
       NewRoute : Date -> RouteSumT -> WhsEvent RouteKey
       --CloseRoute : (ref:RouteKey) -> WhsEvent ()        
       GetRoute : (ref:RouteKey) -> WhsEvent (Maybe RouteSumT)
       
       SetRouteNumber : DocumentNumber->RouteKey-> WhsEvent ()
       GetRouteNumber : RouteKey -> WhsEvent (Maybe DocumentNumber)
       ListRefs : WhsEvent (List RouteKey)
       ListDocs : WhsEvent (List DocumentNumber)
       ListRoute : WhsEvent (List DocumentNumber)

       UpdateUserData : UserData -> WhsEvent ()
       GetUserDataW : WhsEvent UserDataMap       
       
       Put   : RouteKey -> MoveKey -> FxEvent -> WhsEvent DocumentNumber
       Get :   (Maybe RouteKey) -> MoveKey -> WhsEvent (List WhsEntry)
       
       Log : JournalLog -> WhsEvent () --Log state affecting events
       
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
   record SystemState where
      constructor MkSS
      --fx_map : SortedMap RouteKey FxData
      routes : SortedMap RouteKey RouteSumT
      --led_map : LocationMap
      jm   : SortedMap MoveKey (List WhsEntry)
      journal : List JournalLog
      user_data : UserDataMap
      web_socket : Maybe WsSocket
      allocentry : SortedMap RouteKey AllocationEntry
      
      name2hash : SortedMap DocumentNumber H256
      hash2name : SortedMap H256 DocumentNumber
      
      counters : SortedMap DocumentType Int
      docs : SortedMap H256 WhsEntry
      
      route_number : SortedMap RouteKey DocumentNumber
      route_key : SortedMap DocumentNumber RouteKey
   {-
   export
   Show SystemState where
      show (MkSS fx_map routes led_map jm j user_data ws ae  n2h h2n dcouners) = "system state"
   -}
export
unMaybe : Maybe String -> String
unMaybe Nothing = ""
unMaybe (Just x) = x

