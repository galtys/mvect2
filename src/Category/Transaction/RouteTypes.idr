module Category.Transaction.RouteTypes

import Data.SnocList
import Category.Transaction.Types
import Libc.Time
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
data Location =  Self | In BrowseResPartner.RecordModel | Out BrowseResPartner.RecordModel | Border BrowseResPartner.RecordModel |Init | Loss | Control DirectionTag BrowseResPartner.RecordModel |Partner DirectionTag BrowseResPartner.RecordModel | Transit DirectionTag BrowseResPartner.RecordModel | Taxman BrowseResPartner.RecordModel | Bank BrowseResPartner.RecordModel
%runElab derive "Location" [Generic, Meta, Eq, Ord,Show,ToJSON,FromJSON]

public export
AllocationRef : Type
AllocationRef = String
public export     
RouteRef : Type 
RouteRef = String --where --order reference used in warehouse

public export
data FxEvent = Fx121 Date Hom121 | Fx11 Date Hom11 
%runElab derive "FxEvent" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]

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
record MoveKey where
  constructor MkMK
  from : Location
  to : Location
  ledger : Ledger
%runElab derive "MoveKey" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
convMovekey : MoveKey -> MoveKey
convMovekey (MkMK from to OnHand) = (MkMK from to Forecast)
convMovekey (MkMK from to Forecast) = (MkMK from to OnHand)


public export
record SaleForecastRoute where 
   constructor MkSFR
   order : MoveKey
   control : MoveKey
   allocation : MoveKey   --allocation part
   direction : DirectionTag    
%runElab derive "SaleForecastRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   
public export
record OrderRoute where 
   constructor MkORrec
   allocation : MoveKey   --allocation part
   control : MoveKey
   order : MoveKey   
   direction : DirectionTag    
%runElab derive "OrderRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
record PurchaseForecastRoute where 
   constructor MkPFR
   allocation : MoveKey --allocation part
   control : MoveKey
   order : MoveKey
   direction : DirectionTag    
%runElab derive "PurchaseForecastRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
record ReconciliationRoute where --reconcile with 3rd party
   constructor MkRR
   allocation : MoveKey  --allocation
   reconcile : MoveKey
   direction : DirectionTag 
%runElab derive "ReconciliationRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
record ListRoute where --reconcile with 3rd party is not involved
   constructor MkListR
   allocation : MoveKey  --allocation only
   lst : List MoveKey      
   direction : DirectionTag 
%runElab derive "ListRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
record VectRoute (n:Nat) where --reconcile with 3rd party is not involved
   constructor MkVectR
   allocation : MoveKey  --allocation only
   lst : Vect n MoveKey      
%runElab derive "VectRoute" [Generic, Meta]      
--%runElab derive "VectRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

{-
public export
record SnocListRoute where --reconcile with 3rd party is not involved
   constructor MkSnocListR
   allocation : MoveKey  --allocation only
   lst : SnocList MoveKey      
%runElab derive "SnocListRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   
-}

{-
public export
record ListRoute where
   constructor MkLR
   allocation : MoveKey  --allocation only
   lst : List MoveKey   


   
public export
Route : Type
Route = List Location
-}
public export
data RouteSumT = MkSoR SaleForecastRoute | MkPoR PurchaseForecastRoute | MkReR ReconciliationRoute | MkAl ListRoute | MkOR OrderRoute
%runElab derive "RouteSumT" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]   
export
allocationMove : RouteSumT -> MoveKey
allocationMove (MkSoR (MkSFR order control allocation d)) = allocation
allocationMove (MkPoR (MkPFR allocation control order d)) = allocation
allocationMove (MkOR (MkORrec allocation control order d)) = allocation
allocationMove (MkReR (MkRR allocation reconcile d)) = allocation
allocationMove (MkAl (MkListR allocation lst d)) = allocation



public export
data Ref = MkAllocationRef AllocationRef | MkRouteKeyRef RouteKey
%runElab derive "Ref" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]   
            
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
public export
record AllocationEntry where
  constructor MkAE
  --date : Date
  ledger : Ledger  
  moves : List AllocationItem --(Route,Route,Ledger,FxEvent)
%runElab derive "AllocationEntry" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

