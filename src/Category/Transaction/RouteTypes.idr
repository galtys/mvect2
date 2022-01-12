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
record OrderControlRoute where 
   constructor MkORrec
   allocation : MoveKey 
   control : MoveKey
   order : MoveKey   
   direction : DirectionTag    
%runElab derive "OrderControlRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   
public export
record ReconciliationRoute where 
   constructor MkRR
   allocation : MoveKey  --allocation
   reconcile : MoveKey
   direction : DirectionTag 
%runElab derive "ReconciliationRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
record ListRoute where 
   constructor MkListR
   allocation : MoveKey 
   lst : List MoveKey      
   direction : DirectionTag 
%runElab derive "ListRoute" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
record VectRoute (n:Nat) where 
   constructor MkVectR
   allocation : MoveKey 
   lst : Vect n MoveKey      
%runElab derive "VectRoute" [Generic, Meta]      

public export
data RouteSumT =   MkReR ReconciliationRoute | MkAl ListRoute | MkOR OrderControlRoute 
%runElab derive "RouteSumT" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]   
export
allocationMove : RouteSumT -> MoveKey
allocationMove (MkOR (MkORrec allocation control order d)) = allocation
allocationMove (MkReR (MkRR allocation reconcile d)) = allocation
allocationMove (MkAl (MkListR allocation lst d)) = allocation

public export
data DocumentType = Order | Invoice |CreditNote| Payment | Refund | Delivery |Dispatch| Return | Reservation |Allocation|Shipping
%runElab derive "DocumentType" [Generic, Meta, Eq,Show,Ord,EnumToJSON,EnumFromJSON]

public export
data Ref = MkAllocationRef AllocationRef | MkRouteKeyRef RouteKey
%runElab derive "Ref" [Generic, Meta, Eq,Show,Ord,ToJSON,FromJSON]   
            
public export
record AllocationItem where
  constructor MkAI
  supplier : RouteKey
  customer: RouteKey
  fx : FxEvent
%runElab derive "AllocationItem" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   
public export
record AllocationEntry where
  constructor MkAE
  ledger : Ledger  
  moves : List AllocationItem
%runElab derive "AllocationEntry" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   



public export
record WhsEntry where
     constructor MkWE
     ref : Ref   
     fx : FxEvent
%runElab derive "WhsEntry" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
record RouteMetaKey where
   constructor MkRMK
   --move : MoveKey
   --ui_info   : List (DxCx,(DrCr,String) )
   --location : Location
   --dir : DirectionTag
   --ledger : Ledger
   move : MoveKey
   dxcx : DxCx
   drcr : DrCr
   --doc : String
   --whse : List WhsEntry
%runElab derive "RouteMetaKey" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   

public export
record RouteLine where
   constructor MkRL
   move : MoveKey
   whse_f : List (WhsEntry,DocumentType)
   whse_oh : List (WhsEntry,DocumentType)
   --doc_f : List DocumentType   
   --doc_oh : List DocumentType   
   --forecast : RouteDataLine
   --onhand : RouteDataLine
%runElab derive "RouteLine" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   


public export
record RouteData where
   constructor MkRD
   key : RouteKey   
   dir : DirectionTag
   lines : List RouteLine 
%runElab derive "RouteData" [Generic, Meta, Eq,Show,Ord,RecordToJSON,RecordFromJSON]   
