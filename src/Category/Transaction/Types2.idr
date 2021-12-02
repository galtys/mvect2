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
record Location where
  constructor MkL
  --name : String
  directionTag: DirectionTag -- Sale | Purchase
  controlTag:   ControlTag   -- Self | Control |Partner
  ledger:       Ledger       -- OnHand | Forecast
  --address : Address
  
%runElab derive "Location" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
public export
record FxData where
   constructor MkFx
   date:Date
   l:Address
   h3:Hom3
%runElab derive "FxData" [Generic, Meta, RecordToJSON,RecordFromJSON]   
     
FxRef : Type 
FxRef = String --where --order reference used in warehouse

public export
data OrderEvent : Type -> Type where
     --New : Order FxData -> OrderEvent ()
     Move : (date:Date)->(h:Hom3)->(from:Location)->(to:Location)->OrderEvent ()     
       
     Confirm : Order FxData -> OrderEvent ()
     Invoice : FxData -> OrderEvent (Order FxData)
     
     Log : String -> OrderEvent ()
     Show : (Show ty) => ty -> OrderEvent ()
     Pure : ty -> OrderEvent ty
     Bind : OrderEvent a -> (a -> OrderEvent b) -> OrderEvent b

--%runElab derive "OrderEvent" [Generic, Meta, Eq, Ord,Show,ToJSON,FromJSON]

namespace OrderEventDo
  public export
  (>>=) : OrderEvent a -> (a -> OrderEvent b) -> OrderEvent b
  (>>=) = Bind

  public export
  (>>) : OrderEvent () -> OrderEvent b -> OrderEvent b
  ma >> mb = Bind ma (\ _ => mb)


