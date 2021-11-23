module Data.HashDB.Types

import Crypto.Hash.SHA256
--import Data.Vect
import Generics.Derive
import JSON
import Data.SortedMap

%language ElabReflection

public export
TypePtr : Type
TypePtr = String --H256

public export
data Arg = AType String | AVal String | ACon String | APtr TypePtr | AVar String
%runElab derive "Arg" [Generic, Meta, Eq, Ord, Show,ToJSON,FromJSON]
public export
data DBError = EIO String | EJS | EHashLink
%runElab derive "DBError" [Generic, Meta, Eq, Ord, Show,ToJSON,FromJSON]

--public export

toString : Arg -> String
toString (AType x) = x
toString (AVal x) = x
toString (ACon x) = x
toString (APtr x) = x
toString (AVar x) =x

public export
record HType where
  constructor MkHT
  val : List Arg
  ptr : TypePtr
%runElab derive "HType" [Generic, Meta, Eq, Ord,RecordToJSON,RecordFromJSON]

public export
Show HType where
  show ht = (show $ ptr ht ) ++ "->"++(show $ val ht) 

getPtr : List Arg -> TypePtr
getPtr [] = (sha256 "")
getPtr (x::xs) = sha256 ((sha256 $ toString x)++(getPtr xs))

export
fromArg : List Arg -> HType
fromArg x = MkHT x (getPtr x)

export
toType : String -> HType
toType x = fromArg [AType x]
export
toAPtr : HType -> Arg
toAPtr x = (APtr (ptr x))

namespace DBQueue
  public export
  Name : Type
  Name = String
  public export
  record FR where
    constructor MkFR
    f : TypePtr
    r : TypePtr
    lt : HType    
    slt : HType
  %runElab derive "DBQueue.FR" [Generic, Meta, Eq, Ord, Show,RecordToJSON,RecordFromJSON]

namespace Subscriber
  public export
  record Rec where
    constructor MkS
    mg_index : Int
    inq : DBQueue.FR -- incomming msg queue
    outq : DBQueue.FR -- outgoing msg queue

namespace Observable
  public export
  data OblCmd = ObAll | ObOne 
  %runElab derive "OblCmd" [Generic, Meta, Eq, Ord, Show,EnumToJSON,EnumFromJSON]
  
  public export
  record Rec where
    constructor MkO
    subscribers : TypePtr -- List of Subscriber s
    removed : TypePtr  -- List of deactivated subscribers, should be tree
    lo : HType
    lr : HType
    inq : DBQueue.FR -- incomming msg queue
    cmd : DBQueue.FR -- command queue
    
  %runElab derive "Observable.Rec" [Generic, Meta, Eq, Ord, Show,RecordToJSON,RecordFromJSON]
