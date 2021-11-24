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
data DBError = EIO String | EJS | EHashLink | ErrorJS String
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

public export
data HCommand : Type -> Type where
     Store : HType -> HCommand ()
     Read : TypePtr -> HCommand HType --String
     Log : String -> HCommand ()
     Show : (Show ty) => ty -> HCommand ()
     LinkError : ty -> HCommand ty
     DecodeError : ty -> HCommand ty
     Pure : ty -> HCommand ty
     Bind : HCommand a -> (a -> HCommand b) -> HCommand b

namespace HCommandDo
  public export
  (>>=) : HCommand a -> (a -> HCommand b) -> HCommand b
  (>>=) = Bind

  public export
  (>>) : HCommand () -> HCommand b -> HCommand b
  ma >> mb = Bind ma (\ _ => mb)




getPtr : List Arg -> TypePtr
getPtr [] = (sha256 "")
getPtr (x::xs) = sha256 ((sha256 $ toString x)++(getPtr xs))

export
fromArg : List Arg -> HType
fromArg x = MkHT x (getPtr x)

export
fromArgCmd : List Arg -> HCommand HType
fromArgCmd x = do
  --let p_x = (getPtr x)
  let ht =MkHT x (getPtr x)
  Store ht
  Pure ht

export
toType : String -> HType
toType x = fromArg [AType x]

export
fromName : String -> HCommand HType
fromName x = fromArgCmd [AVar x]

export
toAPtr : HType -> Arg
toAPtr x = (APtr (ptr x))

namespace DBQueueStr--DBQueue
{-  public export
  Name : Type
  Name = String-}
  public export
  record FR where
    constructor MkFR
    f : TypePtr
    r : TypePtr
    lt : HType    
    slt : HType
  %runElab derive "DBQueueStr.FR" [Generic, Meta, Eq, Ord, Show,RecordToJSON,RecordFromJSON]

namespace Subscriber
  public export
  record Rec where
    constructor MkS
    mg_index : Int
    inq : DBQueueStr.FR -- incomming msg queue
    outq : DBQueueStr.FR -- outgoing msg queue
  %runElab derive "Subscriber.Rec" [Generic, Meta, Eq, Ord, Show,RecordToJSON,RecordFromJSON]

namespace Observable

  public export
  data Cmd = ObAll | ObOne 
  %runElab derive "Observable.Cmd" [Generic, Meta, Eq, Ord, Show,EnumToJSON,EnumFromJSON]
  
  public export
  record Rec where
    constructor MkO
    subscribers : TypePtr -- List of Subscriber s
    removed : TypePtr  -- List of deactivated subscribers, should be tree
    lo : HType
    lr : HType
    inq : DBQueueStr.FR -- incomming msg queue
    cmd : DBQueueStr.FR -- command queue
    
  %runElab derive "Observable.Rec" [Generic, Meta, Eq, Ord, Show,RecordToJSON,RecordFromJSON]

