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
