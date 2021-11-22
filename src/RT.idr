module RT

import Crypto.Hash.SHA256
import Data.Vect
import Generics.Derive
import JSON
import Data.SortedMap

%language ElabReflection

public export
TypePtr : Type
TypePtr = H256

public export
data Arg = AType String | AVal String | ACon String | APtr TypePtr
%runElab derive "Arg" [Generic, Meta, Eq, Ord, Show,ToJSON,FromJSON]

public export
toString : Arg -> String
toString (AType x) = x
toString (AVal x) = x
toString (ACon x) = x
toString (APtr x) = x

public export
record HType where
  constructor MkHT
  val : List Arg
  ptr : TypePtr
%runElab derive "HType" [Generic, Meta, Eq, Ord, Show,RecordToJSON,RecordFromJSON]

public export
getPtr : List Arg -> TypePtr
getPtr [] = (sha256 "")
getPtr (x::xs) = sha256 ((sha256 $ toString x)++(getPtr xs))

public export
Show HType where
  show ht = (show $ ptr ht ) ++ "->"++(show $ val ht) 

export
fromArg : List Arg -> HType
fromArg x = MkHT x (getPtr x)

export
toType : String -> HType
toType x = fromArg [AType x]

export
toVal : String -> HType
toVal x = fromArg [AVal x]

export
toCon : String -> HType
toCon x = fromArg [ACon x]
export
toAPtr : HType -> Arg
toAPtr x = (APtr (ptr x))


export
addPtr : HType -> HType -> HType
addPtr (MkHT val1 ptr1) (MkHT val2 ptr2) = (fromArg (val1++val2))

export
tList : (a:HType) -> HType
tList a = fromArg [AType "List",toAPtr a]


export
tNil : (tList:HType) -> HType
tNil l = fromArg [ACon "NIL", toAPtr l]

export
tCon : (val:String) -> (prev:HType) -> (list:HType) -> HType
tCon val prev tl = fromArg [ACon "CON", AVal val, toAPtr prev, toAPtr tl]

-- Using list str
export
StrT : HType
StrT = toType "String"

export
StrListT : HType
StrListT = tList StrT

export
nullStrListT : HType
nullStrListT = tNil StrListT

export
l1 : HType
l1 = tCon "muf" nullStrListT StrListT

export
l2 : HType
l2 = tCon "ocx" l1 StrListT

safeHead : {a:Type} -> List a -> Maybe a
safeHead [] = Nothing
safeHead (x::xs) = Just x

export
toHList : List String -> (HType, SortedMap TypePtr HType)
toHList [] = (nullStrListT,(insert k nullStrListT empty)) where
    k : TypePtr
    k = ptr nullStrListT
toHList (x::xs) =  (new, (insert k new prev_map)) where
     prev : (HType, SortedMap TypePtr HType)
     prev = toHList xs
     
     prev_ht : HType
     prev_ht = fst prev
     prev_map : SortedMap TypePtr HType
     prev_map = snd prev
     new : HType
     new = tCon x prev_ht StrListT
     k : TypePtr
     k = ptr new


export
testList : List String
testList = [ (cast x) | x <- [1..10]]



