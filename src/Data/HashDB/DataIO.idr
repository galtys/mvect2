module Data.HashDB.DataIO

import Data.HashDB.Types
import System.Directory
import System.File.ReadWrite
--import public System.File.Error
{-
import System.File.Handle
import public System.File.Error
import System.File.Support
import public System.File.Types
-}
import JSON

export
testList : List String
testList = [ (cast x) | x <- [1..10]]

export
tList : (a:HType) -> HType
tList a = fromArg [AType "List",toAPtr a]
export
tNil : (tList:HType) -> HType
tNil l = fromArg [ACon "NIL", toAPtr l]
export
tCons : (val:String) -> (prev:HType) -> (list:HType) -> HType
tCons val prev tl = fromArg [ACon "CONS", AVal val, toAPtr prev, toAPtr tl]
export
tSnocList : (a:HType) -> HType
tSnocList a = fromArg [AType "SnocList",toAPtr a]
export
tLin : (tList:HType) -> HType
tLin l = fromArg [ACon "LIN", toAPtr l]
export
tSnoc : (val:String) -> (prev:HType) -> (snoclist:HType) -> HType
tSnoc val prev tsl = fromArg [ACon "SNOC", toAPtr prev, AVal val, toAPtr tsl]

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
l1 = tCons "muf" nullStrListT StrListT
export
l2 : HType
l2 = tCons "ocx" l1 StrListT

data_store_dir : String
data_store_dir = "/home/jan/github.com/mvect2/data/"

export
readHcnt : HasIO io => TypePtr -> io (Either FileError String)
readHcnt tp = do
 let pth = data_store_dir ++ tp
 cnt <- readFile pth
 pure cnt


readHType : HasIO io => TypePtr -> io (Maybe HType)
readHType tp = do
  Right cnt <- readHcnt tp
    | Left x => pure Nothing  
  case (decode cnt) of
    Left x => pure Nothing
    Right arg => pure $Just (MkHT arg tp)

export
storeHType : HasIO io => HType -> io (Either FileError ())
storeHType ht = do
 let pth = data_store_dir ++ (ptr ht)
     cnt = (encode $ val ht)
 rw <- writeFile pth cnt
 pure rw
  
export
db_main : HasIO io => io ()
db_main = do
  Right d <- listDir data_store_dir 
    | Left x => printLn ("Directory does not exist:"++data_store_dir)
  
  printLn d
  
  printLn nullStrListT
  printLn l1
  printLn l2
  
  --let (prev, htype_map) = toHList testList
  --printLn prev
  --
  --traverse_ printLn (map snd (Data.SortedMap.toList htype_map))

