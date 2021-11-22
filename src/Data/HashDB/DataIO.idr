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
 --printLn pth
 cnt <- readFile pth
 pure cnt


readHType : HasIO io => TypePtr -> io (Either DBError HType)
readHType tp = do
  Right cnt <- readHcnt tp
    | Left x => pure $ Left $EIO (show x)
  case (decode cnt) of
    Left x => pure $ Left $ EIO (show x) --Nothing
    Right arg => pure $ Right (MkHT arg tp)

export
storeHType : HasIO io => HType -> io (Either DBError ())
storeHType ht = do
 --putStrLn$show ht
 
 let pth = data_store_dir ++ (ptr ht)
     cnt = (encode $ val ht)
 Right ret <- writeFile pth cnt
     | Left x => pure $ Left (EIO $show x)
 pure $ Right ()

db_write_test : HasIO io => io ()  
db_write_test = do
  Right d <- listDir data_store_dir 
    | Left x => printLn ("Directory does not exist:"++data_store_dir)  
  
  Right x <- storeHType nullStrListT
    | Left x => printLn x
  
  Right x <- storeHType l1
    | Left x => printLn x
  
  Right x <- storeHType l2
    | Left x => printLn x
  pure ()

db_write_list' : HasIO io => List String -> (prev:HType) -> io (Either DBError TypePtr )
db_write_list' [] prev = do  

  Right x <- storeHType prev
    | Left x => pure $Left $ EIO ("error writing: "++(ptr prev))
  pure $ Right $ (ptr prev)  

db_write_list' (x :: xs) prev = do
   let new = tCons x prev StrListT   
   
   Right ok <- storeHType new
     | Left x => pure $Left $ EIO (show x)
     
   Right ret <- db_write_list' xs new
    | Left x => pure $Left $ EIO ("error writing: "++(ptr new))
    
   pure $ Right ret

--0A769E8F51F42A4D935984EA4824CBCC7B969B724CA5E6DE6624FB5ABD97E65F
db_write_list : HasIO io => List String -> io (Either DBError TypePtr)
db_write_list xs = do
  let null = nullStrListT
  x<- storeHType null
  Right ret <- db_write_list' xs null
     | Left x => pure $ Left $ EIO (show x)
  pure $ Right ret

export
db_main : HasIO io => io ()
db_main = do
  
  Right p_list <- db_write_list testList
     | Left x => printLn "error writing list"
  printLn p_list
  
  --printLn (ptr l1)
  
  
  --let (prev, htype_map) = toHList testList
  --printLn prev
  --
  --traverse_ printLn (map snd (Data.SortedMap.toList htype_map))

