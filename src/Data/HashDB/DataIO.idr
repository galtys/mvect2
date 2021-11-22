module Data.HashDB.DataIO

import Data.HashDB.Types
import System.Directory
import System.File.ReadWrite
import Data.SnocList

--import public System.File.Error
{-
import System.File.Handle
import public System.File.Error
import System.File.Support
import public System.File.Types
-}
import JSON

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
StrSnocListT : HType
StrSnocListT = tList StrT

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

namespace DB_List
  db_write_list' : HasIO io => List String -> (prev:HType) ->(lt:HType)-> io (Either DBError TypePtr )
  db_write_list' [] prev lt = do  
    Right x <- storeHType prev
      | Left x => pure $Left $ EIO ("error writing: "++(ptr prev))
    pure $ Right $ (ptr prev)  

  db_write_list' (x :: xs) prev lt= do
     let new = tCons x prev lt --StrListT      
     Right ok <- storeHType new
       | Left x => pure $Left $ EIO (show x)     
     Right ret <- db_write_list' xs new lt
      | Left x => pure $Left $ EIO ("error writing: "++(ptr new))    
     pure $ Right ret

  export
  db_write_list : HasIO io => List String -> (lt:HType) -> io (Either DBError TypePtr)
  db_write_list xs lt = do
    let null = tNil lt --nullStrListT
    x<- storeHType null
    Right ret <- db_write_list' xs null lt
       | Left x => pure $ Left $ EIO (show x)
    pure $ Right ret

  export
  db_read_list : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (List String))
  db_read_list tp lt = do
    Right ht <- readHType tp
      | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)
    --printLn arg
    case arg of
      ( (ACon "CONS")::(AVal x)::(APtr prev)::(APtr ltype)::[]  ) => do
         Right ret_xs <- db_read_list prev lt
            | Left e => pure $ Left e       
         pure $ Right (x::ret_xs)       
      ( (ACon "NIL")::(APtr ltype)::[] ) => pure $ Right []
      _ => pure $ Left EHashLink

  export
  db_read_snoclist : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (SnocList String))
  db_read_snoclist tp lt = do
    Right ht <- readHType tp
      | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)
    --printLn arg
    case arg of
      ( (ACon "CONS")::(AVal x)::(APtr prev)::(APtr ltype)::[]  ) => do
         Right ret_xs <- db_read_snoclist prev lt
            | Left e => pure $ Left e       
         pure $ Right (ret_xs:<x)       
      ( (ACon "NIL")::(APtr ltype)::[] ) => pure $ Right [<]
      _ => pure $ Left EHashLink

export
testList : List String
testList = [ (cast x) | x <- [1..10]]

export
testList2 : List String
testList2 = [ "T2:"++x | x<- testList]    
export
db_main : HasIO io => io ()
db_main = do
  --printLn StrListT
  --let p_list = "0A769E8F51F42A4D935984EA4824CBCC7B969B724CA5E6DE6624FB5ABD97E65F"
  
  Right p_t1 <- db_write_list testList StrListT
     | Left x => printLn "error writing list"
  printLn p_t1

  Right p_t2 <- db_write_list testList2 StrListT
     | Left x => printLn "error writing list"
  printLn p_t2
        
  ret <- db_read_list p_t1 StrListT
  printLn ret

  Right ret <- db_read_snoclist p_t2 StrListT
     | Left e => pure ()
    
  printLn ret
  printLn $ toList ret
  
    
  --let (prev, htype_map) = toHList testList
  --printLn prev
  --
  --traverse_ printLn (map snd (Data.SortedMap.toList htype_map))

