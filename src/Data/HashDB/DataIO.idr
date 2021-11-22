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

namespace DBList
  export
  append : HasIO io=>String->(prev:HType)->(lt:HType)->io (Either DBError HType )
  append item prev lt = do
     let new_item = tCons item prev lt --StrListT      
     Right ok <- storeHType new_item
       | Left x => pure $Left x    
     pure $ Right (new_item)
  export
  new :HasIO io=>(lt:HType)->io (Either DBError HType )
  new lt = do
    let null = tNil lt --nullStrListT
    Right x<- storeHType null
      | Left y => pure $Left $ EIO (show y)
    pure $ Right null

  write' : HasIO io => List String -> (prev:HType) ->(lt:HType)-> io (Either DBError TypePtr )
  write' [] last lt = do  
    Right x <- storeHType last
      | Left x => pure $Left $ EIO ("error writing: "++(ptr last))
    pure $ Right $ (ptr last)  
  write' (x :: xs) prev lt= do
     Right newi <- DBList.append x prev lt
       | Left x => pure $Left $ EIO (show x)
       
     Right ret <- DBList.write' xs newi lt
      | Left x => pure $Left $ EIO ("error writing: "++(ptr newi))    
     pure $ Right ret

  export
  write : HasIO io => List String -> (lt:HType) -> io (Either DBError TypePtr)
  write xs lt = do
        
    Right null <- DBList.new lt
       | Left x => pure $ Left $ x
    
    Right ret <- DBList.write' xs null lt
       | Left x => pure $ Left $ EIO (show x)
    pure $ Right ret

  export
  head : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (Maybe String))
  head tp lt = do
    Right ht <- readHType tp
      | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)

    case arg of
      ( (ACon "CONS")::(AVal x)::(APtr prev)::(APtr ltype)::[]  ) => do
         --Right ret_xs <- DBList.read prev lt
         --   | Left e => pure $ Left e       
         pure $ Right (Just x)
      ( (ACon "NIL")::(APtr ltype)::[] ) => pure $ Right Nothing
      _ => pure $ Left EHashLink
    
  export
  read : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (List String))
  read tp lt = do
  
    Right ht <- readHType tp
      | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)
    --printLn arg
    case arg of
      ( (ACon "CONS")::(AVal x)::(APtr prev)::(APtr ltype)::[]  ) => do
         Right ret_xs <- DBList.read prev lt
            | Left e => pure $ Left e       
         pure $ Right (x::ret_xs)       
      ( (ACon "NIL")::(APtr ltype)::[] ) => pure $ Right []
      _ => pure $ Left EHashLink

  export
  readAsSnocList : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (SnocList String))
  readAsSnocList tp lt = do
    Right ht <- readHType tp
      | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)
    --printLn arg
    case arg of
      ( (ACon "CONS")::(AVal x)::(APtr prev)::(APtr ltype)::[]  ) => do
         Right ret_xs <- DBList.readAsSnocList prev lt
            | Left e => pure $ Left e       
         pure $ Right (ret_xs:<x)       
      ( (ACon "NIL")::(APtr ltype)::[] ) => pure $ Right [<]
      _ => pure $ Left EHashLink

namespace DBSnocList
  export
  append : HasIO io=>String->(prev:HType)->(lt:HType)->io (Either DBError HType )
  append item prev lt = do
     let new_item = tSnoc item prev lt
     Right ok <- storeHType new_item
       | Left x => pure $Left $ EIO (show x)     
     pure $ Right (new_item)
  
  write' : HasIO io => List String -> (prev:HType) ->(lt:HType)-> io (Either DBError TypePtr )
  write' [] prev lt = do  
    Right x <- storeHType prev
      | Left x => pure $Left $ EIO ("error writing: "++(ptr prev))
    pure $ Right $ (ptr prev)  
    
  write' (x :: xs) prev lt= do
     Right new <- DBSnocList.append x prev lt
       | Left x => pure $Left x 
           
     Right ret <- DBSnocList.write' xs new lt
      | Left x => pure $Left x
     pure $ Right ret

  export
  write : HasIO io => List String -> (lt:HType) -> io (Either DBError TypePtr)
  write xs lt = do
    let null = tLin lt
    x<- storeHType null
    Right ret <- DBSnocList.write' xs null lt
       | Left x => pure $ Left $ EIO (show x)
    pure $ Right ret

  export
  read : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (List String))
  read tp lt = do
    Right ht <- readHType tp
      | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)
    --printLn arg
    case arg of
      ( (ACon "SNOC")::(APtr prev)::(AVal x)::(APtr ltype)::[]  ) => do
         Right ret_xs <- DBSnocList.read prev lt
            | Left e => pure $ Left e       
         pure $ Right (x::ret_xs)       
      ( (ACon "LIN")::(APtr ltype)::[] ) => pure $ Right []
      _ => pure $ Left EHashLink

  export
  readAsSnocList : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (SnocList String))
  readAsSnocList tp lt = do
    Right ht <- readHType tp
      | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)
    --printLn arg
    case arg of
      ( (ACon "SNOC")::(APtr prev)::(AVal x)::(APtr ltype)::[]  ) => do
         Right ret_xs <- DBSnocList.readAsSnocList prev lt
            | Left e => pure $ Left e       
         pure $ Right (ret_xs:<x)       
      ( (ACon "LIN")::(APtr ltype)::[] ) => pure $ Right [<]
      _ => pure $ Left EHashLink

namespace DBQueue
  export
  Name : Type
  Name = String

  export
  record FR where
    constructor MkFR
    f : TypePtr
    r : TypePtr
    qn : DBQueue.Name
  export
  new : HasIO io=> DBQueue.Name -> io (Either DBError FR)
  new qn = do
     Right new_f <- new StrListT
       | Left x => pure $Left x       
     Right new_r <- new StrListT
       | Left x => pure $Left x    
     pure $ Right (MkFR (ptr new_f) (ptr new_r) qn)
  {-
    export
  head : Queue a -> Maybe a   
  head (MkQ [] r) = Nothing
  head (MkQ (x :: xs) r) = Just x
-}
  head : HasIO io=> DBQueue.FR -> io (Either DBError (Maybe String) )
  head (MkFR f r qn) = (DBList.head f StrListT)
     
   
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
  
  Right p_t1 <- DBList.write testList StrListT
     | Left x => printLn "error writing list"
  printLn p_t1
  ret <- DBList.read p_t1 StrListT
  printLn ret

  printLn "snoclist"
  Right p_ts <- DBSnocList.write testList StrSnocListT
     | Left x => printLn "error writing snoclist"
  printLn p_ts
  
  ret <- DBSnocList.read p_ts StrSnocListT
  printLn ret

{-

  Right ret <- DBList.readAsSnocList p_t1 StrListT
     | Left e => pure ()
  printLn ret
  -}
    
  --let (prev, htype_map) = toHList testList
  --printLn prev
  --
  --traverse_ printLn (map snd (Data.SortedMap.toList htype_map))

