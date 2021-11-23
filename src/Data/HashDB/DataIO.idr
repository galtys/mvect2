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
  head : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (Maybe (String,TypePtr)) )
  head tp lt = do
    Right ht <- readHType tp
      | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)    
    case arg of
      ( (ACon "CONS")::(AVal x)::(APtr prev)::(APtr ltype)::[]  ) => do
         pure $ Right (Just (x,prev))
      ( (ACon "NIL")::(APtr ltype)::[] ) => pure $ Right Nothing
      _ => pure $ Left EHashLink
  
  export
  read : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (List String))
  read tp lt = do  
    Right ht <- DBList.head tp lt
      | Left e => pure $ Left e
    case ht of
      Just (x,prev) => do
         Right ret_xs <- DBList.read prev lt
            | Left e => pure $ Left e       
         pure $ Right (x::ret_xs)       
      Nothing => pure $ Right []

  export
  readAsSnocList : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (SnocList String))
  readAsSnocList tp lt = do
    Right ht <- DBList.head tp lt
      | Left e => pure $ Left e
    case ht of
      Just (x,prev) => do
         Right ret_xs <- DBList.readAsSnocList prev lt
            | Left e => pure $ Left e       
         pure $ Right (ret_xs:<x)
         
      Nothing => pure $ Right [<]
  

namespace DBSnocList
  export
  append : HasIO io=>String->(prev:HType)->(lt:HType)->io (Either DBError HType )
  append item prev lt = do
     let new_item = tSnoc item prev lt
     Right ok <- storeHType new_item
       | Left x => pure $Left $ EIO (show x)     
     pure $ Right (new_item)
  export
  head : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (Maybe (String,TypePtr)) )
  head tp lt = do
    Right ht <- readHType tp
      | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)    
    case arg of
      ( (ACon "SNOC")::(APtr prev)::(AVal x)::(APtr ltype)::[]  ) => do
         pure $ Right (Just (x,prev))
      ( (ACon "LIN")::(APtr ltype)::[] ) => pure $ Right Nothing
      _ => pure $ Left EHashLink
     
  export
  new :HasIO io=>(lt:HType)->io (Either DBError HType )
  new lt = do
    let null = tLin lt --nullStrSnocListT
    Right x<- storeHType null
      | Left y => pure $Left $ EIO (show y)
    pure $ Right null
  
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
    Right null <- DBSnocList.new lt
       | Left x => pure $ Left $ x
    Right ret <- DBSnocList.write' xs null lt
       | Left x => pure $ Left $ EIO (show x)
    pure $ Right ret
    
  export
  read : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (List String))
  read tp lt = do
    Right ht <- DBSnocList.head tp lt
      | Left e => pure $ Left e
    case ht of
      Just (x,prev) => do
         Right ret_xs <- DBSnocList.read prev lt
            | Left e => pure $ Left e       
         pure $ Right (x::ret_xs)       
      Nothing => pure $ Right []
            
  export
  readAsSnocList : HasIO io => TypePtr -> (lt:HType)->io (Either DBError (SnocList String))
  readAsSnocList tp lt = do
    Right ht <- DBSnocList.head tp lt
      | Left e => pure $ Left e
    case ht of
      Just (x,prev) => do
         Right ret_xs <- DBSnocList.readAsSnocList prev lt
            | Left e => pure $ Left e       
         pure $ Right (ret_xs:<x)         
      Nothing => pure $ Right [<]
  
  toDBList': HasIO io => (snoc:TypePtr) -> (slt:HType)->(lt:HType)->(dst:HType) -> io (Either DBError TypePtr)
  toDBList' snoc slt lt dst = do
    Right ht <- DBSnocList.head snoc slt
      | Left e => pure $ Left e
    case ht of
      Just (x,snocprev) => do
         Right ni <- DBList.append x dst lt
           | Left e => pure $ Left e
         Right ret <- DBSnocList.toDBList' snocprev slt lt ni
           | Left e => pure $ Left e
         pure $ Right ret
      Nothing => pure $ Right (ptr dst)
  export    
  toDBList : HasIO io => (snoc:TypePtr) -> (slt:HType)->(lt:HType) -> io (Either DBError (Maybe TypePtr) )
  toDBList snoc slt lt = do
    Right ht <- DBSnocList.head snoc slt
      | Left e => pure $ Left e
    
    case ht of
      Just (x,prev) => do 

         Right null <- DBList.new lt
            | Left e => pure $ Left $ e             
         Right ret <- DBSnocList.toDBList' snoc slt lt null
            | Left e => pure $ Left $ e    
         pure $ Right (Just ret)
      
      Nothing => pure $ Right Nothing

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
  new : HasIO io=> DBQueue.Name -> io (Either DBError DBQueue.FR)
  new qn = do
     Right new_f <- DBList.new StrListT
       | Left x => pure $Left x       
     Right new_r <- DBSnocList.new StrSnocListT
       | Left x => pure $Left x    
     pure $ Right (MkFR (ptr new_f) (ptr new_r) qn)
  
  checkf : HasIO io=> DBQueue.FR -> io (Either DBError DBQueue.FR) 
  checkf (MkFR f r qn) = do
      Right hx <- DBList.head f StrListT 
        | Left y => pure $ Left y
      case hx of
         Nothing => do 
            Right new_f <- DBSnocList.toDBList r StrSnocListT StrListT
              | Left e => pure$Left e              
            Right new_r <- DBSnocList.new StrSnocListT
              | Left e => pure $Left e
            case new_f of 
              Just nf => pure $ Right (MkFR nf (ptr new_r) qn)    
              Nothing => pure $ Right (MkFR f r qn)
         Just h =>  pure $ Right $ (MkFR f r qn)
  export
  snoc : HasIO io=> DBQueue.FR -> String ->io (Either DBError DBQueue.FR) 
  snoc (MkFR f pr qn) item = do  
      Right r <- readHType pr 
        | Left e => pure $ Left e
      Right ht <- DBSnocList.append item r StrSnocListT
        | Left e => pure $ Left e
      ret <- DBQueue.checkf (MkFR f (ptr ht)  qn)
      pure ret
  
  export
  tail : HasIO io=> DBQueue.FR ->io (Either DBError DBQueue.FR) 
  tail (MkFR pf pr qn) = do      
      Right hx <- DBList.head pf StrListT 
         | Left y => pure $ Left y
      case hx of
         Just (x,p_xsf) => do
            ret <- DBQueue.checkf (MkFR p_xsf pr  qn)
            pure ret             
         Nothing => do       
            ret <- DBQueue.checkf (MkFR pf pr  qn)
            pure ret
  export
  head : HasIO io=> DBQueue.FR -> io (Either DBError (Maybe (String,TypePtr)) )
  head (MkFR f r qn) = (DBList.head f StrListT)
     
  show : HasIO io=> DBQueue.FR -> io (Either DBError () )
  show (MkFR p_f p_r qn) = do
     printLn ("f: "++qn)
     retf <- DBList.read p_f StrListT
     printLn retf
     printLn ("r: "++qn)     
     retr <- DBSnocList.read p_r StrSnocListT
     printLn retr
     pure $ Right ()
      
      {-
  checkf : Queue a -> Queue a
  checkf (MkQ [] r) = MkQ (toList r) [<]
  checkf q = q 
  export
  snoc : {a:Type} -> Queue a -> a -> Queue a
  snoc (MkQ f r) x = checkf (MkQ f (r:<x))
  export
  tail : Queue a -> Queue a
  tail (MkQ [] r) = checkf (MkQ [] r)
  tail (MkQ (x :: xs) r) = checkf (MkQ xs r)  
  export -}
        
  {-
    export
  head : Queue a -> Maybe a   
  head (MkQ [] r) = Nothing
  head (MkQ (x :: xs) r) = Just x
-}
  
export
test_f : List String
test_f = [ (cast x) | x <- [1..5]]

export
test_r : List String
test_r = [ (cast x) | x<- [6..10]]    


export
db_test_queue : HasIO io => io ()
db_test_queue = do
  Right q1 <- DBQueue.new "test" 
    | Left e => pure () 
  
  Right q1 <- DBQueue.snoc q1 "t3ocas" 
    | Left e => pure () 
  Right q1 <- DBQueue.snoc q1 "8ssa" 
    | Left e => pure () 
  Right q1 <- DBQueue.snoc q1 "ts" 
    | Left e => pure () 
  Right q1 <- DBQueue.snoc q1 "qq" 
    | Left e => pure ()     
    
  Right q1 <- DBQueue.tail q1
    | Left e => pure () 
  Right q1 <- DBQueue.tail q1
    | Left e => pure () 
    
  ret <- DBQueue.show q1  
  printLn ret    
  Right h <- DBQueue.head q1
    | Left e => pure ()   
  printLn h

export
db_main : HasIO io => io ()
db_main = do
  db_test_queue
  --let p_list = "0A769E8F51F42A4D935984EA4824CBCC7B969B724CA5E6DE6624FB5ABD97E65F"

export
db_list_test : HasIO io => io ()
db_list_test = do
  --printLn StrListT
  --let p_list = "0A769E8F51F42A4D935984EA4824CBCC7B969B724CA5E6DE6624FB5ABD97E65F"
  printLn "create front"  
  Right p_f <- DBList.write test_f StrListT
     | Left x => printLn "error writing list"
  --printLn p_t1  
  ret <- DBList.read p_f StrListT
  printLn ret


  printLn "create rear"
  Right p_r <- DBSnocList.write test_r StrSnocListT
     | Left x => printLn "error writing snoclist"
  printLn p_r
  
  printLn "rear printing"  
  ret <- DBSnocList.read p_r StrSnocListT
  printLn ret
  
  printLn "converting: .."
  Right p_r_cnv <- DBSnocList.toDBList p_r StrSnocListT StrListT
     | Left x => printLn (show x)
  printLn p_r_cnv
  
  printLn "print converted: .."
  
  case p_r_cnv of
    (Just px) => do
       ret <- DBList.read px StrListT
       printLn ret
    Nothing => pure ()
  
{-

  Right ret <- DBList.readAsSnocList p_t1 StrListT
     | Left e => pure ()
  printLn ret
  -}
    
  --let (prev, htype_map) = toHList testList
  --printLn prev
  --
  --traverse_ printLn (map snd (Data.SortedMap.toList htype_map))

