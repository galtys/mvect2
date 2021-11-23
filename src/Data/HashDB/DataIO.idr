module Data.HashDB.DataIO

import Data.HashDB.Types
import System.Directory
import System.File.ReadWrite
import Data.SnocList

import Control.Monad.Either

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
StrSnocListT = tSnocList StrT

data_store_dir : String
data_store_dir = "/home/jan/github.com/mvect2/data/"

export
readHcnt : HasIO io => TypePtr -> io (Either FileError String)
readHcnt tp = do
 let pth = data_store_dir ++ tp
 --printLn pth
 cnt <- readFile pth
 pure cnt


readHType : HasIO io=>MonadError DBError io => TypePtr -> io HType
readHType tp = do
  Right cnt <- readHcnt tp
    | Left e => throwError (EIO $show e) --pure $ Left $EIO (show x)
  case (decode cnt) of
    Left e => throwError (EIO $show e) --pure $ Left $ EIO (show x) 
    Right arg => pure (MkHT arg tp)

export
storeHType: HasIO io=>MonadError DBError io => HType -> io ()
storeHType ht = do
 --putStrLn$show ht 
 let pth = data_store_dir ++ (ptr ht)
     cnt = (encode $ val ht)
 Right ret <- writeFile pth cnt
     | Left e => throwError (EIO $show e)
 pure ()

namespace DBList
  export
  new :HasIO io=>MonadError DBError io =>(lt:HType)->io ( HType )
  new lt = do
    let null = tNil lt --nullStrListT
    storeHType null
    --  | Left e => throwError e --$ ConnectionError BAD msg --pure null --pure $Left e
    pure null

  export
  append : HasIO io=>MonadError DBError io=>String->(prev:HType)->(lt:HType)->io HType
  append item prev lt = do
     let new_item = tCons item prev lt --StrListT      
     storeHType new_item
     --  | Left x => pure $Left x    
     pure (new_item)

  write' : HasIO io=>MonadError DBError io => List String -> (prev:HType) ->(lt:HType)-> io TypePtr
  write' [] last lt = do  
    storeHType last
    --  | Left x => pure $Left $ EIO ("error writing: "++(ptr last))
    pure $ ptr last
    
  write' (x :: xs) prev lt= do
     newi <- DBList.append x prev lt
     --  | Left x => pure $Left $ EIO (show x)
       
     ret <- DBList.write' xs newi lt
     -- | Left x => pure $Left $ EIO ("error writing: "++(ptr newi))    
     pure  ret

  export
  write : HasIO io=>MonadError DBError io  => List String -> (lt:HType) -> io TypePtr
  write xs lt = do
        
    null <- DBList.new lt
    --   | Left x => pure $ Left $ x
    
    ret <- DBList.write' xs null lt
    --   | Left x => pure $ Left $ EIO (show x)
    pure ret

  export
  head : HasIO io=>MonadError DBError io  => TypePtr -> (lt:HType)->io (Maybe (String,TypePtr))
  head tp lt = do
    ht <- readHType tp
    --  | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)    
    case arg of
      ( (ACon "CONS")::(AVal x)::(APtr prev)::(APtr ltype)::[]  ) => do
         pure (Just (x,prev))
      ( (ACon "NIL")::(APtr ltype)::[] ) => pure Nothing
      _ => throwError EHashLink
  
  export
  read : HasIO io=>MonadError DBError io  => TypePtr -> (lt:HType)->io (List String)
  read tp lt = do  
    ht <- DBList.head tp lt
    --  | Left e => pure $ Left e
    case ht of
      Just (x,prev) => do
         ret_xs <- DBList.read prev lt
         --   | Left e => pure $ Left e       
         pure (x::ret_xs)       
      Nothing => pure []

  export
  readAsSnocList : HasIO io=>MonadError DBError io  => TypePtr -> (lt:HType)->io (SnocList String)
  readAsSnocList tp lt = do
    ht <- DBList.head tp lt
    --  | Left e => pure $ Left e
    case ht of
      Just (x,prev) => do
         ret_xs <- DBList.readAsSnocList prev lt
         --   | Left e => pure $ Left e       
         pure (ret_xs:<x)
         
      Nothing => pure [<]
  

namespace DBSnocList
  export
  new :HasIO io=>MonadError DBError io => (lt:HType)->io (HType)
  new lt = do
    let null = tLin lt --nullStrSnocListT
    storeHType null
    --  | Left y => pure null --$Left $ EIO (show y)
    pure null
  
  export
  append : HasIO io=>MonadError DBError io=>String->(prev:HType)->(lt:HType)->io HType
  append item prev lt = do
     let new_item = tSnoc item prev lt
     ok <- storeHType new_item
     --  | Left x => pure $Left $ EIO (show x)     
     pure new_item
  export
  head : HasIO io=>MonadError DBError io => TypePtr -> (lt:HType)->io (Maybe (String,TypePtr))
  head tp lt = do
    ht <- readHType tp
    --  | Left e => pure $ Left e
    let arg = (val ht)
    let ltype = (ptr lt)    
    case arg of
      ( (ACon "SNOC")::(APtr prev)::(AVal x)::(APtr ltype)::[]  ) => do
         pure (Just (x,prev))
      ( (ACon "LIN")::(APtr ltype)::[] ) => pure Nothing
      _ => throwError EHashLink --pure $ Left EHashLink
     
  write' : HasIO io=>MonadError DBError io => List String -> (prev:HType) ->(lt:HType)-> io TypePtr
  write' [] prev lt = do  
    x <- storeHType prev
    --  | Left x => pure $Left $ EIO ("error writing: "++(ptr prev))
    pure (ptr prev)  
    
  write' (x :: xs) prev lt= do
     new <- DBSnocList.append x prev lt
     --  | Left x => pure $Left x 
           
     ret <- DBSnocList.write' xs new lt
     -- | Left x => pure $Left x
     pure ret

  export
  write : HasIO io=>MonadError DBError io => List String -> (lt:HType) -> io TypePtr
  write xs lt = do
    null <- DBSnocList.new lt
    --   | Left x => pure $ Left $ x
    ret <- DBSnocList.write' xs null lt
    --   | Left x => pure $ Left $ EIO (show x)
    pure ret
    
  export
  read : HasIO io=>MonadError DBError io => TypePtr -> (lt:HType)->io (List String)
  read tp lt = do
    ht <- DBSnocList.head tp lt
    --  | Left e => pure $ Left e
    case ht of
      Just (x,prev) => do
         ret_xs <- DBSnocList.read prev lt
         --   | Left e => pure e       
         pure (x::ret_xs)       
      Nothing => pure []
            
  export
  readAsSnocList : HasIO io=>MonadError DBError io => TypePtr -> (lt:HType)->io ((SnocList String))
  readAsSnocList tp lt = do
    ht <- DBSnocList.head tp lt
    --  | Left e => pure $ Left e
    case ht of
      Just (x,prev) => do
         ret_xs <- DBSnocList.readAsSnocList prev lt
           -- | Left e => pure $ Left e       
         pure (ret_xs:<x)         
      Nothing => pure [<]
  
  toDBList': HasIO io=>MonadError DBError io => (snoc:TypePtr) -> (slt:HType)->(lt:HType)->(dst:HType) -> io TypePtr
  toDBList' snoc slt lt dst = do
    ht <- DBSnocList.head snoc slt
    --  | Left e => pure $ Left e
    case ht of
      Just (x,snocprev) => do
         ni <- DBList.append x dst lt
         --  | Left e => pure $ Left e
         ret <- DBSnocList.toDBList' snocprev slt lt ni
         --  | Left e => pure $ Left e
         pure ret
      Nothing => pure (ptr dst)
  export    
  toDBList : HasIO io=>MonadError DBError io => (snoc:TypePtr) -> (slt:HType)->(lt:HType) -> io ((Maybe TypePtr) )
  toDBList snoc slt lt = do
    ht <- DBSnocList.head snoc slt
    --  | Left e => pure $ Left e
    
    case ht of
      Just (x,prev) => do 

         null <- DBList.new lt
         --   | Left e => pure $ Left $ e             
         ret <- DBSnocList.toDBList' snoc slt lt null
         --   | Left e => pure $ Left $ e    
         pure (Just ret)
      
      Nothing => pure Nothing

namespace DBQueue
  export
  Name : Type
  Name = String

  export
  record FR where
    constructor MkFR
    f : TypePtr
    r : TypePtr
    lt : HType    
    slt : HType

  export
  new : HasIO io => MonadError DBError io => DBQueue.Name -> io (DBQueue.FR)
  new qn = do
     let slt = fromArg [AVar qn, toAPtr StrSnocListT]
         lt =  fromArg [AVar qn, toAPtr StrListT]
     new_f <- DBList.new lt
     --  | Left x => pure $Left x       
     new_r <- DBSnocList.new slt
     --  | Left x => pure $Left x    
     pure (MkFR (ptr new_f) (ptr new_r) lt slt)
  
  checkf : HasIO io=> MonadError DBError io=> DBQueue.FR -> io (DBQueue.FR) 
  checkf (MkFR f r lt slt) = do
      hx <- DBList.head f lt
      --  | Left y => pure $ Left y
      case hx of
         Nothing => do 
            new_f <- DBSnocList.toDBList r slt lt
            --  | Left e => pure$Left e              
            new_r <- DBSnocList.new slt
            --  | Left e => pure $Left e
            case new_f of 
              Just nf => pure (MkFR nf (ptr new_r) lt slt)    
              Nothing => pure (MkFR f r lt slt)
         Just h =>  pure (MkFR f r lt slt)
         
  export
  snoc : HasIO io=> MonadError DBError io=> DBQueue.FR -> String ->io DBQueue.FR
  snoc (MkFR f pr lt slt) item = do  
      r <- readHType pr 
      --  | Left e => pure $ Left e
      ht <- DBSnocList.append item r slt
      --  | Left e => pure $ Left e
      ret <- DBQueue.checkf (MkFR f (ptr ht) lt slt)
      pure ret
  
  export
  tail : HasIO io=> MonadError DBError io=> DBQueue.FR ->io DBQueue.FR 
  tail (MkFR pf pr lt slt) = do      
      hx <- DBList.head pf lt
      --   | Left y => pure $ Left y
      case hx of
         Just (x,p_xsf) => do
            ret <- DBQueue.checkf (MkFR p_xsf pr lt slt)
            pure ret             
         Nothing => do       
            ret <- DBQueue.checkf (MkFR pf pr lt slt)
            pure ret
  export
  head : HasIO io=> MonadError DBError io=> DBQueue.FR -> io (Maybe (String,TypePtr))
  head (MkFR f r lt slt) = (DBList.head f lt)
  export
  show : HasIO io=> MonadError DBError io=> DBQueue.FR -> io ()
  show (MkFR p_f p_r lt slt) = do
     printLn (show lt)
     retf <- DBList.read p_f lt 
     printLn retf
     printLn (show slt)
     retr <- DBSnocList.read p_r slt
     printLn retr
     pure ()
    
export
test_f : List String
test_f = [ (cast x) | x <- [1..5]]

export
test_r : List String
test_r = [ (cast x) | x<- [6..10]]    

export
db_list_test : HasIO io=> MonadError DBError io => io ()
db_list_test = do
  --printLn StrListT
  --let p_list = "0A769E8F51F42A4D935984EA4824CBCC7B969B724CA5E6DE6624FB5ABD97E65F"
  printLn "create front"  
  p_f <- DBList.write test_f StrListT
  --   | Left x => printLn "error writing list"
  --printLn p_t1  
  ret <- DBList.read p_f StrListT
  printLn ret


  printLn "create rear"
  p_r <- DBSnocList.write test_r StrSnocListT
  --   | Left x => printLn "error writing snoclist"
  printLn p_r
  
  printLn "rear printing"  
  ret <- DBSnocList.read p_r StrSnocListT
  printLn ret
  
  printLn "converting: .."
  p_r_cnv <- DBSnocList.toDBList p_r StrSnocListT StrListT
  --   | Left x => printLn (show x)
  printLn p_r_cnv
  
  printLn "print converted: .."
  
  case p_r_cnv of
    (Just px) => do
       ret <- DBList.read px StrListT
       printLn ret
    Nothing => pure ()
 

export
db_test_queue : HasIO io=> MonadError DBError io => io ()
db_test_queue = do
  db_list_test
  q1 <- DBQueue.new "test"   
  
  q1 <- DBQueue.snoc q1 "t3ocas" 
  --  | Left e => pure () 
  q1 <- DBQueue.snoc q1 "8ssa" 
  --  | Left e => pure () 
  q1 <- DBQueue.snoc q1 "ts" 
  --  | Left e => pure () 
  q1 <- DBQueue.snoc q1 "qq" 
  --  | Left e => pure ()     
    
  q1 <- DBQueue.tail q1
  --  | Left e => pure () 
  q1 <- DBQueue.tail q1
  --  | Left e => pure () 
    
  ret <- DBQueue.show q1  
  printLn ret    
  h <- DBQueue.head q1
  --  | Left e => pure ()   
  printLn h


export
db_main : IO ()
db_main = do
    Left err <- runEitherT (db_test_queue {io = EitherT DBError IO})
            | Right () => pure ()
    printLn err  
  --db_test_queue
  --let p_list = "0A769E8F51F42A4D935984EA4824CBCC7B969B724CA5E6DE6624FB5ABD97E65F"

