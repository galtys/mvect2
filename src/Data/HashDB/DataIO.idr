module Data.HashDB.DataIO

import Data.HashDB.Types
import System.Directory
import System.File.ReadWrite
import Data.SnocList

import Control.Monad.Either
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
 cnt <- readFile pth
 pure cnt

export
readHType : HasIO io=>MonadError DBError io => TypePtr -> io HType
readHType tp = do
  Right cnt <- readHcnt tp
    | Left e => throwError (EIO $show e)
  case (decode cnt) of
    Left e => throwError (EIO $show e)
    Right arg => pure (MkHT arg tp)

export
storeHType: HasIO io=>MonadError DBError io => HType -> io ()
storeHType ht = do
 let pth = data_store_dir ++ (ptr ht)
     cnt = (encode $ val ht)
 Right ret <- writeFile pth cnt
     | Left e => throwError (EIO $show e)
 pure ()

namespace DBList
  export
  new :HasIO io=>MonadError DBError io =>(lt:HType)->io ( HType )
  new lt = do
    let null = tNil lt
    storeHType null
    pure null

  export
  append : HasIO io=>MonadError DBError io=>String->(prev:HType)->(lt:HType)->io HType
  append item prev lt = do
     let new_item = tCons item prev lt
     storeHType new_item
     pure (new_item)

  write' : HasIO io=>MonadError DBError io => List String -> (prev:HType) ->(lt:HType)-> io TypePtr
  write' [] last lt = do  
    storeHType last
    pure $ ptr last
    
  write' (x :: xs) prev lt= do
     newi <- DBList.append x prev lt
     ret <- DBList.write' xs newi lt
     pure  ret

  export
  write : HasIO io=>MonadError DBError io  => List String -> (lt:HType) -> io TypePtr
  write xs lt = do        
    null <- DBList.new lt
    ret <- DBList.write' xs null lt
    pure ret

  export
  head : HasIO io=>MonadError DBError io  => TypePtr -> (lt:HType)->io (Maybe (String,TypePtr))
  head tp lt = do
    ht <- readHType tp
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
    case ht of
      Just (x,prev) => do
         ret_xs <- DBList.read prev lt
         pure (x::ret_xs)       
      Nothing => pure []

  export
  readAsSnocList : HasIO io=>MonadError DBError io  => TypePtr -> (lt:HType)->io (SnocList String)
  readAsSnocList tp lt = do
    ht <- DBList.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBList.readAsSnocList prev lt
         pure (ret_xs:<x)
      Nothing => pure [<]
  

namespace DBSnocList
  export
  new :HasIO io=>MonadError DBError io => (lt:HType)->io (HType)
  new lt = do
    let null = tLin lt
    storeHType null
    pure null
  
  export
  append : HasIO io=>MonadError DBError io=>String->(prev:HType)->(lt:HType)->io HType
  append item prev lt = do
     let new_item = tSnoc item prev lt
     ok <- storeHType new_item
     pure new_item
  export
  head : HasIO io=>MonadError DBError io => TypePtr -> (lt:HType)->io (Maybe (String,TypePtr))
  head tp lt = do
    ht <- readHType tp
    let arg = (val ht)
    let ltype = (ptr lt)    
    case arg of
      ( (ACon "SNOC")::(APtr prev)::(AVal x)::(APtr ltype)::[]  ) => do
         pure (Just (x,prev))
      ( (ACon "LIN")::(APtr ltype)::[] ) => pure Nothing
      _ => throwError EHashLink
     
  write' : HasIO io=>MonadError DBError io => List String -> (prev:HType) ->(lt:HType)-> io TypePtr
  write' [] prev lt = do  
    x <- storeHType prev
    pure (ptr prev)  
    
  write' (x :: xs) prev lt= do
     new <- DBSnocList.append x prev lt
     ret <- DBSnocList.write' xs new lt
     pure ret

  export
  write : HasIO io=>MonadError DBError io => List String -> (lt:HType) -> io TypePtr
  write xs lt = do
    null <- DBSnocList.new lt
    ret <- DBSnocList.write' xs null lt
    pure ret
    
  export
  read : HasIO io=>MonadError DBError io => TypePtr -> (lt:HType)->io (List String)
  read tp lt = do
    ht <- DBSnocList.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBSnocList.read prev lt
         pure (x::ret_xs)       
      Nothing => pure []
            
  export
  readAsSnocList : HasIO io=>MonadError DBError io => TypePtr -> (lt:HType)->io ((SnocList String))
  readAsSnocList tp lt = do
    ht <- DBSnocList.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBSnocList.readAsSnocList prev lt
         pure (ret_xs:<x)         
      Nothing => pure [<]
  
  toDBList': HasIO io=>MonadError DBError io => (snoc:TypePtr) -> (slt:HType)->(lt:HType)->(dst:HType) -> io TypePtr
  toDBList' snoc slt lt dst = do
    ht <- DBSnocList.head snoc slt
    case ht of
      Just (x,snocprev) => do
         ni <- DBList.append x dst lt
         ret <- DBSnocList.toDBList' snocprev slt lt ni
         pure ret
      Nothing => pure (ptr dst)
  export    
  toDBList : HasIO io=>MonadError DBError io => (snoc:TypePtr) -> (slt:HType)->(lt:HType) -> io ((Maybe TypePtr) )
  toDBList snoc slt lt = do
    ht <- DBSnocList.head snoc slt    
    case ht of
      Just (x,prev) => do 
         null <- DBList.new lt
         ret <- DBSnocList.toDBList' snoc slt lt null
         pure (Just ret)
      
      Nothing => pure Nothing

namespace DBQueue
  export
  new : HasIO io => MonadError DBError io => Types.DBQueue.Name -> io (DBQueue.FR)
  new qn = do
     let slt = fromArg [AVar qn, toAPtr StrSnocListT]
         lt =  fromArg [AVar qn, toAPtr StrListT]
     new_f <- DBList.new lt
     new_r <- DBSnocList.new slt
     pure (MkFR (ptr new_f) (ptr new_r) lt slt)
  
  checkf : HasIO io=> MonadError DBError io=> DBQueue.FR -> io (DBQueue.FR) 
  checkf (MkFR f r lt slt) = do
      hx <- DBList.head f lt
      case hx of
         Nothing => do 
            new_f <- DBSnocList.toDBList r slt lt
            new_r <- DBSnocList.new slt
            case new_f of 
              Just nf => pure (MkFR nf (ptr new_r) lt slt)    
              Nothing => pure (MkFR f r lt slt)
         Just h =>  pure (MkFR f r lt slt)
         
  export
  snoc : HasIO io=> MonadError DBError io=> DBQueue.FR -> String ->io DBQueue.FR
  snoc (MkFR f pr lt slt) item = do  
      r <- readHType pr 
      ht <- DBSnocList.append item r slt
      ret <- DBQueue.checkf (MkFR f (ptr ht) lt slt)
      pure ret
  
  export
  tail : HasIO io=> MonadError DBError io=> DBQueue.FR ->io DBQueue.FR 
  tail (MkFR pf pr lt slt) = do      
      hx <- DBList.head pf lt
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

namespace Observable
  export
  new : HasIO io => MonadError DBError io => String -> io (Observable.Rec)
  new on = do
     let lo = fromArg [AVar on, AVar "subscribers", toAPtr StrListT]
         lr =  fromArg [AVar on,AVar "removed",   toAPtr StrListT]
         --lr =  fromArg [AVar qn,AVar "inq",   toAPtr StrListT]         
     new_o <- DBList.new lo
     new_rm <- DBList.new lr
     new_inq <- DBQueue.new (on++"inq")
     new_cmd <- DBQueue.new (on++"cmd")              
     pure (Observable.MkO (ptr new_o) (ptr new_rm) lo lr new_inq new_cmd)
  
  
    
export
test_f : List String
test_f = [ (cast x) | x <- [1..5]]

export
test_r : List String
test_r = [ (cast x) | x<- [6..10]]    

export
db_list_test : HasIO io=> MonadError DBError io => io ()
db_list_test = do
  --let p_list = "0A769E8F51F42A4D935984EA4824CBCC7B969B724CA5E6DE6624FB5ABD97E65F"
  printLn "create front"  
  p_f <- DBList.write test_f StrListT
  ret <- DBList.read p_f StrListT
  printLn ret

  printLn "create rear"
  p_r <- DBSnocList.write test_r StrSnocListT
  printLn p_r
  
  printLn "rear printing"  
  ret <- DBSnocList.read p_r StrSnocListT
  printLn ret
  
  printLn "converting: .."
  p_r_cnv <- DBSnocList.toDBList p_r StrSnocListT StrListT
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
  q1 <- DBQueue.snoc q1 "8ssa" 
  q1 <- DBQueue.snoc q1 "ts" 
  q1 <- DBQueue.snoc q1 "qq" 
  q1 <- DBQueue.tail q1
  q1 <- DBQueue.tail q1
    
  ret <- DBQueue.show q1  
  printLn ret    
  h <- DBQueue.head q1
  printLn h

export
db_main : IO ()
db_main = do
    Left err <- runEitherT (db_test_queue {io = EitherT DBError IO})
            | Right () => pure ()
    printLn err  
