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
 
namespace DBList  
  export
  new : (lt:HType) -> HCommand (TypePtr) --HType
  new lt = do
    let null = tNil lt
    Store null
    Pure (ptr null)
 
  export
  append : String->(prev:HType)->(lt:HType)->HCommand HType
  append item prev lt = do
     let new_item = tCons item prev lt
     Store new_item
     Pure new_item
       
  write' : List String -> (prev:HType) ->(lt:HType)-> HCommand TypePtr
  write' [] last lt = do  
    Store last
    Pure $ ptr last        
  write' (x :: xs) prev lt= do
     newi <- DBList.append x prev lt
     ret <- DBList.write' xs newi lt
     Pure ret
     
  export
  write : List String -> (lt:HType) -> HCommand TypePtr
  write xs lt = do        
    p_null <- DBList.new lt
    null <- Read p_null
    ret <- DBList.write' xs null lt
    Pure ret
    
  export
  head : TypePtr -> (lt:HType)->HCommand (Maybe (String,TypePtr))
  head tp lt = do
    ht <- Read tp
    let arg = (val ht)
    let ltype = (ptr lt)    
    case arg of
      ( (ACon "CONS")::(AVal x)::(APtr prev)::(APtr ltype)::[]  ) => do
         Pure (Just (x,prev))
      ( (ACon "NIL")::(APtr ltype)::[] ) => Pure Nothing      
      _ => LinkError Nothing 
  
  export
  read : TypePtr -> (lt:HType)->HCommand (List String)
  read tp lt = do  
    ht <- DBList.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBList.read prev lt
         Pure (x::ret_xs)       
      Nothing => Pure []

  readAsSnocList : TypePtr -> (lt:HType)->HCommand (SnocList String)
  readAsSnocList tp lt = do
    ht <- DBList.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBList.readAsSnocList prev lt
         Pure (ret_xs:<x)
      Nothing => Pure [<]
      
namespace DBSnocList
  export
  new :(lt:HType)->HCommand (TypePtr) --TypePtr
  new lt = do
    let null = tLin lt
    Store null
    Pure (ptr null)  
  export
  append : String->(prev:HType)->(lt:HType)->HCommand HType
  append item prev lt = do
     let new_item = tSnoc item prev lt
     ok <- Store new_item
     Pure new_item
  export
  head : TypePtr -> (lt:HType)->HCommand (Maybe (String,TypePtr))
  head tp lt = do
    ht <- Read tp
    let ltype = (ptr lt)
    case (val ht) of
      ( (ACon "SNOC")::(APtr prev)::(AVal x)::(APtr ltype)::[]  ) => do
         Pure (Just (x,prev))
      ( (ACon "LIN")::(APtr ltype)::[] ) => Pure Nothing
      _ => LinkError Nothing
     
  write' : List String -> (prev:HType) ->(lt:HType)-> HCommand TypePtr
  write' [] prev lt = do  
    x <- Store prev
    Pure (ptr prev)      
  write' (x :: xs) prev lt= do
     new <- DBSnocList.append x prev lt
     ret <- DBSnocList.write' xs new lt
     Pure ret

  export
  write : List String -> (lt:HType) -> HCommand TypePtr
  write xs lt = do
    p_null <- DBSnocList.new lt
    null <- Read p_null
    ret <- DBSnocList.write' xs null lt
    Pure ret
    
  export
  read : TypePtr -> (lt:HType)->HCommand (List String)
  read tp lt = do
    ht <- DBSnocList.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBSnocList.read prev lt
         Pure (x::ret_xs)       
      Nothing => Pure []
            
  export
  readAsSnocList : TypePtr -> (lt:HType)->HCommand (SnocList String)
  readAsSnocList tp lt = do
    ht <- DBSnocList.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBSnocList.readAsSnocList prev lt
         Pure (ret_xs:<x)         
      Nothing => Pure [<]
  
  toDBList': (snoc:TypePtr) -> (slt:HType)->(lt:HType)->(dst:HType) -> HCommand TypePtr
  toDBList' snoc slt lt dst = do
    ht <- DBSnocList.head snoc slt
    case ht of
      Just (x,snocprev) => do
         ni <- DBList.append x dst lt
         ret <- DBSnocList.toDBList' snocprev slt lt ni
         Pure ret
      Nothing => Pure (ptr dst)
  export    
  toDBList : (snoc:TypePtr) -> (slt:HType)->(lt:HType) -> HCommand ((Maybe TypePtr) )
  toDBList snoc slt lt = do
    ht <- DBSnocList.head snoc slt    
    case ht of
      Just (x,prev) => do          
         p_null <- DBList.new lt
         null <- Read p_null
         ret <- DBSnocList.toDBList' snoc slt lt null
         Pure (Just ret)
      
      Nothing => Pure Nothing
 
namespace DBQueue
  export
  new : (qname:HType) -> HCommand (DBQueue.FR) --Types.DBQueue.Name 
  new qn = do
     slt <- fromArgCmd [toAPtr qn, toAPtr StrSnocListT]
     lt <-  fromArgCmd [toAPtr qn, toAPtr StrListT]
     
     new_f <- DBList.new lt
     new_r <- DBSnocList.new slt
     Pure (MkFR new_f new_r lt slt)
  
--  export
--  new2 : 
  
  checkf : DBQueue.FR -> HCommand (DBQueue.FR) 
  checkf (MkFR f r lt slt) = do
      hx <- DBList.head f lt
      case hx of
         Nothing => do 
            new_f <- DBSnocList.toDBList r slt lt
            new_r <- DBSnocList.new slt
            case new_f of 
              Just nf => Pure (MkFR nf ( new_r) lt slt)    
              Nothing => Pure (MkFR f r lt slt)
         Just h =>  Pure (MkFR f r lt slt)
         
  export
  snoc : DBQueue.FR -> String ->HCommand DBQueue.FR
  snoc (MkFR f pr lt slt) item = do  
      r <- Read pr 
      ht <- DBSnocList.append item r slt
      ret <- DBQueue.checkf (MkFR f (ptr ht) lt slt)
      Pure ret

  export
  tail : DBQueue.FR ->HCommand DBQueue.FR 
  tail (MkFR pf pr lt slt) = do      
      hx <- DBList.head pf lt
      case hx of
         Just (x,p_xsf) => do
            ret <- DBQueue.checkf (MkFR p_xsf pr lt slt)
            Pure ret             
         Nothing => do       
            ret <- DBQueue.checkf (MkFR pf pr lt slt)
            Pure ret
  export
  head : DBQueue.FR -> HCommand (Maybe (String,TypePtr))
  head (MkFR f r lt slt) = (DBList.head f lt)
  export
  show : DBQueue.FR -> HCommand ()
  show (MkFR p_f p_r lt slt) = do
     Log "lt"
     Show lt
     retf <- DBList.read p_f lt 
     Log "Front List"
     Show retf
     Log "slt"
     Show slt
     retr <- DBSnocList.read p_r slt
     Log "Rear SnocList"
     Show retr
     Pure ()
{-
namespace Observable
  export
  new : String -> HCommand Observable.Rec
  new on = do
     let lo = fromArg [AVar on, AVar "subscribers", toAPtr StrListT]
         lr =  fromArg [AVar on,AVar "removed",   toAPtr StrListT]
         --lr =  fromArg [AVar qn,AVar "inq",   toAPtr StrListT]         
     new_o <- DBList.new lo
     new_rm <- DBList.new lr
     new_inq <- DBQueue.new (on++"inq")
     new_cmd <- DBQueue.new (on++"cmd")              
     Pure (Observable.MkO new_o new_rm lo lr new_inq new_cmd)
-}     
  --new2 : String -> HCommand TypePtr
  --new2 

export
test_f : List String
test_f = [ (cast x) | x <- [1..5]]
export
test_r : List String
test_r = [ (cast x) | x<- [6..10]]    
export
db_list_test : HCommand (List String)
db_list_test = do
  p_f <- DBList.write test_f StrListT
  ret <- DBList.read p_f StrListT
  Show ret --  printLn ret
  Log "create rear"
  p_r <- DBSnocList.write test_r StrSnocListT
  Show p_r
  
  Log ("rear printing")
  ret <- DBSnocList.read p_r StrSnocListT
  Show ret
  
  Log ("converting: ..")
  p_r_cnv <- DBSnocList.toDBList p_r StrSnocListT StrListT
  Show p_r_cnv
  Log "print converted: .."
  
  case p_r_cnv of
    (Just px) => do
       ret <- DBList.read px StrListT
       Show ret
    Nothing => Pure ()
  Pure ret
  
export
db_test_queue : HCommand ()
db_test_queue = do

  qn <- fromName "test"   
  q1 <- DBQueue.new qn
  q1 <- DBQueue.snoc q1 "t3ocas" 
  q1 <- DBQueue.snoc q1 "8ssa" 
  q1 <- DBQueue.snoc q1 "ts" 
  q1 <- DBQueue.snoc q1 "qq" 
  q1 <- DBQueue.tail q1
  q1 <- DBQueue.tail q1
    
  ret <- DBQueue.show q1  
  Show ret    
  h <- DBQueue.head q1
  Show  h

-- IO part  
      
data_store_dir : String
data_store_dir = "/home/jan/github.com/mvect2/data/"
export
readHType : HasIO io=>MonadError DBError io => TypePtr -> io HType
readHType tp = do
  let pth = data_store_dir ++ tp
  Right cnt <- readFile pth
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
 

runHCommand : HasIO io=>MonadError DBError io => HCommand a -> io a --HCommand a -> IO a
runHCommand (Store x) = storeHType x --putStr (show x)
runHCommand (Read x)= readHType x --getLine
runHCommand (Log x )= printLn x
runHCommand (Show x) = printLn $ show x
runHCommand (LinkError x) = throwError EHashLink
runHCommand (Pure val) = pure val
runHCommand (Bind c f) = do res <- runHCommand c
                            runHCommand (f res)

export
db_runc : HasIO io => MonadError DBError io => io (List String)
db_runc = do
    runHCommand (db_test_queue >> db_list_test)
    

export
db_main : IO ()
db_main = do
    
    Right ret <- runEitherT (db_runc {io = EitherT DBError IO})
            | Left (err) => pure ()
    printLn ret 
    

{-
  export  
  L1 : (k:Type) -> Type
  L1 k = SOP I [[],[k,(L1 k)] ]
  export
  S1 : (k:Type) -> Type
  S1 k = SOP I [[],[(S1 k),k] ]
  export
  Q1 : (k:Type) -> Type
  Q1 k = NP I [(L1 k),(S1 k)]
  Mus : L1 Int
  Mus = MkSOP (Z [])
  Mu1 : (L1 Int)
  Mu1 = MkSOP (S $ Z [4,Mus])
  Mu2 : (L1 Int)
  Mu2 = MkSOP (S $ Z [1,Mu1])
  --N1 : (k:Type) -> Type
  --N1 k = Z k --NS I [k]
  --L1Int : L1 Int
  --L1Int = MkSOP ()
  --Q1 : (k:Type) -> Type
  --Q1 k = SOP I [[],[k,(Q1 k)] ]
-}
