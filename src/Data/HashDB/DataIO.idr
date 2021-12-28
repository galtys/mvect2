module Data.HashDB.DataIO

import Crypto.Hash.SHA256
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
 
namespace DBListStr  
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
     newi <- DBListStr.append x prev lt
     ret <- DBListStr.write' xs newi lt
     Pure ret
     
  export
  write : List String -> (lt:HType) -> HCommand TypePtr
  write xs lt = do        
    p_null <- DBListStr.new lt
    null <- Read p_null
    ret <- DBListStr.write' xs null lt
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
    ht <- DBListStr.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBListStr.read prev lt
         Pure (x::ret_xs)       
      Nothing => Pure []
  export
  readAsSnocList : TypePtr -> (lt:HType)->HCommand (SnocList String)
  readAsSnocList tp lt = do
    ht <- DBListStr.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBListStr.readAsSnocList prev lt
         Pure (ret_xs:<x)
      Nothing => Pure [<]

namespace DBList
  export
  append :(ToJSON vty)=>(FromJSON vty)=> vty->(prev:HType)->(lt:HType)->HCommand HType
  append vty prev lt = DBListStr.append (encode vty) prev lt
  export
  head : (FromJSON vty)=>TypePtr->(lt:HType)->HCommand (Maybe (vty,TypePtr))
  head tp lt = do
    Just (str_ret,p_ret) <- DBListStr.head tp lt
      | Nothing => Pure Nothing
    case (decode str_ret) of
      Left err => LinkError Nothing
      Right x => Pure $ Just (x,p_ret)
  export
  write : (ToJSON vty)=>List vty->(lt:HType)->HCommand TypePtr
  write xs lt = do
     let str_list = map encode xs
     ret <- DBListStr.write str_list lt
     Pure ret
  export
  read : (FromJSON vty)=>TypePtr -> (lt:HType)->HCommand (Maybe (List vty))
  read tp lt = do
     ret <- DBListStr.read tp lt
     let decodeOne : String -> (Maybe vty)
         decodeOne str = do
            case (decode str) of 
              Left err => Nothing
              Right x => Just x
         retxs : (Maybe (List vty))
         retxs = traverse decodeOne ret
     case retxs of 
       Nothing => DecodeError Nothing
       (Just xs) => Pure $Just xs
  export
  readAsSnocList : (FromJSON vty)=>TypePtr -> (lt:HType)->HCommand (Maybe (SnocList vty))
  readAsSnocList tp lt = do
     ret <- DBListStr.readAsSnocList tp lt
     let decodeOne : String -> (Maybe vty)
         decodeOne str = do
            case (decode str) of 
              Left err => Nothing
              Right x => Just x
         retxs : (Maybe (SnocList vty))
         retxs = traverse decodeOne ret
     case retxs of 
       Nothing => DecodeError Nothing
       (Just xs) => Pure $Just xs

namespace DBSnocListStr
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
     new <- DBSnocListStr.append x prev lt
     ret <- DBSnocListStr.write' xs new lt
     Pure ret

  export
  write : List String -> (lt:HType) -> HCommand TypePtr
  write xs lt = do
    p_null <- DBSnocListStr.new lt
    null <- Read p_null
    ret <- DBSnocListStr.write' xs null lt
    Pure ret
    
  export
  read : TypePtr -> (lt:HType)->HCommand (List String)
  read tp lt = do
    ht <- DBSnocListStr.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBSnocListStr.read prev lt
         Pure (x::ret_xs)       
      Nothing => Pure []
            
  export
  readAsSnocList : TypePtr -> (lt:HType)->HCommand (SnocList String)
  readAsSnocList tp lt = do
    ht <- DBSnocListStr.head tp lt
    case ht of
      Just (x,prev) => do
         ret_xs <- DBSnocListStr.readAsSnocList prev lt
         Pure (ret_xs:<x)         
      Nothing => Pure [<]
  
  toDBListStr': (snoc:TypePtr) -> (slt:HType)->(lt:HType)->(dst:HType) -> HCommand TypePtr
  toDBListStr' snoc slt lt dst = do
    ht <- DBSnocListStr.head snoc slt
    case ht of
      Just (x,snocprev) => do
         ni <- DBListStr.append x dst lt
         ret <- DBSnocListStr.toDBListStr' snocprev slt lt ni
         Pure ret
      Nothing => Pure (ptr dst)
  export    
  toDBListStr : (snoc:TypePtr) -> (slt:HType)->(lt:HType) -> HCommand ((Maybe TypePtr) )
  toDBListStr snoc slt lt = do
    ht <- DBSnocListStr.head snoc slt    
    case ht of
      Just (x,prev) => do          
         p_null <- DBListStr.new lt
         null <- Read p_null
         ret <- DBSnocListStr.toDBListStr' snoc slt lt null
         Pure (Just ret)
      
      Nothing => Pure Nothing
--namespace DBSnocList

namespace DBQueueStr--DBQueueStr
  export
  new : (qname:HType) -> HCommand (DBQueueStr.FR) --Types.DBQueueStr.Name 
  new qn = do
     slt <- fromArgCmd [toAPtr qn, toAPtr StrSnocListT]
     lt <-  fromArgCmd [toAPtr qn, toAPtr StrListT]
     
     new_f <- DBListStr.new lt
     new_r <- DBSnocListStr.new slt
     Pure (MkFR new_f new_r lt slt)
  
--  export
--  new2 : 
  
  checkf : DBQueueStr.FR -> HCommand (DBQueueStr.FR) 
  checkf (MkFR f r lt slt) = do
      hx <- DBListStr.head f lt
      case hx of
         Nothing => do 
            new_f <- DBSnocListStr.toDBListStr r slt lt
            new_r <- DBSnocListStr.new slt
            case new_f of 
              Just nf => Pure (MkFR nf ( new_r) lt slt)    
              Nothing => Pure (MkFR f r lt slt)
         Just h =>  Pure (MkFR f r lt slt)
         
  export
  snoc : DBQueueStr.FR -> String ->HCommand DBQueueStr.FR
  snoc (MkFR f pr lt slt) item = do  
      r <- Read pr 
      ht <- DBSnocListStr.append item r slt
      ret <- DBQueueStr.checkf (MkFR f (ptr ht) lt slt)
      Pure ret

  export
  tail : DBQueueStr.FR ->HCommand DBQueueStr.FR 
  tail (MkFR pf pr lt slt) = do      
      hx <- DBListStr.head pf lt
      case hx of
         Just (x,p_xsf) => do
            ret <- DBQueueStr.checkf (MkFR p_xsf pr lt slt)
            Pure ret             
         Nothing => do       
            ret <- DBQueueStr.checkf (MkFR pf pr lt slt)
            Pure ret
  export
  head : DBQueueStr.FR -> HCommand (Maybe (String,TypePtr))
  head (MkFR f r lt slt) = (DBListStr.head f lt)
  export
  show : DBQueueStr.FR -> HCommand ()
  show (MkFR p_f p_r lt slt) = do
     Log "lt"
     Show lt
     retf <- DBListStr.read p_f lt 
     Log "Front List"
     Show retf
     Log "slt"
     Show slt
     retr <- DBSnocListStr.read p_r slt
     Log "Rear SnocList"
     Show retr
     Pure ()
     
namespace DBQueue
    export
    snoc :(ToJSON vty)=>DBQueueStr.FR -> vty->HCommand DBQueueStr.FR
    snoc q item = DBQueueStr.snoc q (encode item)
    export
    head :(FromJSON vty)=>DBQueueStr.FR -> HCommand (Maybe (vty,TypePtr))
    head q = do
       Just (s_ret,p_ret)<-DBQueueStr.head q
         | Nothing => Pure Nothing
       case (decode s_ret) of
         Left er => DecodeError Nothing
         Right rs => Pure $ Just (rs,p_ret)
{-
namespace Observable
  export
  new : String -> HCommand Observable.Rec
  new on = do
     let lo = fromArg [AVar on, AVar "subscribers", toAPtr StrListT]
         lr =  fromArg [AVar on,AVar "removed",   toAPtr StrListT]
         --lr =  fromArg [AVar qn,AVar "inq",   toAPtr StrListT]         
     new_o <- DBListStr.new lo
     new_rm <- DBListStr.new lr
     new_inq <- DBQueueStr.new (on++"inq")
     new_cmd <- DBQueueStr.new (on++"cmd")              
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
  p_f <- DBListStr.write test_f StrListT
  ret <- DBListStr.read p_f StrListT
  Show ret --  printLn ret
  Log "create rear"
  p_r <- DBSnocListStr.write test_r StrSnocListT
  Show p_r
  
  Log ("rear printing")
  ret <- DBSnocListStr.read p_r StrSnocListT
  Show ret
  
  Log ("converting: ..")
  p_r_cnv <- DBSnocListStr.toDBListStr p_r StrSnocListT StrListT
  Show p_r_cnv
  Log "print converted: .."
  
  case p_r_cnv of
    (Just px) => do
       ret <- DBListStr.read px StrListT
       Show ret
    Nothing => Pure ()
  Pure ret
  
export
db_test_queue : HCommand ()
db_test_queue = do
  qn <- fromName "test"   
  q1 <- DBQueueStr.new qn
  q1 <- DBQueueStr.snoc q1 "t3ocas" 
  q1 <- DBQueueStr.snoc q1 "8ssa" 
  q1 <- DBQueueStr.snoc q1 "ts" 
  q1 <- DBQueueStr.snoc q1 "qq" 
  q1 <- DBQueueStr.tail q1
  q1 <- DBQueueStr.tail q1
    
  ret <- DBQueueStr.show q1  
  Show ret    
  h <- DBQueueStr.head q1
  Show  h
  
export
db_test_queue2 : HCommand ()
db_test_queue2 = do
  qn <- fromName "test2"   
  q1 <- DBQueueStr.new qn
  q1 <- DBQueueStr.snoc q1 "t3ocas" 
  q1 <- DBQueueStr.snoc q1 "8ssa" 
  q1 <- DBQueueStr.snoc q1 "ts" 
  q1 <- DBQueueStr.snoc q1 "qq" 
  q1 <- DBQueueStr.tail q1
  q1 <- DBQueueStr.tail q1
    
  ret <- DBQueueStr.show q1  
  Show ret    
  h <- DBQueueStr.head q1
  Show  h

-- IO part  
namespace DirectoryMap
  export
  insert : ToJSON k=>ToJSON v=>HasIO io=>MonadError DBError io =>k->v->String->io String
  insert k v dir = do
     let k_e : String
         k_e = encode k
         cnt : String
         cnt = encode v
         kh : String
         kh = sha256 k_e
         pth:String
         pth = dir ++ kh
     Right ret <- writeFile pth cnt
       | Left e => throwError (EIO $show e)
     pure dir
  export
  lookup : ToJSON k=>FromJSON v=>HasIO io=>MonadError DBError io =>k->String->io v
  lookup k dir = do
     let k_e : String
         k_e = encode k
         kh : String
         kh = sha256 k_e
         pth:String
         pth = dir ++ kh
     Right cnt <- readFile pth
       | Left e => throwError (EIO $show e)
     case (decode cnt) of
       Left e => throwError (ErrorJS $show e)
       Right arg => pure arg
  export
  remove : ToJSON k=>HasIO io=>MonadError DBError io =>k->String->io ()
  remove k dir = do
     let k_e : String
         k_e = encode k
         kh : String
         kh = sha256 k_e
         pth:String
         pth = dir ++ kh
     Right x <- removeFile pth
        | Left e => throwError (EIO $show e)
     pure x

export
readHType : HasIO io=>MonadError DBError io => TypePtr -> String -> io HType
readHType tp dir = do
  let pth = dir ++ tp
  Right cnt <- readFile pth
    | Left e => throwError (EIO $show e)
  case (decode cnt) of
    Left e => throwError (ErrorJS $show e)
    Right arg => pure (MkHT arg tp)

export
storeHType: HasIO io=>MonadError DBError io => HType -> String -> io ()
storeHType ht dir= do 
 let pth = dir ++ (ptr ht)
     cnt = (encode $ val ht)
 Right ret <- writeFile pth cnt
     | Left e => throwError (EIO $show e)
 pure ()


runHCommand : HasIO io=>MonadError DBError io => HCommand a -> String->io a
runHCommand (Store x) dir = storeHType x dir 
runHCommand (Read x) dir = readHType x dir 
runHCommand (Log x ) dir= printLn x
runHCommand (Show x) dir = printLn $ show x
runHCommand (LinkError x) dir = throwError EHashLink
runHCommand (DecodeError x) dir= throwError (ErrorJS "Error while decoding JS")
runHCommand (Pure val) dir = pure val
runHCommand (Bind c f) dir = do 
                    res <- runHCommand c dir
                    runHCommand (f res) dir

export
db_runc : HasIO io => MonadError DBError io => io (List String)
db_runc = do
    let data_store_dir:String
        data_store_dir="/home/jan/github.com/mvect2/data/hcmd"
    runHCommand (db_test_queue >> db_list_test) data_store_dir

export
db_main : IO ()
db_main = do
    Right ret <- runEitherT (db_runc {io = EitherT DBError IO})
            | Left (err) => pure ()
    printLn ret 
    
