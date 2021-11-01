module Test1

import Data.IORef
import System.Directory

import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Hom
import Category.Transaction.Journal
import Category.Transaction.Demo
import Category.Transaction.Types
import Data.Ratio

import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Language.Reflection.Types

import System.FFI
import JSON

import Generics.Derive
import JSON

import Control.Monad.Either

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

%language ElabReflection

data RunIO : Type -> Type where
     Quit : a -> RunIO a
     Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b
     Seq : IO () -> Inf (RunIO b) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

(>>) : IO () -> Inf (RunIO b) -> RunIO b
(>>) = Seq

{-
data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run (More fuel) (Seq io k) = do io; run fuel k
run Dry p = pure Nothing


partial
forever : Fuel
forever = More forever
-}

json_result : String
json_result = "{\"result\": 332}"

WEB_ROOT : String
WEB_ROOT = "/home/jan/github.com/websocket-examples/jsClient"


x_my_http_handler : HasIO io => Ptr MG_CONNECTION -> MG_EVENT_TYPE -> Ptr EV_DATA -> Ptr FN_DATA -> io ()
x_my_http_handler p_conn MG_EV_HTTP_MSG p_ev p_fn = do
                    let hm = (ev_to_http_message p_ev)                    
                    --putStrLn ("HTTP is null: " ++ (show (is_ptr_null p_fn)))
                    putStrLn ("HTTP val: " ++ (show (get_p_int p_fn )))                    
                    
                    if (mg_http_match_uri hm "/rest")==1 then do
                           mg_http_reply p_conn 200 "Content-Type: application/json\r\n" json_result
                           set_p_int p_fn 100
                           
                       else if (mg_http_match_uri hm "/websocket")==1 then do
                           mg_ws_upgrade p_conn p_ev get_pchar_NULL  
                         else do
                             p_opts <- (get_and_malloc__mg_http_serve_opts  WEB_ROOT)
                             mg_http_serve_dir p_conn hm p_opts 
x_my_http_handler p_conn MG_EV_ACCEPT p_ev p_fn = do
                    --putStrLn ("MG_EV_ACCEPT: " ++ (show (is_ptr_null p_fn)))
                    putStrLn ("EV acceptp_fn  val: " ++ (show (get_p_int p_fn)))
                    --x <- malloc_pint                     
                    --set_p_int x 10                    
                    --set_fn_data p_conn x
                    pure ()
                    
x_my_http_handler p_conn MG_EV_WS_MSG p_ev p_fn = do
                    putStrLn ("EV WS  val: " ++ (show (get_p_int p_fn)))
                    let p_wm = (ev_to_ws_message p_ev)
                    msg <- mg_ws_receive_as_String p_conn p_wm                 
                    mg_ws_send_text p_conn msg                    
x_my_http_handler p_conn ev p_ev p_fn = do 
                  pure ()

my_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn (fromBits8 ev) p_ev p_fn)

{-
greet : RunIO ()
greet = do putStr "Enter your name: "
           name <- getLine
           if name == ""
              then do putStrLn "Bye bye!"
                      Quit ()
              else do putStrLn ("Hello " ++ name)
                      greet

inf_loop2 : (Ptr MG_MGR) -> Int -> RunIO ()
inf_loop2 p_mgr time_out = do
  mg_mgr_poll p_mgr time_out
  inf_loop2 p_mgr time_out
-}

partial
inf_loop : (Ptr MG_MGR) -> Int -> IO ()
inf_loop p_mgr time_out = do
  mg_mgr_poll p_mgr time_out
  inf_loop p_mgr time_out


fn_data_ref : HasIO io => io (IORef Country)
fn_data_ref = newIORef UK

data_store_dir : String
data_store_dir = "/home/jan/github.com/mvect2/data"


--------------------------------------------------------------------------------
--          Product and Bom
--------------------------------------------------------------------------------

Id : Column
Id = primarySerial64 Bits32 "id" (Just . cast)

Name : Column
Name = notNull String "name" Text Just id

SKU  : Column
SKU = notNull String "default_code" Text Just id

Product : Table
Product = MkTable "product_product"
         [Id, SKU]


ProdQty : Column
ProdQty = notNull Bits32 "product_qty" BigInt (Just . cast) cast

ProductID : Column
ProductID = notNull Bits32 "product_id" BigInt (Just . cast) cast

BomID : Column
BomID = nullable Bits32 "bom_id" BigInt (Just . cast) cast

BoM_NP : Table
BoM_NP = MkTable "mrp_bom"
      [Id,ProductID,ProdQty,BomID]
      
record RBoM where
  constructor MkRBoM
  product_id : Bits32
  product_qty : Bits32
  bom_id : (Maybe Bits32)
  id_ : Bits32
        
%runElab derive "RBoM" [Generic, Meta, Show, Eq]

--IdTable : Table
--IdTable = MkTable "id_col"
--          [BomID]


main_ : HasIO io => MonadError SQLError io => io ()
main_ = do
  c    <- connect "postgresql://jan@localhost:5432/pjb-2021-10-27_1238"
  --createAndFill c
  --rows <- get c Product (columns Product) (SKU /= "Eleni")
  --traverse_ printLn rows
  
  rows <- get c BoM_NP (columns BoM_NP) (True)
  traverse_ printLn rows
  
  finish c

toRBoM : (NP I [Bits32, Bits32,Maybe Bits32,Bits32] ) -> RBoM
toRBoM x = MkRBoM (get Bits32 x)  (get Bits32 (tl x)) (get (Maybe Bits32) (tl (tl x) ) )     (get (Bits32) (tl (tl (tl x)) ) )


read_bom2 : HasIO io => MonadError SQLError io => Connection -> List RBoM -> io (List BoM32)
read_bom2 c [] = pure [] --pure (Node32 0 0 [])
read_bom2 c (x::xs) = do 
        case (bom_id x) of
          Nothing => do
              --printLn "ocas4"
              rows <- get c BoM_NP [ProductID,ProdQty,BomID,Id] (BomID == (Just (cast (id_ x) )) )
              let child = [ (toRBoM ox) | ox <- rows ]
              muf <- read_bom2 c child
              pure [Node32 (product_qty x) (product_id x) muf]
              
          Just b_id => do
              --rows <- get c BoM_NP [ProductID,ProdQty,BomID,Id] (BomID == (Just (cast b_id)) )                
              --let child = [ (toRBoM ox) | ox <- rows ]
              xu <- read_bom2 c xs
              let ret =Node32 (product_qty x) (product_id x) [] --xu
              pure ([ret]++xu)

mutual
  read_bom3 : HasIO io => MonadError SQLError io => Connection -> List RBoM -> io (List BoM32)
  read_bom3 c [] = pure []
  read_bom3 c (x::xs) = do 
              xu <- read_bom3 c xs
              
              rows <- get c BoM_NP [ProductID,ProdQty,BomID,Id] (BomID == (Just (cast (id_ x) )) )
              let child = [ product_id (toRBoM ox) | ox <- rows ]
              
              ch <- read_bom_p_id c child
                            
              let ret =Node32 (product_qty x) (product_id x) ch --xu
              pure ([ret])

  read_bom_p_id : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> io (List BoM32) 
  read_bom_p_id c [] = pure []
  read_bom_p_id c (p_id::xs) = do
    rows <- get c BoM_NP [ProductID,ProdQty,BomID,Id] (ProductID == (cast p_id) )  
    ret <- read_bom3 c [ toRBoM ox | ox <- rows ]
    xu <- read_bom_p_id c xs
    pure (ret++xu)

main_read_bom : HasIO io => MonadError SQLError io => Bits32 -> io ()
main_read_bom p_id = do
  c    <- connect "postgresql://jan@localhost:5432/pjb-2021-10-27_1238"  
  --rows <- get c BoM_NP [Id] (IsNull BomID)  
  
  boms <- read_bom_p_id c [p_id]
  printLn boms
  
    --printLn ocas
  --traverse_ printLn rows

  finish c
  
{-
main_2 : HasIO io => MonadError SQLError io => io ()
main_2 = do
  c    <- connect "postgresql://jan@localhost:5432/pjb-2021-10-27_1238"  
  
  --rows <- get c BoM_NP [Id] (IsNull BomID)
  rows <- get c BoM_NP [Id,BomID] (BomID == (Just 1633) )  
  printLn rows
  --traverse_ printLn rows

  finish c
-}
main_3 : IO ()
main_3 = do Left err <- runEitherT (main_read_bom {io = EitherT SQLError IO} 3303)
              | Right () => pure ()
            printLn err


{-
main_pg : IO ()
main_pg = do Left err <- runEitherT (main_2 {io = EitherT SQLError IO})
              | Right () => pure ()
             printLn err
-}
main : IO ()
main = do
  --main_pg
  main_3
--  read_bom 44
  --ignore $ run forever greet
  
  --c_ref <- fn_data_ref
  --c <- readIORef c_ref
  --putStrLn (show c)
  Right d <- listDir data_store_dir 
    | Left x => printLn ("Directory does not exist:"++data_store_dir)
  
  printLn d
  
  mg_log_set "3"
  p_mgr <- get_and_malloc__mg_mgr
  mg_mgr_init p_mgr 

  x1 <- malloc_pint                     
  set_p_int x1 99                    
      
  mg_http_listen p_mgr "0.0.0.0:8080" my_http_handler x1
  --inf_loop p_mgr 1000
  mg_mgr_free p_mgr 
  
  --test_demo
  --printLn ( (get_hom1 so1_lt1 ))  
  --printLn (mufum (get_hom1 so1_lt1 ))

  
{-  
  p_mgr <- get_and_malloc__mg_mgr
  mg_mgr_init p_mgr 
  
  --mg_http_listen p_mgr "0.0.0.0:8000" prim__fn_http_handler p_mgr
  mg_http_listen p_mgr "0.0.0.0:8080" my_http_handler p_mgr
  
  inf_loop p_mgr 1000
  mg_mgr_free p_mgr 
-}
