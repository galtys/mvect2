module Test1

import Data.IORef
import System.Directory

import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256

import Data.SortedMap

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Hom
import Category.Transaction.Journal
import Category.Transaction.Demo
import Category.Transaction.Types
import Data.Ratio
--import Data.Zippable

import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Language.Reflection.Types

import PQ.Schema
--import System.FFI
import JSON

import Generics.Derive
import JSON

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

import Ledger.PG.BoM
import Ledger.PG.Order

import Ledger.Schema.Types
import Ledger.Schema.Order

import Odoo.Schema.PJB


{-
import Control.Monad.Either

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types
-}

%language ElabReflection
%ambiguity_depth 10

{-
data RunIO : Type -> Type where
     Quit : a -> RunIO a
     Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b
     Seq : IO () -> Inf (RunIO b) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

(>>) : IO () -> Inf (RunIO b) -> RunIO b
(>>) = Seq

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
                    l1 <- muf_3
                    putStrLn ("EV WS  val: " ++ (show (get_p_int p_fn)))
                    let p_wm = (ev_to_ws_message p_ev)
                    msg <- mg_ws_receive_as_String p_conn p_wm                 
                    mg_ws_send_text p_conn msg                    
x_my_http_handler p_conn ev p_ev p_fn = do 
                  pure ()

my_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn (fromBits8 ev) p_ev p_fn)


partial
inf_loop : (Ptr MG_MGR) -> Int -> IO ()
inf_loop p_mgr time_out = do
  mg_mgr_poll p_mgr time_out
  inf_loop p_mgr time_out


fn_data_ref : HasIO io => io (IORef Country)
fn_data_ref = newIORef UK

data_store_dir : String
data_store_dir = "/home/jan/github.com/mvect2/data"

gen_adder : Int -> (Int ->Int)
gen_adder x = (\a => a+x)

so_id_44575 : Bits32
so_id_44575 = 44575

--Cast Bits32 (DBType (pqType Id_OT)) where
--  cast = ?mufs



main : IO ()
main = do

  --l1 <- muf_3
  --l1 <- SO_Simple.read (Id_OT == (cast so_id_44575)) --no implementation
  --l1 <- SO_Simple.read (NameOT == (cast "SO44512"))
  test_main_x
  
  {-
  l1 <- PrimOrder.read (True)
      
  traverse_ printLn l1
  printLn (length l1)
  -}
  
  
  --l2 <- SOL_Simple.read (True)
  --traverse_ printLn l2
  --printLn (length l2)  
  
--  read_bom 44
  --ignore $ run forever greet
  
  --c_ref <- fn_data_ref
  --c <- readIORef c_ref
  --putStrLn (show c)
  
  {-
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
  -}
  
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
