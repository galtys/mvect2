module Test1


import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256

import Data.SortedMap

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Hom
import Category.Transaction.Journal
import Category.Transaction.Types
import Category.Transaction.Demo
import Category.Transaction.Demo2
import Data.Ratio
--import Data.Zippable

import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Language.Reflection.Types

import Data.HashDB.Types
import Data.HashDB.DataIO

import PQ.Schema
--import System.FFI
import JSON

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

import Odoo.PG.BoM
import Odoo.PG.Order

import Category.Schema.Types
import Category.Schema.PJB

import Odoo.Schema.PJBRecDef
import Odoo.Schema.PJB

import Core.Context
import System.FFI
import Libc.Time
--import System

%ambiguity_depth 10

json_result : String
json_result = "{\"result\": 332}"

WEB_ROOT : String
WEB_ROOT = "/home/jan/github.com/websocket-examples/jsClient"

{-
ptrToString : Ptr String -> Maybe String
ptrToString ptr =
  if prim__nullPtr ptr == 1
     then Nothing
     else Just (believe_me ptr)
-}





x_my_http_handler : HasIO io => Ptr MG_CONNECTION -> MG_EVENT_TYPE -> Ptr EV_DATA -> Ptr FN_DATA -> io ()
x_my_http_handler p_conn MG_EV_HTTP_MSG p_ev p_fn = do
                    let hm = (ev_to_http_message p_ev)                    
                    --putStrLn ("HTTP is null: " ++ (show (is_ptr_null p_fn)))
                    putStrLn ("HTTP val: " ++ (show (get_p_int p_fn ) ))                                        
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

my_http_handler :  (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn (fromBits8 ev) p_ev p_fn)


partial
inf_loop : (Ptr MG_MGR) -> Int -> IO ()
inf_loop p_mgr time_out = do
  mg_mgr_poll p_mgr time_out
  inf_loop p_mgr time_out



gen_adder : Int -> (Int ->Int)
gen_adder x = (\a => a+x)

so_id_44575 : Bits32
so_id_44575 = 44575
{-
pjb_test : IO ()
pjb_test = do
  t0 <- time
  p <- BrowseResPartner.read (True)
  let toPair : String -> (String,Int)
      toPair x = (x,1)
      pmap : SortedMap String Int
      pmap = fromList (map (toPair . name) p)
  t1 <- time
  traverse_ printLn (Data.SortedMap.toList pmap)
  
  printLn (t1-t0)
  t2 <- time
  printLn (lookup "Zaki3" pmap)
  t3 <- time
  printLn (t3-t2)
  -}
pjb_test : IO ()
pjb_test = do
  so <- BrowseOrder.read_ids [21833] (True)
  printLn so
  inv <- BrowseAccountInvoice.read (True)
  sp <- BrowseStockPicking.read (True)
  av <- BrowseAccountVoucher.read (True)  
  traverse_ printLn so
  traverse_ printLn inv
  traverse_ printLn sp  
  traverse_ printLn av
  
  
mg_test : IO ()
mg_test = do
  --ignore $ run forever greet

  
  
  mg_log_set "3"
  p_mgr <- get_and_malloc__mg_mgr
  mg_mgr_init p_mgr 

  x1 <- malloc_pint                     
  set_p_int x1 99                    
      
  mg_http_listen p_mgr "0.0.0.0:8080" my_http_handler x1
  --inf_loop p_mgr 1000
  mg_mgr_free p_mgr 
  

main : IO ()
main = do
--  test_libc_time
  --pjb_test
  test_demo2
  --test_main_x
  --db_main  
  --mg_test
  pure ()
