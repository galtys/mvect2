module Test1


import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256

import Data.SortedMap

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Types2
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
                    --l1 <- muf_3_bom
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


calc_so : List BrowseOrder.RecordModel -> IO ()
calc_so [] = pure ()
calc_so (so::xs) = do
  let h1_order =qtyFromOrderLine (order_line so)
      {-
      bom_map = toBoM_map boms
      h1_bom = map_to_BoM32 h1_order bom_map
      
      INT-PART
      
      h1_order_stock = variants_BoM32 $ mult_BoM32 1  h1_bom
      h1_stock = fromStockMove sp_43747.move_ids
      -}
      h2 = priceFromOrderLine (order_line so)
      --h2_picking = priceFromStockMove INC20 sp_43747.move_ids
      
      inc20 =evalHom1 $ getCxINC20 $ ( applyHom2 h2 h1_order )
      ex20 =evalHom1 $ getCxEX20 $ ( applyHom2 h2 h1_order )
      
      tax = evalHom1 $( applyHom2Tax h2 h1_order )
      tax_diff = map fromPrice (tax - [amount_tax so])
      
      totals : Hom1
      totals = [amount_tax so,amount_untaxed so,amount_total so]
      
      ex_diff : Hom1 --List Double
      ex_diff = ( ex20-[amount_untaxed so] )
      
      inc20_diff : Hom1 --List Double
      inc20_diff =  (inc20 - [amount_total so] )
      
  --print_BoM32 3303 m32x
  
  case inc20 of
     [] => do
          case (map fromPrice ex_diff) of
             [] => pure ()
             (ex::exs) => case ex>1.00 of
                 True => do
                     printLn (so.name,so.pk, totals, tax)
                     printLn ( "  " ++ (show ex) )
                 False => pure ()
                
     (x::xs) => 
          case (map fromPrice inc20_diff ) of
             [] => pure ()
             (i_d :: sss) => case i_d>1.00 of
                  True => do
                      printLn (so.name,so.pk, totals, tax)
                      printLn ( "  " ++ (show i_d) )
                  False => pure ()
                
  calc_so xs

pjb_test : IO ()
pjb_test = do
  cust <- BrowseResPartner.read_ids [31587] (True)
  
  so <- BrowseOrder.read (True)  
  --so <- BrowseOrder.read_ids [19273, 44970, 24359, 45063, 45064] (True)  
  av <- BrowseAccountVoucher.read_ids [43244] (True)  
  sp <- BrowseStockPicking.read_ids [43747] (True)
  mv <- BrowseStockMove.read (True)
  traverse_ printLn [ (location_id m, location_dest_id m) | m <- mv ]
  --traverse_ printLn so  
  --traverse_ printLn sp      
  --traverse_ printLn av
  --calc_so so
{-    
  boms <- BrowseBoM.read (True)
  --traverse_ printLn boms
  let h1_order =qtyFromOrderLine so_44970.order_line
      
      bom_map = toBoM_map boms
      h1_bom = map_to_BoM32 h1_order bom_map
      
      h1_order_stock = variants_BoM32 $ mult_BoM32 1  h1_bom
      h1_stock = fromStockMove sp_43747.move_ids
      
      h2 = priceFromOrderLine so_44970.order_line
      h2_picking = priceFromStockMove INC20 sp_43747.move_ids
      
      inc20 = evalHom1 $ getCxINC20 $ ( applyHom2 h2 h1_order )
      ex20 = evalHom1 $ getCxEX20 $ ( applyHom2 h2 h1_order )
      
      tax = evalHom1 $ ( applyHom2Tax h2 h1_order )
      
  --print_BoM32 3303 m32x

  
  --traverse_ printLn $ ( h1_order_stock - h1_stock)
  printLn $ (inc20-tax)
  
  printLn $ (ex20+tax)
  
  --traverse_ printLn $ evalHom1 $ ( applyHom2 h2 h1_order )
  --traverse_ printLn $ evalHom1 $ ( applyHom2Tax h2 h1_order )
    
  --printLn $ evalHom1 $ ( applyHom2 h2_picking h1_stock )  --discount missing
  --printLn $ fromAccountVoucher [va_43244]
-}      
  pure ()
          
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
  --x <- muf_3_bom
  --traverse_ printLn x
  pjb_test
  --test_main_x
  --db_main  
  --mg_test
  pure ()
