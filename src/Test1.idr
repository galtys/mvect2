module Test1


import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256

import Data.SortedMap

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Types2
import Category.Transaction.RouteTypes
import Category.Transaction.Hom
import Category.Transaction.Journal
import Category.Transaction.Types
import Category.Transaction.Demo

import Data.Ratio
--import Data.Zippable
import Control.Monad.State

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

--import Core.Context
import System.FFI
import Libc.Time
import UserDataDemo

import Test2
import Category.Transaction.Owner
import Category.Transaction.Warehouse
import Category.Transaction.Demo2
import Control.Monad.Either

import Examples.Selector
import Rhone.JS
import Crypto.Hash.SHA256
import Text.Html
import Examples.CSS

import Browser.WebSocket
import Browser.WS2

%ambiguity_depth 10

json_result : String
json_result = "{\"result\": 332}"

WEB_ROOT : String
WEB_ROOT = "."


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
                    if (mg_http_match_uri hm "/rest")==1 then do
                           mg_http_reply p_conn 200 "Content-Type: application/json\r\n" json_result
                           set_p_int p_fn 100                           
                       else if (mg_http_match_uri hm "/websocket")==1 then do
                           mg_ws_upgrade p_conn p_ev get_pchar_NULL  
                         else do
                             p_opts <- (get_and_malloc__mg_http_serve_opts  WEB_ROOT)
                             mg_http_serve_dir p_conn hm p_opts 

x_my_http_handler p_conn MG_EV_ACCEPT p_ev p_fn = do
                    putStrLn ("EV acceptp_fn  val: " ++ (show (get_p_int p_fn)))
                    pure ()
                    
x_my_http_handler p_conn MG_EV_WS_MSG p_ev p_fn = do
                    let p_wm = (ev_to_ws_message p_ev)
                    msg <- mg_ws_receive_as_String p_conn p_wm                 
                    putStrLn ("Sending Back: (with MAGIC)" ++ msg)
                    mg_ws_send_text p_conn (msg++" with MAGIC")
                    
x_my_http_handler p_conn ev p_ev p_fn = do 
                  pure ()

my_http_handler :  (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn (fromBits8 ev) p_ev p_fn)


partial
inf_loop : (Ptr MG_MGR) -> Int -> IO ()
inf_loop p_mgr time_out = do
  mg_mgr_poll p_mgr time_out
  inf_loop p_mgr time_out




{-
ret where
   key : (Bits32,Bits32)
   key = (location_id,location_dest_id) 
   
   ret : SortedMap (Bits32,Bits32) (List BrowseStockMove.RecordModel)
   ret =  case (lookup key m) of
             Just mv_list  => insert (move::mv_list) (moveMap xs)
             Nothing => insert [move] (moveMap xs)
-}
             
{-
moveCount : SortedMap (Bits32,Bits32) BrowseStockMove.RecordModel -> ( (Bits32,Bits32), Integer )
moveCount m = [ (x, length y) | (x,y) <- toList m ]
-}
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

   
export
test_demo2 : IO ()
test_demo2 = do
  {-
  boms <- BrowseBoM.read (True)
  let bom_map = toBoM_map boms
      dx = qtyFromOrderLine (order_line so_44970)     
      h1_bom = map_to_BoM32 dx bom_map
      h1_order_stock = variants_BoM32 $ mult_BoM32 1  h1_bom
  --printLn  dx      
  --printLn  h1_order_stock  
  let dx1 : List Bits32
      dx1 =[735,1042,1064,4085,4089,4095]
      dx2 : List Bits32
      dx2= [735,2932,2852,726,2942,3531,733,4085,4089,4095]
  let ocas : List Bits32
      ocas = dx1++dx2 --[735,1042,1064,4085,4089,4095] 
      
      ff : Bits32 -> Bool
      ff x = elemBy (==) x ocas
  
  products <- BrowseProduct.read_ids ocas (True)
  putStrLn "import Libc.Time" 
  putStrLn "import Data.Ratio" 
  putStrLn "import Category.Transaction.Types"  
  putStrLn "import Odoo.Schema.PJBRecDef"
  putStrLn ""
  putStrLn "static_products : List BrowseProduct.RecordModel"
  putStrLn #"static_products = \#{show products}"# 

  let boms2 = filter (ff . product_id) boms
  putStrLn ""
  putStrLn "static_boms : List BrowseBoM.RecordModel"
  putStrLn #"static_boms = \#{show boms2}"# 
  -}  

  
  Right ret <- runEitherT (run_interpret_d {io = EitherT DBError IO})
            | Left (err) => putStrLn $ show err
  printLn ret 
  
  reas22 <- execStateT initState (interpret (toWhs   demo_po_so)   )    
  
--  printLn reas
  
  pure ()


pjb_test : IO ()
pjb_test = do
  cust <- BrowseResPartner.read_ids [31587] (True)
  
  so <- BrowseOrder.read (True)  
  --so <- BrowseOrder.read_ids [19273, 44970, 24359, 45063, 45064] (True)  
  av <- BrowseAccountVoucher.read_ids [43244] (True)  
  sp <- BrowseStockPicking.read_ids [43747] (True)
  mv <- BrowseStockMove.read (True)
  printLn "reading Done"
  --print_group mv
  
  
  --retMvs <- execStateT (empty,empty,empty)  (interpret $ toWhs demo_po_so)       ---(moveMap xs) --runXdd xs
  
  --xas <- 
  
  
  --printLn xas
  
{-  
  let retR : (StockMoveMap, () )
      retR = runState (moveMap mv) 
  -}    
  --traverse_ printLn [ (location_id m, location_dest_id m) | m <- mv ]
  
  
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
      
  mg_http_listen p_mgr "0.0.0.0:8000" my_http_handler x1
  inf_loop p_mgr 1000
  mg_mgr_free p_mgr 
  

main : IO ()
main = do
  --test_libc_time
  --pjb_test
  --test_demo2
  --putStrLn $ Html.render Selector.content
  --putStrLn CSS.allRules
  --x <- muf_3_bom
  --traverse_ printLn x
  --pjb_test
  --test_main_x
  --db_main  
  mg_test
  pure ()
