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

import public RT

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
import Data.SnocList

%language ElabReflection

namespace Queue
  public export
  data Queue : (a:Type) -> Type where
     MkQ : (f:List a) -> (r:SnocList a) -> Queue a  
  %runElab derive "Queue" [Generic, Meta, Eq, Ord, Show]
  
  data Muf : Type where
     C1 : Muf  
     C2 : Int -> Muf -> Muf
  %runElab derive "Muf" [Generic, Meta, Eq, Ord, Show]
  
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
    
            
  export
  head : Queue a -> Maybe a   
  head (MkQ [] r) = Nothing
  head (MkQ (x :: xs) r) = Just x
  
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
  export
  qtoList : Queue a -> List a
  qtoList x = case (head x) of
     Nothing => []
     Just y => [y] ++ (qtoList $ tail x)
  export
  toQueue : {a:Type} -> List a -> Queue a
  toQueue [] = MkQ [] [<]
  toQueue (x :: xs) = snoc (toQueue xs) x 

q1 : Queue Int
q1 = MkQ [] [<]

q2 : Queue Int
q2 = snoc q1 10

q3 : Queue Int
q3 = snoc q2 20

q4 : Queue Int
q4 = snoc q3 120

  --snoc (MkQ f (sx :< x)) a = ?sdfsdf_2 --(MkQ f (sx :< x))
  
  --snoc (MkQ f [<]) a = ?sdfsdf_1 --(MkQ f [<])
  --snoc (MkQ f (sx :< x)) a = ?sdfsdf_2 --(MkQ f (sx :< x))




public export
data Ref : (l : label) -> Type -> Type where
     [search l]
     MkRef : IORef a -> Ref x a
     
export   
newRef : HasIO io => (x : label) -> t -> io (Ref x t)
newRef x val
    = do ref <-  (newIORef val)
         pure (MkRef ref)

export %inline
get : HasIO io => (x : label) -> {auto ref : Ref x a} -> io a
get x {ref = MkRef io} = (readIORef io)

export %inline
put : HasIO io => (x : label) -> {auto ref : Ref x a} -> a -> io ()
put x {ref = MkRef io} val = (writeIORef io val)

export %inline
update : HasIO io => (x : label) -> {auto ref : Ref x a} -> (a -> a) -> io ()
update x f
  = do v <- get x
       put x (f v)
       
public export
data MGs : Type where

public export
record MGSt where
  constructor MkMGSt
  cn : Int



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


x_my_http_handler : HasIO io => {auto cx:Ref MGs MGSt} -> Ptr MG_CONNECTION -> MG_EVENT_TYPE -> Ptr EV_DATA -> Ptr FN_DATA -> io ()
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

my_http_handler : {auto cx:Ref MGs MGSt} -> (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn (fromBits8 ev) p_ev p_fn)


partial
inf_loop : {auto cx:Ref MGs MGSt} -> (Ptr MG_MGR) -> Int -> IO ()
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


{-
          add_lines : (List PrimOrderLine.RecordModel) ->io (List  O2MOrderLine.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrderLine.MkRecordModel pk price_unit product_uom_qty discount delivery_line order_id product_id)::xs) = do

             let muf1 = ((JC PkOTax TaxIdM2M_ST)&&(OrderLineIdM2M_ST==(cast pk) ))
             tax_ids_np <- getJoin c OTax_NP M2M_ST_NP (columns OTax_NP) muf1
             let tax_ids=[PrimOrderTax.toRecord ox |ox <-tax_ids_np]
             let muf = ((PkOT==(cast order_id)))
             order_id <- PrimOrder.read_records_c c muf            
             let ret =(O2MOrderLine.MkRecordModel pk price_unit product_uom_qty discount delivery_line order_id product_id tax_ids)
             ret_xs <- add_lines xs
             pure ([ret]++ret_xs)
-}

{-
          add_lines : (List PrimOrder.RecordModel) ->io (List  O2MOrder.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id requested_date)::xs) = do
            let muf = ((OrderIdOLT==(cast pk)))

            order_line <- O2MOrderLine.read_records_c c muf
            
            let ret =(O2MOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id order_line requested_date)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)          
-}


{-

            let muf = ((OrderIdOLT==(cast pk)))

            order_line <- O2MOrderLine.read_records_c c muf
            
            let ret =(O2MOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id order_line requested_date)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)
-}

main : IO ()
main = do

  --l1 <- muf_3
  --l1 <- SO_Simple.read (Id_OT == (cast so_id_44575)) --no implementation
  --l1 <- SO_Simple.read (NameOT == (cast "SO44512"))
  --test_main_x
{-  
  so <- O2MOrder.read_ids [21833] (True)
  
  printLn so
  
  inv <- O2MAccountInvoice.read (True)
  sp <- O2MStockPicking.read (True)
  av <- O2MAccountVoucher.read (True)
  
  --traverse_ printLn so
  traverse_ printLn inv
  traverse_ printLn sp  
  traverse_ printLn av
-}
  --traverse_ printLn (toHList testList Nothing)
  printLn nullStrListT
  printLn RT.l1
  printLn RT.l2
  
  let (prev, htype_map) = toHList testList
  --printLn prev
  --
  traverse_ printLn (map snd (Data.SortedMap.toList htype_map))
  
  --ret <- O2MResPartner.read_ids [11992] (True)
  --traverse_ printLn ret
  
  --ret <- O2MResPartner.read  (True)
  --let x = (map child_ids ret)
  --traverse_ printLn x
  
  --
  --traverse_ printLn ret
  
  --[19446]
  --ret2 <- O2MOrder.read  (True)
  --traverse_ printLn ret2
  
  pure ()
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
