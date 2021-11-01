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
%ambiguity_depth 10

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

{-
prods_to_bom_ids : HasIO io => MonadError SQLError io => Connection -> List (Bits32) -> io (List Bits32)
prods_to_bom_ids c [] = pure []
prods_to_bom_ids c ((p_id)::xs) = do
  child_rows <- get c BoM_NP [ProductID,ProdQty,BomID,Id] (IsNull BomID  && ProductID==(cast p_id) )
  let child_b_ids = [ (id_ (toRBoM ox)) | ox <- child_rows ]
  ret_xs <- prods_to_bom_ids c xs
  pure (child_b_ids++ret_xs)



read_bom_p_id2 : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> io (List BoM32) 
read_bom_p_id2 c [] = pure []
read_bom_p_id2 c (b_id::xs) = do  
    --child skus
    child_rows <- get c BoM_NP [ProductID,ProdQty,BomID,Id] (BomID == Just (cast b_id) )  
    let child_rbom = [ toRBoM ox | ox <- child_rows ]
    let child_p_ids = [ product_id ox | ox <-child_rbom ]
    
    --printLn "p_ids"
    --printLn child_p_ids    
    child_b_ids <- prods_to_bom_ids c child_p_ids    
    --printLn child_b_ids
    
    
    ch_boms <- read_bom_p_id2 c  child_b_ids   
    
    
    --this bom
    this <- get c BoM_NP [ProductID,ProdQty,BomID,Id] (Id == (cast b_id) )  
    let this_rbm = [ toRBoM ox | ox <- this ]
    
    --let ret=if (length child_b_ids==0) then read_bom4 child_rbom [] else read_bom4 this_rbm ch_boms
    let ret = read_bom4 this_rbm ch_boms
    u <- read_bom_p_id2 c xs
    pure (ret ++ u)
-}

rbom2bom32  : RBoM -> (List BoM32) -> BoM32
rbom2bom32 (MkRBoM product_id product_qty bom_id id_) xs = let 
   qty = (cast product_qty) in Node32 (fromInteger qty) product_id xs

chmap2bom32 : (List (RBoM, List RBoM) ) -> List BoM32 -> List BoM32
chmap2bom32 [] chld= []
chmap2bom32 ((x, y) :: xs) chld = 
   let ch=[ (rbom2bom32 ox chld) | ox <- y]
       b=rbom2bom32 x ch in [b]++chmap2bom32 xs chld


read_root_boms : HasIO io => MonadError SQLError io => Connection -> io (List RBoM) 
read_root_boms c  = do
  child_rows <- get c BoM_NP [ProductID,ProdQty,BomID,Id] (IsNull BomID)
  let child_rbom = [ toRBoM ox | ox <- child_rows ]
  pure child_rbom
                          
read_bom_p_id2 : HasIO io => MonadError SQLError io => Connection -> (List RBoM) ->  io (List (RBoM, List RBoM) )
read_bom_p_id2 c [] = pure []
read_bom_p_id2 c (x@(MkRBoM product_id product_qty b_id id_) :: xs) = do

  child_rows <- get c BoM_NP [ProductID,ProdQty,BomID,Id] (BomID == Just (cast id_ ) )  
  let child_rbom = [ toRBoM ox | ox <- child_rows ]
  let ret = ((x,child_rbom))
  xs <- read_bom_p_id2 c xs
  pure ([ret]++xs) 

child_map_RBoM : (List (RBoM, List RBoM) ) ->  SortedMap Bits32 (List RBoM)
child_map_RBoM [] = empty
child_map_RBoM (( (MkRBoM product_id product_qty bom_id id_), y) :: xs) = insert product_id y (child_map_RBoM xs)

test_x4 : HasIO io => MonadError SQLError io => Connection -> (List (RBoM, List RBoM) ) ->  io ()
test_x4 c [] = pure ()
test_x4 c ((x, y) :: xs) = do
   lx <- read_bom_p_id2 c y
   printLn lx
   test_x4 c xs
   
safeHead : List x -> Maybe x
safeHead [] = Nothing
safeHead (y :: xs) = Just y

rbom_to_list : Maybe (List RBoM) -> List (Bits32,Bits32)
rbom_to_list Nothing = []
rbom_to_list (Just x) = [ (product_qty u,product_id u) | u<-x]

ret_spaces : Bits32 -> String
ret_spaces x = if x==0 then "" else concat [ "  " | u<- [0..x]]

print_ch : HasIO io =>  Bits32 -> Bits32 -> SortedMap Bits32 (List RBoM) -> io()
print_ch i p_id m = do
  printLn ( (ret_spaces i) ++(show p_id)++":"++(show $ rbom_to_list $ lookup p_id m))

print_ch_r : HasIO io =>  Bits32 -> List (Bits32,Bits32) -> SortedMap Bits32 (List RBoM) -> io ()
print_ch_r i [] m = pure ()
print_ch_r i (muf@(qty,p_id)::xs) m = do
  let ch = rbom_to_list $ lookup p_id m
  
  printLn ( (ret_spaces i) ++(show p_id)++":"++(show ch ))
  print_ch_r (i+1) ch m
  
  print_ch_r (i) xs m


main_read_bom : HasIO io => MonadError SQLError io => Bits32 -> io ()
main_read_bom p_id = do
  c    <- connect "postgresql://jan@localhost:5432/pjb-2021-10-27_1238"  
  boms <- read_root_boms c
  let root_p_ids = [ (product_qty u,product_id u) | u <- boms]
  --printLn (length boms)
  l1 <- read_bom_p_id2 c boms
  --let l2 =  chmap2bom32 l1 []
  let m1 = child_map_RBoM l1
  --rows <- get c BoM_NP [Id] (IsNull BomID)  
  --b_ids <- prods_to_bom_ids c [p_id]
  --boms <- read_bom_p_id2 c b_ids
  
  print_ch_r 0 root_p_ids m1
  {-
  print_ch 0 3303 m1
  print_ch 1 145 m1
  print_ch 2 2919 m1  
  print_ch 2 3003 m1  
  print_ch 1 1670 m1  
  print_ch 2 1393 m1
  print_ch 2 1662 m1
  -}
  
  --printLn (lookup 3303 m1)
  --printLn (lookup 145 m1)
  --printLn (lookup 1670 m1)

  --printLn (lookup 2919 m1)
          
  --printLn $ safeHead l2
  --test_x4 c l1
    --printLn ocas
  --traverse_ printLn rows

  finish c
  
main_3 : IO ()
main_3 = do Left err <- runEitherT (main_read_bom {io = EitherT SQLError IO} 145)
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
