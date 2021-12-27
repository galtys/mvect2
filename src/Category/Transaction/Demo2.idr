module Category.Transaction.Demo2

import Data.Zippable
import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import Libc.Time

import JSON

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.RouteTypes
import Category.Transaction.Route
import Category.Transaction.Types2
import Category.Transaction.Hom
import Category.Transaction.Journal
import Crypto.Hash.SHA256
import Data.Ratio
import Odoo.Schema.PJBRecDef
--import Odoo.Schema.PJB
import UserDataDemo
import Odoo.PG.BoM

%language ElabReflection

safeHead : List x -> Maybe x
safeHead [] = Nothing
safeHead (y :: xs) = Just y

export
userDataToMap : UserData -> UserDataMap
userDataToMap (MkUD p t b tax) = (MkUDMap p_map t_map b_map tax_map) where
     p_map : SortedMap ProdKey  BrowseProduct.RecordModel
     p_map = (fromList [( PK32 DX (pk u), u) | u <- p ])
     
     t_map : SortedMap ProdKey BrowseProductTemplate.RecordModel
     t_map = (fromList [( PK32 DX (pk u), u) | u <- t ])
     b_map : SortedMap ProdKey (List BrowseBoM.RecordModel)    --SortedMap Bits32 BrowseBoM.RecordModel
     b_map = toBoM_map b  --(fromList [(pk u, u) | u <- b ])
     tax_map : SortedMap Bits32 BrowseOrderTax.RecordModel
     tax_map = (fromList [(pk u, u) | u <- tax ])

export
emptyUserData : UserData
emptyUserData = (MkUD [] [] [] [])

export
initState : SystemState --(RouteMap,LocationMap,RouteJournalMap)
initState = (MkSS empty empty empty empty [] (userDataToMap emptyUserData))

export
StockMoveMap : Type
StockMoveMap = SortedMap (Bits32,Bits32) (List BrowseStockMove.RecordModel)

export
moveMap : List BrowseStockMove.RecordModel -> StateT StockMoveMap IO ()
moveMap [] = pure ()
moveMap (x:: xs) = do
   m <- get   
   let keyx : (Bits32,Bits32)
       keyx = (location_id x,location_dest_id x) 
       m' : StockMoveMap
       m' = case (Data.SortedMap.lookup keyx m) of
               Nothing => insert keyx [x] m
               Just mv_list => insert keyx (x::mv_list) m
   --printLn keyx
   put m'
   moveMap xs
export
print_group : List BrowseStockMove.RecordModel -> IO ()    --List ( (Bits32,Bits32), Integer)
print_group xs = do
  retMvs <- execStateT empty (moveMap xs) --runXdd xs
  
  let ocas = [ (x,Prelude.List.length y) | (x,y) <- Data.SortedMap.toList retMvs ]
  traverse_ printLn ocas
  
  pure ()
  
export
mult_p : EQty -> Product -> Product
mult_p x (k,q) = (k,x*q)
  
export
init_self : WhsEvent () --RouteRef
init_self = do
     let user_data = (MkUD static_products [] static_boms [])
     UpdateUserData user_data
     Log (MkUserUpdate user_data)
     let -- InitDate
         price : Product
         price = (toEX20 1000)         
         share : Product
         share = ("GTX43",100)
         h2 : Hom2
         h2 = [ (fst share, price) ]
         h1 : Hom1
         h1 = [share]
         
         h121 : Hom121
         h121 = MkH121 h1 [] h2 (applyHom2 h2 h1)  (MkH11 h1 (applyHom2 h2 h1)   )
         
         je : FxEvent
         je = Fx121 InitDate h121          
         {-
         je_cx : FxEvent
         je_cx = Fx11 InitDate (MkH11 [] (applyHom2 h2 h1))
         -}
         je_dx : FxEvent
         je_dx = Fx11 InitDate (MkH11 h1 [])         
         fx : FxData
         fx = MkFx InitDate Sale self_company self_company h121         
         fx_empty : FxEvent
         fx_empty = Fx121 (date fx) (MkH121 [] [] (appl $ h3 fx) [] emptyHom11)
         
     ref_init <- NewRoute InitDate InitRouteT       
     SetFxData (ref_init) fx
     --Log (MkOpen fx)
     --Log (MkNewRoute InitRouteT je)       
     Put (MkRouteKeyRef ref_init) (reconcile InitRoute) je  --forecast
     Put (MkRouteKeyRef ref_init) (convMovekey $ reconcile InitRoute) je  --forecast
     --Show je     
     Put (MkRouteKeyRef ref_init) (allocation InitRoute) je 
     Put (MkRouteKeyRef ref_init) (convMovekey $ allocation InitRoute) je_dx
     inventory_route <- NewRoute InitDate InventoryRouteT
     --Log (MkNewRoute InventoryRouteT fx_empty)
     tax_route <- NewRoute InitDate TaxRouteT
     --Log (MkNewRoute TaxRouteT fx_empty)          
     bank_route <- NewRoute InitDate BankRouteT
     --Log (MkNewRoute BankRouteT fx_empty)     
     fx_route <- NewRoute InitDate FxRouteT
     --Log (MkNewRoute FxRouteT fx_empty)       
     Pure ()

export
confirm_po : OwnerEvent ()
confirm_po = do
 Init 
 user_data  <- GetUserData 
 let prod_map = products user_data
     date1 : Date
     date1 = "2021-10-01"
     dx1 : Hom1 
     dx1 = [ (PK32 DX 1, 10), (PK32 DX 3, 15), (PK32 DX 4, 5), (PK32 DX 5, 1), (PK32 DX 6,2)]
     
     h2 : Hom1 -> Hom2
     h2 dx_' = [ (fst x, mult_p 0.9 (trade_price (lookup (fst x) prod_map)) ) |x <- dx_' ]
     
     cx1 : Hom1
     cx1 = (applyHom2 (h2 dx1) dx1)     
     h11_1 : Hom11
     h11_1 = MkH11 dx1 cx1          
     po1 : FxData
     po1 = MkFx date1 Purchase factory1 factory1 (MkH121 dx1 [] (h2 dx1) cx1 h11_1)
     
     date2 : Date
     date2 = "2021-10-15"
     dx2 : Hom1 
     dx2 = (map (mult_p 2) dx1) ++ [ (PK32 DX 7,3) ]
     cx2 : Hom1
     cx2 = (applyHom2 (h2 dx2) dx2)     
     
     h11_2 : Hom11
     h11_2 = MkH11 dx2 cx2          
     po2 : FxData
     po2 = MkFx date2 Purchase factory2 factory2 (MkH121 dx2 [] (h2 dx1) cx2 h11_1)
     
     date3 : Date
     date3 = "2021-11-05"
     po3 : FxData
     po3 = MkFx date3 Purchase factory1 factory1 (MkH121 dx1 [] (h2 dx1) cx1 h11_1)
     
 rew_r <- ConfirmOrder po1
 rew_r <- ConfirmOrder po2
 rew_r <- ConfirmOrder po3
      
 Pure ()

export
confirm_so : OwnerEvent ()
confirm_so = do

 user_data  <- GetUserData 

 let date = "2021-11-01"
     bom_map = boms user_data
     --h1 = [p1,p2,p3,p4]
     dx = qtyFromOrderLine (order_line so_44970)     
     h1_bom = map_to_BoM32 dx bom_map
     
     h1_order_stock = variants_BoM32 $ mult_BoM32 1  h1_bom
     h2 = priceFromOrderLine (order_line so_44970)
     
     tax = applyHom2Tax h2 dx           
     cx = (applyHom2 h2 dx) + tax     
     h11 = evalHom11 $ MkH11 h1_order_stock cx
     fx = MkFx date Sale hilton hilton (MkH121 dx h1_bom h2 cx h11) 
     
 Show h1_order_stock
 
 new_r <- ConfirmOrder fx
 r <- GetRoute new_r
 case r of
   Nothing => Pure ()
   Just rt => do
       let al = allocationMove rt
       x <- Get al
       Show x
       x <- Get (convMovekey al)
       Show x
       
 ff <- Get $ allocation InitRoute
 aa <- Get $ convMovekey $allocation InitRoute
 {-
 Show (ff)
 Show (aa)  
 Show $evalHom11 (ff-aa) --to spend
-} 
 Pure ()



public export
update_ledger : (Location, Ledger) -> Hom1 -> LocationMap -> LocationMap 
update_ledger k [] m = m
update_ledger k@(ct,l) ( (pk,eq)::xs) m = ret where
          key : (Location, Ledger,ProdKey)
          key = (ct,l,pk)
          
          ret : LocationMap
          ret = case (lookup key m ) of
                  (Just q) => (update_ledger k xs (insert key (eq+q) m) )
                  Nothing => (update_ledger k xs  (insert key eq m)     )
{-
export
validateDirection : (from:Location) -> (to:Location) -> Bool
validateDirection (Partner Sale y) (Control Sale x) = True
validateDirection (Control Sale y) Self = True
validateDirection Self (Control Purchase y) = True
validateDirection (Control Purchase y) (Partner Purchase x) = True
validateDirection Init Self = True
validateDirection _ _ = False
-}


export
toWhs : OwnerEvent a -> WhsEvent a
toWhs (ConfirmOrder fx) = do
       Log (MkOpen fx)       
       let fx_ev : FxEvent
           fx_ev = Fx121 (date fx) (h3 fx)
           fx_empty : FxEvent
           fx_empty = Fx121 (date fx) (MkH121 [] [] (appl $ h3 fx) [] emptyHom11)
           
           so : SaleForecastRoute
           so = soForecastFromFx fx
           po : PurchaseForecastRoute
           po = poForecastFromFx fx
           
       case (direction fx) of
           Purchase => do
               new_r <- NewRoute (date fx) (MkPoR po)
               SetFxData new_r fx
               let route_key = MkRouteKeyRef new_r
               Put route_key  (forecastIn po) fx_ev               
               Put route_key  (purchaseInvoice po) fx_empty               
               Put route_key  (purchaseOrder po) fx_ev
               Pure new_r
           Sale => do
               new_r <- NewRoute (date fx) (MkSoR so)
               SetFxData new_r fx               
               let route_key = MkRouteKeyRef new_r               
               Put route_key (saleOrder so) fx_ev               
               Put route_key (saleInvoice so) fx_empty
               Put route_key (saleDemand so) fx_ev               
               Pure new_r
toWhs (GetFxData key) = do
       r <- GetFxData key
       Pure r
toWhs (GetRoute key) = do
       r <- GetRoute key
       Pure r
                      
toWhs (Init) = do  
      init_self                     
       
toWhs (UpdateUserData user_data) = do
       UpdateUserData user_data
       Log (MkUserUpdate user_data)       
toWhs (GetUserData) = do
       ret <- GetUserDataW
       Pure ret                      
toWhs (Post ref key fx) = do
      Put (MkRouteKeyRef ref) key fx
      Log (MkPost ref key fx)
toWhs (Get key) = do
      whs_xs <- Get key
      let xs = (map fx whs_xs)
          fxToH11 : FxEvent -> Hom11
          fxToH11 (Fx121 date h121) = fromH121 h121
          fxToH11 (Fx11 date h11) = h11          
          sum_ : Hom11
          sum_ = sumHom11 $ map fxToH11 xs
      Pure sum_
      
toWhs (Close ref) = do
       CloseRoute ref
       Log (MkClose ref)       
toWhs (Allocate entry@(MkAE ledger moves) ) = do       
       let a_cnt = encode entry
           a_ref : Ref
           a_ref = (MkAllocationRef (sha256 a_cnt))
           
           {-
           muf2 : AllocationItem -> WhsEvent (Maybe (RouteSumT,RouteSumT,FxEvent))
           muf2 ai =  do
               rf <- GetRoute (supplier ai)
               rt <- GetRoute (customer ai)
               case (rf,rt) of
                  (Just rx, Just ry) => Pure (Just (rx,ry,fx ai))
                  _ => Pure Nothing
                  
           allocateItem : (RouteSumT,RouteSumT,FxEvent) -> WhsEvent () -- Maybe (RouteKey, RouteKey, FxEvent)
           allocateItem (rx,ry,fe) = do
               let rkx : Maybe MoveKey
                   rkx = safeHead $ route2ft (reverse rx) ledger
                   rky : Maybe MoveKey               
                   rky = safeHead $ route2ft ry ledger
                   
               case (rkx,rky) of
                   (Just jx, Just jy) => do
                        Put a_ref jx fe
                        Put a_ref jy fe
                   _ => Pure ()
           allocate : List AllocationItem -> WhsEvent ()
           allocate [] = Pure ()
           allocate (x::xs) = do
                ret <- muf2 x
                case ret of
                   (Just y) => allocateItem y
                   Nothing => Pure ()
                allocate xs
       allocate moves
       Log (MkAEntry entry)
       -}
       Pure a_ref
       
toWhs (Show x) = Show x --Pure ()
toWhs (Pure x) = Pure x
toWhs (Bind x f) = do res <- toWhs x
                      toWhs (f res) --?toWhs_rhs_4
                      
{-
new_route : Date -> RouteSumT -> WhsEvent RouteKey
new_rotue d r = NewRoute (date fx) route
-}
export
interpret : WhsEvent a -> StateT SystemState IO a
       
interpret  (NewRoute dt route) = do
             (MkSS fx_map routes led_map rjm j user_data)<-get             
             let route_ref = routeSha route                 
                 r_k : RouteKey
                 r_k = (MkRK dt route_ref Progress)                                                   
                 routes' : SortedMap RouteKey RouteSumT
                 routes' = insert r_k  route routes
                 
             put (MkSS fx_map routes' led_map rjm j user_data)
             pure r_k

interpret  (SetFxData r_k fx) = do
             (MkSS fx_map routes led_map rjm j user_data)<-get                          
             let fx_map' : SortedMap RouteKey FxData
                 fx_map' = insert r_k fx fx_map                 
             put (MkSS fx_map' routes led_map rjm j user_data)

interpret (UpdateUserData user_data ) = do
             (MkSS fx_map routes led_map rjm j udm)<-get
             let udm' = userDataToMap user_data
             put (MkSS fx_map routes led_map rjm j udm')
             
interpret (GetUserDataW ) = do
             (MkSS fx_map routes led_map rjm j user_data_map)<-get
             pure user_data_map
             
interpret (CloseRoute route_ref@(MkRK date ref state)   ) = do     
            (MkSS fx_map routes led_map rjm j user_data_map)<-get
            case lookup route_ref routes of
              Nothing => pure ()
              (Just this) => do
                  let new_ref : RouteKey
                      new_ref = (MkRK date ref Completed)
                      routes' : SortedMap RouteKey RouteSumT
                      routes' = insert new_ref this routes
                      
                      routes'' : SortedMap RouteKey RouteSumT
                      routes'' = delete route_ref routes'
                  put (MkSS fx_map routes'' led_map rjm j user_data_map)
                  
            pure ()
interpret (GetFxData rk) = do
            (MkSS fx_map routes led_map rjm j user_data_map)<-get
            pure (lookup rk fx_map)
            
interpret (GetRoute rk) = do
            (MkSS fx_map routes led_map rjm j user_data_map)<-get
            pure (lookup rk routes)
interpret (Put ref (MkMK f t ledger) je) = do
             (MkSS fx_map routes led_map rjm j user_data)<-get             
             let whs_e : WhsEntry
                 whs_e = MkWE ref je
                 
                 key : MoveKey                 
                 key = (MkMK f t ledger)
                 
                 kf : (Location, Ledger)
                 kf = (f,ledger)
                                  
                 kt : (Location, Ledger)
                 kt = (t,ledger)
             
                 Hom11_2_LM : Hom11 -> LocationMap
                 Hom11_2_LM h11 = led2'' where
                    led1' : LocationMap
                    led1' = update_ledger kf ( dx h11) led_map
                    led1'' : LocationMap
                    led1'' = update_ledger kf (invHom1 $ cx h11) led1'
                    led2' : LocationMap
                    led2' = update_ledger kt (invHom1 $ dx h11) led1''
                    led2'' : LocationMap
                    led2'' = update_ledger kt (cx h11) led2'
                 
                 je2lm : FxEvent -> LocationMap
                 je2lm (Fx121 d h121 ) = Hom11_2_LM ( fromH121 h121 ) --(MkH11 (dx h121) (cx h121) )
                 je2lm (Fx11  d h11 ) = Hom11_2_LM h11
                 
                 led' : LocationMap
                 led' = je2lm je
                
             case (lookup key rjm) of
                Nothing => do
                   let rjm' = insert key [whs_e] rjm
                   put (MkSS fx_map routes led' rjm' j user_data)
                   
                Just je_list => do
                   let rjm' = insert key (whs_e::je_list) rjm
                   put (MkSS fx_map routes led' rjm' j user_data)
             pure ()
             
interpret (Get key) = do 
     (MkSS fx_map routes led_map rjm j user_data)<-get
     let muf1 : Maybe (List WhsEntry)
         muf1 = (lookup key rjm)
     case muf1 of
        Just xs => pure xs
        Nothing => pure []

interpret (Log x) = do
     (MkSS fx_map routes led_map rjm js user_data)<-get
     let js'= (x::js)
     putStrLn $ show x --pure () --?interpret_rhs_1
     putStrLn ""
     put (MkSS fx_map routes led_map rjm js' user_data)
     
interpret (Show x) = putStrLn $ show x --pure () --?interpret_rhs_2
interpret (Pure x) = pure x
interpret (Bind x f) = do res <- interpret x
                          interpret (f res)


