module Category.Transaction.Owner

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

--%language ElabReflection

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
print_group : List BrowseStockMove.RecordModel -> IO ()  
print_group xs = do
  retMvs <- execStateT empty (moveMap xs) 
  let ocas = [ (x,Prelude.List.length y) | (x,y) <- Data.SortedMap.toList retMvs ]
  traverse_ printLn ocas
  
  pure ()
  
export
mult_p : EQty -> Product -> Product
mult_p x (k,q) = (k,x*q)
export
new_po : Date->Hom1->BrowseResPartner.RecordModel->BrowseResPartner.RecordModel->OwnerEvent RouteKey 
new_po date1 dx1 supp invoice = do
 user_data  <- GetUserData 
 let prod_map = products user_data
     h2 : Hom1 -> Hom2
     h2 dx_' = [ (fst x, mult_p 0.9 (trade_price (lookup (fst x) prod_map)) ) |x <- dx_' ]
     
     cx1 : Hom1
     cx1 = (applyHom2 (h2 dx1) dx1)     
     h11_1 : Hom11
     h11_1 = MkH11 dx1 cx1          
     po1 : FxData
     po1 = MkFx date1 Purchase factory1 factory1 (MkH121 dx1 [] (h2 dx1) cx1 h11_1)
 
 new_r <- ConfirmOrder po1
 r <- GetRoute new_r
 case r of
   Nothing => Pure ()
   Just rt => do       
       x <- Get $ allocationMove rt
       let aitem : AllocationItem
           aitem = MkAI new_r InventoryRouteKey (Fx11 date1 x)       
       --aref <- Allocate (MkAE OnHand [aitem])
       aref <- Allocate (MkAE Forecast [aitem])       
       x <- Get (convMovekey $allocationMove rt)
       Show "Can be allocated"
       Show x
 Pure new_r

export
transit_po_full : RouteKey -> Date -> OwnerEvent ()
transit_po_full rk date1 = do
 m_rst <- GetRoute rk
 case m_rst of
   Nothing => Pure ()
   Just rt => do
       case rt of
          (MkOR (MkORrec allocation control order Sale)) => Pure ()
          (MkOR (MkORrec allocation control order Purchase)) => do
               let transit_key  = convMovekey order                   
               xfc <- Get order --transit_fcast_key
               Post rk transit_key (Fx11 date1 xfc)                         
          (MkReR re) => Pure ()
          (MkAl lr) => Pure () 
export
receive_po_full : RouteKey -> Date -> OwnerEvent ()
receive_po_full rk date1 = do
 m_rst <- GetRoute rk
 case m_rst of
   Nothing => Pure ()
   Just rt => do
       case rt of
          (MkOR (MkORrec allocation control order Sale)) => Pure ()
          (MkOR (MkORrec allocation control order Purchase)) => do
               let po_invoice_key = control
                   recv_key  = convMovekey po_invoice_key  
               x <- Get po_invoice_key               
               let fx11 : FxEvent
                   fx11 =  (Fx11 date1 x)       
                   aitem : AllocationItem
                   aitem = MkAI rk InventoryRouteKey fx11
               Post rk recv_key fx11
               aref <- Allocate (MkAE OnHand [aitem])
               Pure ()
          (MkReR re) => Pure ()
          (MkAl lr) => Pure () 

export
reserve_so_full : RouteKey -> Date -> OwnerEvent ()
reserve_so_full rk date1 = do
  m_rst <- GetRoute rk
  case m_rst of
    Nothing => Pure ()
    Just rt => do
       case rt of
          (MkOR (MkORrec allocation control order Sale)) => do         
               let so_demand_key = allocation
                   reservation_key  = convMovekey so_demand_key  
               x <- Get so_demand_key                             
               let fx11 : FxEvent
                   fx11 =  (Fx11 date1 x)       
                   aitem : AllocationItem
                   aitem = MkAI rk InventoryRouteKey fx11
               aref <- Allocate (MkAE OnHand [aitem])               
               Pure ()          
          (MkOR (MkORrec allocation control order Purchase)) => Pure ()                    
          (MkReR re) => Pure ()
          (MkAl lr) => Pure () 
export
deliver_so_full : RouteKey -> Date -> OwnerEvent ()
deliver_so_full rk date1 = do
  m_rst <- GetRoute rk
  case m_rst of
    Nothing => Pure ()
    Just rt => do
       case rt of
          (MkOR (MkORrec allocation control order Sale)) => do
               let so_invoice_key = control 
                   so_demand_key = allocation 
                   so_delivery_key  = convMovekey so_invoice_key  
               x <- Get so_demand_key                             
               let fx11 : FxEvent
                   fx11 =  (Fx11 date1 x)       
               Post rk so_delivery_key fx11
               Pure ()                    
          (MkOR (MkORrec allocation control order Purchase)) => Pure ()                    
          (MkReR re) => Pure ()
          (MkAl lr) => Pure () 

export
invoice_so_full : RouteKey -> Date -> OwnerEvent ()
invoice_so_full rk date1 = do
  m_rst <- GetRoute rk
  --TODO: Use Fx121
  case m_rst of
    Nothing => Pure ()
    Just rt => do
       case rt of
          (MkOR (MkORrec allocation control order Sale)) => do --Pure ()
               let so_invoice_key = control --so
                   so_delivery_key  = convMovekey so_invoice_key  
               x <- Get so_delivery_key                             
               let fx11 : FxEvent
                   fx11 =  (Fx11 date1 x)       
               Post rk so_invoice_key fx11
               Pure ()
          (MkOR (MkORrec allocation control order Purchase)) => Pure ()                    
          (MkReR re) => Pure ()
          (MkAl lr) => Pure () 
export
shipping_done_so_full : RouteKey -> Date -> OwnerEvent ()
shipping_done_so_full rk date1 = do
  m_rst <- GetRoute rk
  case m_rst of
    Nothing => Pure ()
    Just rt => do
       case rt of
          (MkOR (MkORrec allocation control order Sale)) => do --Pure ()
               let so_invoice_key = control --so
                   so_delivery_key  = convMovekey so_invoice_key 
                   so_sale_order_key = order --so
                   so_shipping_key =  convMovekey so_sale_order_key                    
               x <- Get so_delivery_key                             
               let fx11 : FxEvent
                   fx11 =  (Fx11 date1 x)       
               Post rk so_shipping_key fx11
               Pure ()                    
          (MkOR (MkORrec allocation control order Purchase)) => Pure ()                    
          (MkReR re) => Pure ()
          (MkAl lr) => Pure () 


export
new_so : Date->Hom1->BrowseResPartner.RecordModel->BrowseResPartner.RecordModel->OwnerEvent RouteKey
new_so date1 dx1 cust cust_inv = do
 user_data  <- GetUserData      
 let bom_map : SortedMap ProdKey (List BrowseBoM.RecordModel)
     bom_map = boms_m user_data
     
     prod_map : SortedMap ProdKey  BrowseProduct.RecordModel
     prod_map = products user_data     
     h2 : Hom1 -> Hom2
     h2 dx_' = [ (fst x,  trade_price (lookup (fst x) prod_map)) |x <- dx_' ]
     
     dx1 : Hom1 
     dx1 = [ (PK32 DX 1, 1), (PK32 DX 3, 1), (PK32 DX 4, 2)]     
     cx1 : Hom1
     cx1 = (applyHom2 (h2 dx1) dx1)     
     h11_1 : Hom11
     h11_1 = MkH11 dx1 cx1          
     
     h1_bom : List BoM32
     h1_bom = map_to_BoM32 dx1 bom_map
     tax : Hom1
     tax = applyHom2Tax (h2 dx1) dx1           
     cx : Hom1
     cx = cx1 + tax     
     fx : FxData
     fx = MkFx date1 Sale cust cust_inv (MkH121 dx1 h1_bom (h2 dx1) cx h11_1) 
 
 new_r <- ConfirmOrder fx
 r <- GetRoute new_r
 case r of
   Nothing => Pure ()
   Just rt => do
       x <- Get $ allocationMove rt
       let aitem : AllocationItem
           aitem = MkAI new_r InventoryRouteKey (Fx11 date1 x)  
       aref <- Allocate (MkAE Forecast [aitem])                   
       Show aref
 ff <- Get $ allocation InitRoute
 aa <- Get $ convMovekey $allocation InitRoute
 Pure new_r

export
init_self : WhsEvent () 
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
toWhs : OwnerEvent a -> WhsEvent a
toWhs (ConfirmOrder fx) = do
       Log (MkOpen fx)       
       let fx_ev : FxEvent
           fx_ev = Fx121 (date fx) (h3 fx)
           fx_empty : FxEvent
           fx_empty = Fx121 (date fx) (MkH121 [] [] (appl $ h3 fx) [] emptyHom11)
           
           so : OrderControlRoute
           so = soForecastFromFx fx
           po : OrderControlRoute
           po = poForecastFromFx fx
           
       case (direction fx) of
           Purchase => do
               new_r <- NewRoute (date fx) (MkOR po)
               SetFxData new_r fx
               let route_key = MkRouteKeyRef new_r
               Put route_key  (control po) fx_empty               
               Put route_key  (order po) fx_ev
               Pure new_r
           Sale => do
               new_r <- NewRoute (date fx) (MkOR so)
               SetFxData new_r fx               
               let route_key = MkRouteKeyRef new_r               
               Put route_key (order so) fx_ev               
               Put route_key (control so) fx_empty
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
                      
           muf2 : AllocationItem -> WhsEvent (Maybe (RouteSumT,RouteSumT,FxEvent))
           muf2 ai =  do
               rf <- GetRoute (supplier ai)
               rt <- GetRoute (customer ai)
               case (rf,rt) of
                  (Just rx, Just ry) => Pure (Just (rx,ry,fx ai))
                  _ => Pure Nothing
                  
           allocateItem : (RouteSumT,RouteSumT,FxEvent) -> WhsEvent () 
           -- Maybe (RouteKey, RouteKey, FxEvent)
           allocateItem (rx,ry,fe) = do
               case ledger of
                  Forecast => do
                    Put a_ref (allocationMove rx) fe
                    Put a_ref (allocationMove ry) fe
                  OnHand => do
                    Put a_ref (convMovekey $allocationMove rx) fe
                    Put a_ref (convMovekey $allocationMove ry) fe
                           
           
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
       
       Pure a_ref
       
toWhs (Show x) = Show x --Pure ()
toWhs (Pure x) = Pure x
toWhs (Bind x f) = do res <- toWhs x
                      toWhs (f res) --?toWhs_rhs_4