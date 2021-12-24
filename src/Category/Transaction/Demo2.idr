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
import Odoo.PG.BoM

%language ElabReflection

safeHead : List x -> Maybe x
safeHead [] = Nothing
safeHead (y :: xs) = Just y

export
userDataToMap : UserData -> UserDataMap
userDataToMap (MkUD p t b tax) = (MkUDMap p_map t_map b_map tax_map) where
     p_map : SortedMap Bits32  BrowseProduct.RecordModel
     p_map = (fromList [(pk u, u) | u <- p ])
     t_map : SortedMap Bits32 BrowseProductTemplate.RecordModel
     t_map = (fromList [(pk u, u) | u <- t ])
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
sti20 : PrimOrderTax.RecordModel
sti20 = PrimOrderTax.MkRecordModel 
      { pk = 14, 
        name = "Standard rate sales IncVAT (20%)", 
        description = Just "STI20", 
        amount = 1/5, 
        type = Just "percent", 
        price_include = Just True }

export
so_44970 : BrowseOrder.RecordModel
so_44970 = BrowseOrder.MkRecordModel 
       { pk = 44970, 
         origin = Nothing, 
         order_policy = "manual", 
         date_order = "2021-11-18", 
         partner_id = 31587, 
         amount_tax = (PKPrice CX GBP TAXAMOUNT,833), --MkPrice { tax = TAXAMOUNT, price = 833 }, 
         state = "manual", 
         partner_invoice_id = 31587, 
         amount_untaxed = (PKPrice CX GBP EX20, 4165), --MkPrice { tax = EX20, price = 4165 }, 
         amount_total = (PKPrice CX GBP INC20, 4998), --MkPrice { tax = INC20, price = 4998 }, 
         name = "SO44907", 
         partner_shipping_id = 31587, 
         picking_policy = "direct", 
         carrier_id = Just 7, 
         order_line = 
             [MkRecordModel { pk = 176369, 
                              price_unit = 0, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 0), 
                              delivery_line = Just True, 
                              order_id = 44970, 
                              product_id = Just 735, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176363, 
                              price_unit = 3199, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 0), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 1042, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176364, 
                              price_unit = 1799, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 0), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 1064, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176366,
                              price_unit = 139, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 100), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 4085, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176367, 
                              price_unit = 69, 
                              product_uom_qty = 2, 
                              discount = Just (EQVal EPercent 100), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 4089, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176368, 
                              price_unit = 49, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 100), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 4095, 
                              tax_ids = [sti20]} ] , 
         requested_date = Nothing }
         
export
sp_43747 : BrowseStockPicking.RecordModel
sp_43747 = MkRecordModel 
      { pk = 43747, 
        origin = Just "SO44907", 
        backorder_id = Nothing, 
        date_done = "", 
        partner_id = Just 31587, 
        min_date = "2021-11-19 12:00:00", 
        name = "OUT40884", 
        state = "assigned", 
        move_ids = 
          [MkRecordModel 
             { pk = 163195, 
               origin = Just "SO44907", 
               price_unit = Just 999, 
               product_qty = 1, 
               product_id =  726, 
               location_id =  12, 
               location_dest_id =  9, 
               picking_id = Just 43747, 
               purchase_line_id = 1,
               sale_line_id = 1,
               state = "confirmed" }, 
           MkRecordModel 
             { pk = 163196, 
               origin = Just "SO44907", 
               price_unit = Just 211, 
               product_qty = 8, 
               product_id =  2932, 
               location_id =  12, 
               location_dest_id =  9, 
               picking_id = Just 43747, 
               purchase_line_id = 1,
               sale_line_id = 1,
               state = "confirmed" }, 
           MkRecordModel 
             { pk = 163197, 
               origin = Just "SO44907", 
               price_unit = Just 88, 
               product_qty = 8, 
               product_id =  2852, 
               location_id =  12, 
               location_dest_id =  9, 
               picking_id = Just 43747, 
               purchase_line_id = 1,
               sale_line_id = 1,
               state = "confirmed" }, 
           MkRecordModel 
             { pk = 163198, 
               origin = Just "SO44907", 
               price_unit = Just 199, 
               product_qty = 1, 
               product_id =  733, 
               location_id =  12, 
               location_dest_id =  9, 
               picking_id = Just 43747, 
               purchase_line_id = 1,
               sale_line_id = 1,
               state = "confirmed" }, 
           MkRecordModel 
               { pk = 163199, 
                 origin = Just "SO44907", 
                 price_unit = Just 734, 
                 product_qty = 2, 
                 product_id =  2942, 
                 location_id =  12, 
                 location_dest_id =  9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,
                 state = "confirmed" }, 
           MkRecordModel { 
                 pk = 163200, 
                 origin = Just "SO44907", 
                 price_unit = Just 95, 
                 product_qty = 2, 
                 product_id =  3531, 
                 location_id =  12, 
                 location_dest_id =  9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,
                 state = "confirmed" }, 
           MkRecordModel { 
                 pk = 163192, 
                 origin = Just "SO44907", 
                 price_unit = Just 139, 
                 product_qty = 1, 
                 product_id =  4085, 
                 location_id =  12, 
                 location_dest_id = 9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,
                 state = "assigned" }, 
           MkRecordModel { 
                 pk = 163193, 
                 origin = Just "SO44907", 
                 price_unit = Just 69, 
                 product_qty = 2, 
                 product_id =  4089, 
                 location_id =  12, 
                 location_dest_id =  9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,                 
                 state = "assigned" }, 
           MkRecordModel { 
                 pk = 163194, 
                 origin = Just "SO44907", 
                 price_unit = Just 49, 
                 product_qty = 1, 
                 product_id =  4095, 
                 location_id =  12, 
                 location_dest_id =  9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,                 
                 state = "assigned" }] }
export
va_43244 : BrowseAccountVoucher.RecordModel 
va_43244 = BrowseAccountVoucher.MkRecordModel 
         { pk = 43244, 
           number = "WALT1248", 
           partner_id = Just 31587, 
           journal_id = Just 23, 
           amount = 4998 }
export
retail_cust_31587 : BrowseResPartner.RecordModel
retail_cust_31587 = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "John Retail1", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Sharing House", 
         contract = Just False, 
         city = Just "Mid London", 
         zip = Just "CE5 CE6", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "retail_cust1@btconnect.com", 
         street2 = Just "Mid Lane" }

export
hilton : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
hilton = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "John Hilton", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Sharing Hilton House", 
         contract = Just False, 
         city = Just "Mid London", 
         zip = Just "CE6", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "hilton@btconnect.com", 
         street2 = Just "Mid Lane" }
         --retail_cust_31587 --MkA "Street" "" "London" "SU 4X" UK (MkC "Hilton")


export
factory1 : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
factory1 = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "factory 1", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Sharing House", 
         contract = Just False, 
         city = Just "Mid London", 
         zip = Just "CE5 CE6", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "retail_cust1@btconnect.com", 
         street2 = Just "Mid Lane" }
         --retail_cust_31587 --MkA "Factory street" "" "Asia" "44AX" UK (MkC "Factory1")

export
factory2 : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
factory2 = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "factory 2", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Sharing House", 
         contract = Just False, 
         city = Just "Mid London", 
         zip = Just "CE5 CE6", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "retail_cust1@btconnect.com", 
         street2 = Just "Mid Lane" }
         --retail_cust_31587  --MkA "Factory street2" "" "Asia2" "X" UK (MkC "Factory2")

export
p1:Product
p1 = ("p1",2)
export
p2:Product
p2 = ("p2",7)
export
p3:Product
p3 = ("p1",10)

export
p4:Product
p4 = ("p4",10)

export
mult_p : EQty -> Product -> Product
mult_p x (k,q) = (k,x*q)

export
confirm_po : OwnerEvent ()
confirm_po = do
 let date = "2021-10-01"
     h1 = map (mult_p 10) [p1,p2,p3]     
     up1 = (toEX20 21.73)
     up2 = (toEX20 11.03)
     up3 = (toEX20 17.00)     
     h2 = [ (fst p1, up1 ), 
            (fst p2, up2 ),
            (fst p3, up3 )]
     
     
     
     fx = MkFx date Purchase factory1 factory1 (MkH121 h1 [] h2 (applyHom2 h2 h1) emptyHom11) 
     
     h1' : Hom1
     h1' = map (mult_p 10) [p4]
     h2' : Hom2
     h2' = [ (fst p4, toEX20 15.43)]
     fx' : FxData
     fx' = MkFx date Purchase factory2 factory2 (MkH121 h1' [] h2' (applyHom2 h2' h1') emptyHom11) 
     
 rew_r <- ConfirmOrder fx
 rew_r' <- ConfirmOrder fx' 
 Pure ()


export
init_self : WhsEvent () --RouteRef
init_self = do
     UpdateUserData emptyUserData
     Log (MkUserUpdate emptyUserData)

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
{-
export
route2ft : Route -> Ledger -> List MoveKey --(Location,Location)
route2ft [] l = []
route2ft (x::[]) l= []
route2ft (x::y::xs) l = [(MkMK x y l)]++(route2ft xs l)

export
fillRoute : Ref -> List MoveKey -> FxEvent -> WhsEvent ()
fillRoute ref [] fxe = Pure ()
fillRoute ref (mk::xs) fxe = do
     Put ref mk fxe
     fillRoute ref xs fxe
-}
export
confirm_so : OwnerEvent ()
confirm_so = do
 Init
 
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



export
test_demo2 : IO ()
test_demo2 = do
  
  reas <- execStateT initState (interpret (toWhs   confirm_so)   )
  --printLn reas
  pure ()
