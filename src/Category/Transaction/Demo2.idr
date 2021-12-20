module Category.Transaction.Demo2

import Data.Zippable
import Generics.Derive
import Data.SortedMap
import Control.Monad.State

import JSON

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Types2
import Category.Transaction.Hom
import Category.Transaction.Journal
import Crypto.Hash.SHA256
import Data.Ratio
import Odoo.Schema.PJBRecDef
--import Odoo.Schema.PJB

%language ElabReflection



{-
public export
LocationMap  : Type
LocationMap = SortedMap (Location, Ledger, ProdKey) EQty
public export
RouteJournalMap  : Type
RouteJournalMap = SortedMap (Location, Location,Ledger) (List JournalEvent)
export
RouteMap : Type
RouteMap = SortedMap (Date,RouteRef,RouteState) Route

public export
record SystemState where
   constructor MkSS
   routes : RouteMap
   led_map : LocationMap
   jm   : RouteJournalMap


-}


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
{-
export   
runXdd : List BrowseStockMove.RecordModel -> IO StockMoveMap
runXdd xs = execStateT empty (moveMap xs)
-}
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
     fx = MkFx date Purchase factory1 factory1 (MkH121 h1 [] h2 (applyHom2 h2 h1) ) 
     
     h1' : Hom1
     h1' = map (mult_p 10) [p4]
     h2' : Hom2
     h2' = [ (fst p4, toEX20 15.43)]
     fx' : FxData
     fx' = MkFx date Purchase factory2 factory2 (MkH121 h1' [] h2' (applyHom2 h2' h1') ) 
     
 rew_r <- Open fx
 rew_r' <- Open fx' 
 Pure ()
export
initRoute : List Location
initRoute = [Init, Self]

export
init_self : OwnerEvent RouteRef
init_self = do
     let date = "2010-01-15"
         price : Product
         price = (toEX20 1000)
         
         share : Product
         share = ("GTX43",100)
         h2 : Hom2
         h2 = [ (fst share, price) ]
         h1 : Hom1
         h1 = [share]
         
         h121 : Hom121
         h121 = MkH121 h1 [] h2 (applyHom2 h2 h1)
         
         je : FxEvent
         je = Fx121 (date, h121)
     ref <- Init initRoute je emptyUserData
     Pure ref

export
confirm_so : OwnerEvent ()
confirm_so = do
 iref <- init_self
 let date = "2021-11-01"
     --h1 = [p1,p2,p3,p4]
     h1 = qtyFromOrderLine (order_line so_44970)
     {-
     up1 = (toEX20 31.73)
     up2 = (toEX20 15.03)
     up3 = (toEX20 25.00)
     up4 = (toEX20 21.00)     
     h2 = [ (fst p1, up1), 
             (fst p2, up2),
            (fst p3, up3),
            (fst p4, up3) ]
     -}
     h2 = priceFromOrderLine (order_line so_44970)
            
     fx = MkFx date Sale hilton hilton (MkH121 h1 [] h2 (applyHom2 h2 h1) ) 
     
 rew_r <- Open fx
 --Log rew_r
 
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

export
validateDirection : (from:Location) -> (to:Location) -> Bool
validateDirection (Partner Sale y) (Control Sale x) = True
validateDirection (Control Sale y) Self = True
validateDirection Self (Control Purchase y) = True
validateDirection (Control Purchase y) (Partner Purchase x) = True
validateDirection Init Self = True
validateDirection _ _ = False



export
custWiRoute : (c:BrowseResPartner.RecordModel) -> (i:BrowseResPartner.RecordModel) -> List Location
custWiRoute c i = [Partner Sale c, Control Sale i, Self]

export
suppWiRoute : (s:BrowseResPartner.RecordModel) -> (i:BrowseResPartner.RecordModel) -> List Location
suppWiRoute s i = [Self, Control Purchase i, Partner Purchase s]

export
route2ft : Route -> Ledger -> List MoveKey --(Location,Location)
route2ft [] l = []
route2ft (x::[]) l= []
route2ft (x::y::xs) l = [(MkMK x y l)]++(route2ft xs l)

export
fillRoute : List MoveKey -> FxEvent -> WhsEvent ()
fillRoute [] fxe = Pure ()
fillRoute (mk::xs) fxe = do
     Put mk fxe
     fillRoute xs fxe
     
export
toWhs : OwnerEvent a -> WhsEvent a
toWhs (Init route je  user_data) = do  
       UpdateUserData user_data
       Log (MkUserUpdate user_data)
       Log (MkNewRoute route je)
       
       let je2dh : FxEvent -> (Date, Hom11)
           je2dh (Fx121 (date, h121)) = (date, fromH121 h121)
           je2dh (Fx11  (date, h11)) = (date, h11)       
           --je2dh (Empty date) = (date , MkH11 [] [])
           ret : (Date,Hom11)
           ret = je2dh je       
           
       ref <- NewRoute (fst ret) route
       -- todo: use route param to populate it
       let r_ft_onhand = route2ft route OnHand
           r_ft_forecast = route2ft route Forecast
       fillRoute r_ft_onhand je       
       fillRoute r_ft_forecast je
       Pure ref
toWhs (UpdateUserData user_data) = do
       UpdateUserData user_data
       Log (MkUserUpdate user_data)
       
toWhs (GetUserData) = do
       ret <- GetUserDataW
       Pure ret 
                     
toWhs (Open fx) = do
       Log (MkOpen fx)
       let inv : BrowseResPartner.RecordModel
           inv = (invoice fx)
           del : BrowseResPartner.RecordModel
           del = (delivery fx)           
           route_c : Route
           route_c = custWiRoute del inv           
           route_s : Route
           route_s = suppWiRoute del inv
           je : FxEvent
           je = Fx121 (date fx, h3 fx)           
           je_inv : FxEvent
           je_inv = Fx121 (date fx, MkH121 [] [] (appl $ h3 fx) [] )           
       case (direction fx) of
           Purchase => do
               new_r <- NewRoute (date fx) route_s
               Put (MkMK Self (Control Purchase inv) Forecast) je_inv --empty invoice
               
               Put (MkMK (Control Purchase inv) (Partner Purchase del) Forecast) je               
               Pure new_r
           Sale => do
               new_r <- NewRoute (date fx) route_c           
               Put (MkMK (Partner Sale del) (Control Sale inv) Forecast) je
               Put (MkMK (Control Sale inv) Self Forecast) je_inv --empty invoice
               Pure new_r
toWhs (Post ref key fx) = do
      Put key fx
      Log (MkPost ref key fx)      
      
toWhs (Close date ref) = do
       CloseRoute date ref
       Log (MkClose date ref)
       
toWhs (Allocate entry) = do
       Log (MkAEntry entry)       
       
toWhs (Show x) = Show x --Pure ()
toWhs (Pure x) = Pure x
toWhs (Bind x f) = do res <- toWhs x
                      toWhs (f res) --?toWhs_rhs_4
                      

                      
     --(RouteMap,LocationMap,RouteJournalMap)
export
interpret : WhsEvent a -> StateT SystemState IO a
interpret  (NewRoute date route) = do
             (MkSS routes led_map rjm j user_data)<-get             
             let route_cnt = encode route
                 route_ref = sha256 route_cnt
                 r_k : RouteKey --(Date,RouteRef,RouteState)
                 r_k = (MkRK date route_ref Progress)                 
                 routes' : SortedMap RouteKey Route
                 routes' = insert r_k  route routes                
             put (MkSS routes' led_map rjm j user_data)
             pure route_ref

interpret (UpdateUserData user_data ) = do
             (MkSS routes led_map rjm j udm)<-get
             let udm' = userDataToMap user_data
             put (MkSS routes led_map rjm j udm')
             
interpret (GetUserDataW ) = do
             (MkSS routes led_map rjm j user_data_map)<-get
             pure user_data_map
             
interpret (CloseRoute date route_ref ) = do            
            pure ()             
interpret (Put (MkMK f t ledger) je) = do
             (MkSS routes led_map rjm j user_data)<-get             
             let key = (MkMK f t ledger)
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
                 je2lm (Fx121 (d,h121) ) = Hom11_2_LM ( fromH121 h121 ) --(MkH11 (dx h121) (cx h121) )
                 je2lm (Fx11  (d,h11)) = Hom11_2_LM h11
                 
                 led' : LocationMap
                 led' = je2lm je
                
             case (lookup key rjm) of
                Nothing => do
                   let rjm' = insert key [je] rjm
                   put (MkSS routes led' rjm' j user_data)
                   
                Just je_list => do
                   let rjm' = insert key (je::je_list) rjm
                   put (MkSS routes led' rjm' j user_data)
             pure ()                     
interpret (Log x) = do
     (MkSS routes led_map rjm js user_data)<-get
     let js'= (x::js)
     putStrLn $ show x --pure () --?interpret_rhs_1
     putStrLn ""
     put (MkSS routes led_map rjm js' user_data)
     
interpret (Show x) = putStr $ show x --pure () --?interpret_rhs_2
interpret (Pure x) = pure x
interpret (Bind x f) = do res <- interpret x
                          interpret (f res)



export
test_demo2 : IO ()
test_demo2 = do
  
  reas <- execStateT initState (interpret (toWhs   confirm_so)   )
  --printLn reas
  pure ()
