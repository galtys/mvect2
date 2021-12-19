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
import Odoo.Schema.PJB

%language ElabReflection

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
         amount_tax = (PKPrice GBP TAXAMOUNT,833), --MkPrice { tax = TAXAMOUNT, price = 833 }, 
         state = "manual", 
         partner_invoice_id = 31587, 
         amount_untaxed = (PKPrice GBP EX20, 4165), --MkPrice { tax = EX20, price = 4165 }, 
         amount_total = (PKPrice GBP INC20, 4998), --MkPrice { tax = INC20, price = 4998 }, 
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
               price_unit = 999, 
               product_qty = 1, 
               product_id = Just 726, 
               location_id = Just 12, 
               location_dest_id = Just 9, 
               picking_id = Just 43747, 
               state = "confirmed" }, 
           MkRecordModel 
             { pk = 163196, 
               origin = Just "SO44907", 
               price_unit = 211, 
               product_qty = 8, 
               product_id = Just 2932, 
               location_id = Just 12, 
               location_dest_id = Just 9, 
               picking_id = Just 43747, 
               state = "confirmed" }, 
           MkRecordModel 
             { pk = 163197, 
               origin = Just "SO44907", 
               price_unit = 88, 
               product_qty = 8, 
               product_id = Just 2852, 
               location_id = Just 12, 
               location_dest_id = Just 9, 
               picking_id = Just 43747, 
               state = "confirmed" }, 
           MkRecordModel 
             { pk = 163198, 
               origin = Just "SO44907", 
               price_unit = 199, 
               product_qty = 1, 
               product_id = Just 733, 
               location_id = Just 12, 
               location_dest_id = Just 9, 
               picking_id = Just 43747, 
               state = "confirmed" }, 
           MkRecordModel 
               { pk = 163199, 
                 origin = Just "SO44907", 
                 price_unit = 734, 
                 product_qty = 2, 
                 product_id = Just 2942, 
                 location_id = Just 12, 
                 location_dest_id = Just 9, 
                 picking_id = Just 43747, 
                 state = "confirmed" }, 
           MkRecordModel { 
                 pk = 163200, 
                 origin = Just "SO44907", 
                 price_unit = 95, 
                 product_qty = 2, 
                 product_id = Just 3531, 
                 location_id = Just 12, 
                 location_dest_id = Just 9, 
                 picking_id = Just 43747, 
                 state = "confirmed" }, 
           MkRecordModel { 
                 pk = 163192, 
                 origin = Just "SO44907", 
                 price_unit = 139, 
                 product_qty = 1, 
                 product_id = Just 4085, 
                 location_id = Just 12, 
                 location_dest_id = Just 9, 
                 picking_id = Just 43747, 
                 state = "assigned" }, 
           MkRecordModel { 
                 pk = 163193, 
                 origin = Just "SO44907", 
                 price_unit = 69, 
                 product_qty = 2, 
                 product_id = Just 4089, 
                 location_id = Just 12, 
                 location_dest_id = Just 9, 
                 picking_id = Just 43747, 
                 state = "assigned" }, 
           MkRecordModel { 
                 pk = 163194, 
                 origin = Just "SO44907", 
                 price_unit = 49, 
                 product_qty = 1, 
                 product_id = Just 4095, 
                 location_id = Just 12, 
                 location_dest_id = Just 9, 
                 picking_id = Just 43747, 
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
hilton : Address
hilton = MkA "Street" "" "London" "SU 4X" UK (MkC "Hilton")

export
factory1 : Address
factory1 = MkA "Factory street" "" "Asia" "44AX" UK (MkC "Factory1")

export
factory2 : Address
factory2 = MkA "Factory street2" "" "Asia2" "X" UK (MkC "Factory2")

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
     fx = MkFx date Purchase factory1 factory1 (MkH121 h1 [] h2 (apply2' h2 h1) ) 
     
     h1' : Hom1
     h1' = map (mult_p 10) [p4]
     h2' : Hom2
     h2' = [ (fst p4, toEX20 15.43)]
     fx' : FxData
     fx' = MkFx date Purchase factory2 factory2 (MkH121 h1' [] h2' (apply2' h2' h1') ) 
     
 rew_r <- Open fx
 rew_r' <- Open fx' 
 Pure ()

export
confirm_so : OwnerEvent ()
confirm_so = do
 let date = "2021-11-01"
     h1 = [p1,p2,p3,p4]
     up1 = (toEX20 31.73)
     up2 = (toEX20 15.03)
     up3 = (toEX20 25.00)
     up4 = (toEX20 21.00)     
     h2 = [ (fst p1, up1), 
            (fst p2, up2),
            (fst p3, up3),
            (fst p4, up3) ]
            
     fx = MkFx date Sale hilton hilton (MkH121 h1 [] h2 (apply2' h2 h1) ) 
 rew_r <- Open fx
 Pure ()



public export
LedgerMap  : Type
LedgerMap = SortedMap (Location, Ledger, ProdKey) EQty
public export
JournalMap  : Type
JournalMap = SortedMap (Location, Location,Ledger) (List JournalEvent)


export
RouteMap : Type
RouteMap = SortedMap (Date,RouteRef,RouteState) Route

public export
update_ledger : (Location, Ledger) -> Hom1 -> LedgerMap -> LedgerMap 
update_ledger k [] m = m
update_ledger k@(ct,l) ( (pk,eq)::xs) m = ret where
          key : (Location, Ledger,ProdKey)
          key = (ct,l,pk)
          
          ret : LedgerMap
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
         h121 = MkH121 h1 [] h2 (apply2' h2 h1)
         
         je : JournalEvent
         je = Fx121 (date, h121)
     ref <- Init initRoute je
     Pure ref

export
custWiRoute : (c:Address) -> (i:Address) -> List Location
custWiRoute c i = [Partner Sale c, Control Sale i, Self]

export
suppWiRoute : (s:Address) -> (i:Address) -> List Location
suppWiRoute s i = [Self, Control Purchase i, Partner Purchase s]

export
route2ft : Route -> List (Location,Location)
route2ft [] = []
route2ft (x::[]) = []
route2ft (x::y::xs) = [(x,y)]++(route2ft xs)
export
fillRoute : List (Location,Location) -> Ledger -> JournalEvent -> WhsEvent ()
fillRoute [] l je = Pure ()
fillRoute (x::xs) ledger je = do
     let (f,t) = x
     Put f t ledger je
     fillRoute xs ledger je
     
export
toWhs : OwnerEvent a -> WhsEvent a
toWhs (Init route je) = do     
       
       let je2dh : JournalEvent -> (Date, Hom11)
           je2dh (Fx121 (date, h121)) = (date, fromH121 h121)
           je2dh (Fx11  (date, h11)) = (date, h11)       
           --je2dh (Empty date) = (date , MkH11 [] [])
           ret : (Date,Hom11)
           ret = je2dh je
       
       ref <- NewRoute (fst ret) route
       -- todo: use route param to populate it
       let r_ft = route2ft route
       fillRoute r_ft OnHand je
       fillRoute r_ft Forecast je
       --put route into completed state
       
       Pure ref
       
toWhs (Open fx) = do
       let inv : Address
           inv = (invoice fx)
           del : Address
           del = (delivery fx)           
           route_c : Route
           route_c = custWiRoute del inv           
           route_s : Route
           route_s = suppWiRoute del inv
           je : JournalEvent
           je = Fx121 (date fx, h3 fx)
           
           je_inv : JournalEvent
           je_inv = Fx121 (date fx, MkH121 [] [] (appl $ h3 fx) [] )
           
       case (direction fx) of
           Purchase => do
               new_r <- NewRoute (date fx) route_s
               Put Self (Control Purchase inv) Forecast je_inv --empty invoice
               
               Put (Control Purchase inv) (Partner Purchase del) Forecast je               
               Pure new_r
           Sale => do
               new_r <- NewRoute (date fx) route_c           
               Put (Partner Sale del) (Control Sale inv) Forecast je
               Put (Control Sale inv) Self Forecast je_inv --empty invoice
               Pure new_r
       
toWhs (Log x) =  Pure ()
toWhs (Show x) = Pure ()
toWhs (Pure x) = Pure x
toWhs (Bind x f) = do res <- toWhs x
                      toWhs (f res) --?toWhs_rhs_4
     
export
interpret : WhsEvent a -> State (RouteMap,LedgerMap,JournalMap) a
interpret  (NewRoute date route) = do
             (routes,led_map,jm)<-get             
             let route_cnt = encode route
                 route_ref = sha256 route_cnt
                 routes' = insert (date, route_ref,Progress)  route routes
             put (routes', led_map,jm)
             pure route_ref            

interpret (CloseRoute date route_ref ) = do
            
            pure ()

             
interpret (Put f t ledger je) = do
             (routes,led_map,jm)<-get
             
             let key = (f,t, ledger)
                 kf : (Location, Ledger)
                 kf = (f,ledger)
                 
                 kt : (Location, Ledger)
                 kt = (t,ledger)
             
                 Hom11_2_LM : Hom11 -> LedgerMap
                 Hom11_2_LM h11 = led2'' where
                    led1' : LedgerMap
                    led1' = update_ledger kf ( dx h11) led_map
                    led1'' : LedgerMap
                    led1'' = update_ledger kf (invHom1 $ cx h11) led1'
                    led2' : LedgerMap
                    led2' = update_ledger kt (invHom1 $ dx h11) led1''
                    led2'' : LedgerMap
                    led2'' = update_ledger kt (cx h11) led2'
                 
                 je2lm : JournalEvent -> LedgerMap
                 je2lm (Fx121 (d,h121) ) = Hom11_2_LM ( fromH121 h121 ) --(MkH11 (dx h121) (cx h121) )
                 je2lm (Fx11  (d,h11)) = Hom11_2_LM h11
                 
                 led' : LedgerMap
                 led' = je2lm je
                
             case (lookup key jm) of
                Nothing => do
                   let jm' = insert key [je] jm
                   put (routes,led',jm')
                   
                Just je_list => do
                   let jm' = insert key (je::je_list) jm
                   put (routes,led',jm')
             pure ()
        
             
interpret (Log x) = pure () --?interpret_rhs_1
interpret (Show x) = pure () --?interpret_rhs_2
interpret (Pure x) = pure x
interpret (Bind x f) = do res <- interpret x
                          interpret (f res)




export
test_demo2 : IO ()
test_demo2 = do
  
  pure ()
