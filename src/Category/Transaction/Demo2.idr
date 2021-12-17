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
%language ElabReflection

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
     h2 = [ (fst p1, ("GBP",up1)), 
            (fst p2, ("GBP",up2)),
            (fst p3, ("GBP",up3))]
     fx = MkFx date Purchase factory1 factory1 (MkH121 h1 h2 (apply2' h2 h1) ) 
     
     h1' : Hom1
     h1' = map (mult_p 10) [p4]
     h2' : Hom2
     h2' = [ (fst p4, ("GBP", toEX20 15.43) )]
     fx' : FxData
     fx' = MkFx date Purchase factory2 factory2 (MkH121 h1' h2' (apply2' h2' h1') ) 
     
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
     h2 = [ (fst p1, ("GBP",up1)), 
            (fst p2, ("GBP",up2)),
            (fst p3, ("GBP",up3)),
            (fst p4, ("GBP",up3)) ]
            
     fx = MkFx date Sale hilton hilton (MkH121 h1 h2 (apply2' h2 h1) ) 
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
         price : Price
         price = (toEX20 1000)
         
         share : Product
         share = ("company_shares",100)
         h2 : Hom2
         h2 = [ (fst share, ("GBP",price)) ]
         h1 : Hom1
         h1 = [share]
         
         h121 : Hom121
         h121 = MkH121 h1 h2 (apply2' h2 h1)
         
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
           je_inv = Fx121 (date fx, MkH121 [] (appl $ h3 fx) [] )
           
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
