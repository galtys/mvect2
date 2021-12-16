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

public export
tree:TreeB
tree= Node (Node (Leaf "1 ") "2 " (Leaf "3 "))
           "4 "
           (Leaf "5 ")
public export
show1:TreeB-> String
show1 (Leaf s)=s
show1 (Node l s r) = 
    show1 l ++ s ++ show1 r

-- cps
public export
show2 : TreeB -> (String->a) -> a
show2 (Leaf s) k = k s
show2 (Node lft s rgt) k= 
  show2 lft (\ls => 
     show2 rgt (\rs => 
       k (ls++s++rs)))

mutual
  done : String -> String
  done x = x

  next : (String,TreeB,(String->String))-> String -> String
  next (s,rgt,k) ls = show3 rgt (cont (ls,s,k))

  cont: (String,String,(String->String))-> String -> String
  cont (ls,s,k) rs = k (ls++s++rs)

  show3 : TreeB -> (String -> String) -> String
  show3 (Leaf s) k = k s
  show3 (Node lft s rgt) k = 
     show3 lft (next (s,rgt,k))

data Kont = Done
          | Next String TreeB Kont
          | Conc String String Kont
          
mutual
  --()
  --(String,Tree,String->String)
  --(String,String,String->String)
  apply : Kont -> String -> String
  apply Done s = s
  apply (Next s rgt k) ls = show4 rgt (Conc ls s k)
  apply (Conc ls s k)  rs = apply k (ls++s++rs)
  
  public export
  show4 : TreeB -> Kont -> String
  show4 (Leaf s) k         = apply k s
  show4 (Node lft s rgt) k = show4 lft (Next s rgt k)

  public export
  show4' : TreeB -> String
  show4' t = show4 t Done
  
data Stack = List (String,Either TreeB String)
{-
mutual
  apply2 : Stack -> String -> String
  apply2 [] s = s
  apply2 ((x,(Left rgt)::xs) s = show5 rgt ( (Right ):xs)
-}
public export
show3' : TreeB -> String
show3' t = show3 t (\x=>x) 
                            
public export
show2' : TreeB -> String
show2' t = show2 t (\x => x)

export
hilton : Address
hilton = MkA "Street" "" "London" "SU 4X" UK (MkC "Hilton")

export
factory1 : Address
factory1 = MkA "Factory street" "" "Asia" "44AX" UK (MkC "Factory1")

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
     fx = MkFx date Purchase factory1 factory1 (MkH121 h1 h2 (apply2' h2 h1) ) Nothing
 --Confirm fx 
 Pure ()

export
confirm_so : OwnerEvent ()
confirm_so = do
 let date = "2021-11-01"
     h1 = [p1,p2,p3]
     up1 = (toEX20 31.73)
     up2 = (toEX20 15.03)
     up3 = (toEX20 25.00)
     
     h2 = [ (fst p1, ("GBP",up1)), 
            (fst p2, ("GBP",up2)),
            (fst p3, ("GBP",up3))]
            
     fx = MkFx date Sale hilton hilton (MkH121 h1 h2 (apply2' h2 h1) ) Nothing
 --Confirm fx
 Pure ()


  
public export
LedgerMap  : Type
LedgerMap = SortedMap (Location, Ledger, ProdKey) EQty

public export
LedgerH11  : Type
LedgerH11 = SortedMap (Location, Location) (List JournalEvent)

public export
LedgerH121  : Type
LedgerH121 = SortedMap (Location, Location) (List JournalEvent)

export
RouteMap : Type
RouteMap = SortedMap (Date,RouteRef,RouteState) Route
{-
public export
record Muf where
  constructor MkMuf 
  new : RouteMap
  progress : RouteMap
  completed : RouteMap
-}  
  {-
  order : SortedMap FxRef FxData --Hom121
  forecast : SortedMap (Date,Address) Hom121
  reservation : SortedMap (Date,Address) Hom11
  actual : SortedMap (Date,Address) Hom11
  invoice : SortedMap (Date,Address) Hom121
  -}

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

{-
export
tail : List a -> List a
tail [] = []
tail (x::xs) = xs
-}
export
custRoute : (c:Address) -> List Location
custRoute c = [Partner Sale c, Control Sale c, Self]
export
suppRoute : (s:Address) -> List Location
suppRoute s = [Self, Control Purchase s, Partner Purchase s]

export
locRoute : (c:Address)->(s:Address)->List Location
locRoute c s = (custRoute c)++ (tail $ suppRoute s)


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
     ref <- Init date initRoute h121
     Pure ref

export
toWhs : OwnerEvent a -> WhsEvent a
toWhs (Init date route h121) = do
       let h11 = (MkH11 (dx h121) (cx h121))
       ref <- NewRoute date route
       Put11 date Init Self OnHand  h11 
       Put121 date Init Self Forecast h121 
       Pure ref

toWhs (Log x) =  Pure ()
toWhs (Show x) = Pure ()
toWhs (Pure x) = Pure x
toWhs (Bind x f) = do res <- toWhs x
                      toWhs (f res) --?toWhs_rhs_4


     
export
interpret : WhsEvent a -> State (RouteMap,LedgerMap,LedgerH11,LedgerH121) a
interpret  (NewRoute date route) = do
             (routes,led,lh11,lh121)<-get
             
             let route_cnt = encode route
                 route_ref = sha256 route_cnt
                 routes' = insert (date, route_ref,Completed)  route routes
             put (routes', led,lh11,lh121)
             pure route_ref

interpret (Put11 date f t ledger h11 ) = do 
             (routes,led,lh11,lh121)<-get
             let key = (f,t) 
                 kf : (Location, Ledger)
                 kf = (f,ledger)
                 
                 kt : (Location, Ledger)
                 kt = (t,ledger)
                                  
                 led1' : LedgerMap
                 led1' = update_ledger kf ( dx h11) led                 
                 led1'' : LedgerMap
                 led1'' = update_ledger kf (invHom1 $ cx h11) led1'
                 
                 led2' : LedgerMap
                 led2' = update_ledger kt (invHom1 $ dx h11) led1''
                 led2'' : LedgerMap
                 led2'' = update_ledger kt (cx h11) led2'
                 
             case (lookup key lh11) of
                Nothing => do
                   let lh' = insert key [Fx11 (date, h11)] lh11
                   put (routes,led2'',lh', lh121)
                Just h11_list => do
                   let lh' = insert key ((Fx11 (date, h11))::h11_list) lh11
                   put (routes,led2'',lh', lh121)
             pure () 

interpret (Put121 date f t ledger h121 ) = do
        (routes,led,lh11,lh121)<-get
        let key = (f,t) 
        case (lookup key lh121) of
                Nothing => do
                   let lh' = insert key [Fx121 (date,h121)] lh121
                   put (routes,led, lh11, lh')
                Just h_list => do
                   let lh' = insert key ( (Fx121 (date, h121) )::h_list) lh121
                   put (routes,led, lh11, lh')
        
        
{-
interpret (Close fx) = pure ()
interpret (Open fx) = do
      let cnt = encode fx
          ref = sha256 cnt
      (so,po,led,lh,journal)<-get
      case (direction fx) of
         Purchase => do
             let o = order po
                 o' : SortedMap FxRef FxData
                 o' = insert ref fx o                 
                 po' : Muf
                 po' = record {order = o'} po
             put (so,po',led,lh,(Fx fx)::journal)
         Sale => do
             let o = order so
                 o' : SortedMap FxRef FxData             
                 o' = insert ref fx o
                 so' : Muf
                 so' = record {order = o'} so
             put (so',po,led,lh,(Fx fx)::journal)
      pure ref
-}
{-
interpret (Confirm fx ) = do 
             (so,po,led,lh,journal)<-get
             
             let f = forecast so
                 key : (Date,Address)
                 key = (date fx, delivery fx) 
                 
                 x : Hom121
                 x = (h3 fx)
                 
                 f' : SortedMap (Date,Address) Hom121
                 f' = (insert key x f)
                 
                 so' : Muf
                 so' = record {forecast = f'} so
             put (so',po,led,lh,journal)

interpret (Confirm (MkO Purchase fx)) = do 
             (so,po,led)<-get
             
             let f = forecast po
                 key : (Date,Address)
                 key = (date fx, delivery fx) 
                 
                 f' : SortedMap (Date,Address) Hom121
                 f' = insert key (h3 fx) f
                 
                 r : SortedMap (Date,Address) Hom11
                 r = reservation po
                 
                 x : Hom121
                 x = h3 fx
                 
                 r' : SortedMap (Date,Address) Hom11
                 r' = insert key (MkH11 (from x) (to x)) r
                 
                 
                 po' : Muf
                 po' = record {forecast = f', reservation = r'} po
             put (so,po',led')
-}
             
interpret (Log x) = pure () --?interpret_rhs_1
interpret (Show x) = pure () --?interpret_rhs_2
interpret (Pure x) = pure x
interpret (Bind x f) = do res <- interpret x
                          interpret (f res)




export
test_demo2 : IO ()
test_demo2 = do
  
  pure ()
  printLn $show1 tree
  printLn $show2' tree
  printLn $show3' tree
  printLn $show4' tree
