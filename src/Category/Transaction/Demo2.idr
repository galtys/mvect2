module Category.Transaction.Demo2

import Data.Zippable
import Generics.Derive
import Data.SortedMap
import Control.Monad.State

import JSON

import Category.Transaction.Qty
import Category.Transaction.Types
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
p1:Product
p1 = ("p1",2)
export
p2:Product
p2 = ("p2",7)
export
p3:Product
p3 = ("p1",10)

export
confirm_po : OrderEvent ()
confirm_po = do
 let date = "2021-10-01"
     h1 = [p1,p2,p3]
     up1 = (toEX20 21.73)
     up2 = (toEX20 11.03)
     up3 = (toEX20 17.00)
     
     h2 = [ (fst p1, ("GBP",up1)), 
            (fst p2, ("GBP",up2)),
            (fst p3, ("GBP",up3))]
            
     fx = MkFx date Purchase factory1 factory1 (MkH121 h1 h2 (apply2' h2 h1) ) Nothing
 Confirm fx
 
 Pure ()

export
confirm_so : OrderEvent ()
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
 Confirm fx
 
     {-
     sub1 = (snd p1) * (cast up1)
     sub2 = (snd p2) * (cast up2)
     sub3 = (snd p3) * (cast up3)
               
     line1 = MkFx date hilton (MkH p1 up1 ("GBP",sub1) )     
     line2 = MkFx date hilton (MkH p2 up2 ("GBP",sub2)  )
     line3 = MkFx date hilton (MkH p3 up3 ("GBP",sub3)  )  
     -}
     
 --Confirm (MkO Sale line1)
 --Confirm (MkO Sale line2)
 --Confirm (MkO Sale line3) 
 Pure ()


public export
record Muf where
  constructor MkMuf 
  order : SortedMap FxRef FxData --Hom121
  forecast : SortedMap (Date,Address) Hom121
  reservation : SortedMap (Date,Address) Hom11
  actual : SortedMap (Date,Address) Hom11
  invoice : SortedMap (Date,Address) Hom121
  
  
public export
LedgerMap  : Type
LedgerMap = SortedMap (ControlTag, DirectionTag, Ledger, ProdKey) EQty


public export
update_ledger : (ControlTag, DirectionTag, Ledger) -> Hom1 -> LedgerMap -> LedgerMap 
update_ledger k [] m = m
update_ledger k@(ct,d,l) ( (pk,eq)::xs) m = ret where
          key : (ControlTag, DirectionTag, Ledger,ProdKey)
          key = (ct,d,l,pk)
          
          ret : LedgerMap
          ret = case (lookup key m ) of
                  (Just q) => (update_ledger k xs (insert key (eq+q) m) )
                  Nothing => (update_ledger k xs  (insert key eq m)     )

export
interpret : OrderEvent a -> State (Muf,Muf,LedgerMap) a
interpret (Open fx) = do
      let cnt = encode fx
          ref = sha256 cnt
      (so,po,led)<-get
      case (direction fx) of
         Purchase => do
             let o = order po
                 o' : SortedMap FxRef FxData
                 o' = insert ref fx o                 
                 po' : Muf
                 po' = record {order = o'} po
             put (so,po',led)
         Sale => do
             let o = order so
                 o' : SortedMap FxRef FxData             
                 o' = insert ref fx o
                 so' : Muf
                 so' = record {order = o'} so
             put (so',po,led)
      pure ref

interpret (Close fx) = pure ()
interpret (Confirm fx ) = do 
             (so,po,led)<-get
             
             let f = forecast so
                 key : (Date,Address)
                 key = (date fx, delivery fx) 
                 
                 x : Hom121
                 x = (h3 fx)
                 
                 f' : SortedMap (Date,Address) Hom121
                 f' = (insert key x f)
                 
                 so' : Muf
                 so' = record {forecast = f'} so
             put (so',po,led)
{-
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
                 
                 k1 : (ControlTag, DirectionTag, Ledger)
                 k1 = (Control (delivery fx), Purchase, OnHand)
                 {-
                 m1: LedgerMap --(insert k1 (from x) empty)
                 m1 = (insert k1 (from x) empty)
                 -}                 
                 led' : LedgerMap
                 led' = update_ledger k1 (from x) led
                 
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
