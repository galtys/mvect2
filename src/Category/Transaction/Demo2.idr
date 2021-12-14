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
            
     fx = (MkFx date factory1 (MkH121 h1 h2 (apply2' h2 h1) ))
 Confirm (MkO Purchase fx)
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
            
     fx = (MkFx date hilton (MkH121 h1 h2 (apply2' h2 h1) ))
 Confirm (MkO Sale fx)
 
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
  forecast : SortedMap (Date,Address) Hom121
  reservation : SortedMap (Date,Address) Hom11
  actual : SortedMap (Date,Address) Hom11
  invoice : SortedMap (Date,Address) Hom121
  

export
interpret : OrderEvent a -> State (Muf,Muf) a
interpret (Confirm (MkO Sale y)) = do 
             (so,po)<-get
             
             let f = forecast so
                 key = (date y, l y) 
                 f' = insert key (h3 y) f                  
                 so' : Muf
                 so' = record {forecast = f'} so
             put (so',po)
             
interpret (Confirm (MkO Purchase y)) = do 
             (so,po)<-get
             
             let f = forecast po
                 key = (date y, l y) 
                 f' = insert key (h3 y) f
                 r = reservation po
                 
                 x = h3 y
                 r' = insert key (MkH11 (from x) (to x)) r
                 
                 po' : Muf
                 po' = record {forecast = f', reservation = r'} po
             put (so,po')
             
interpret (Log x) = pure () --?interpret_rhs_1
interpret (Show x) = pure () --?interpret_rhs_2
interpret (Pure x) = pure x
interpret (Bind x f) = do res <- interpret x
                          interpret (f res)




export
test_demo2 : IO ()
test_demo2 = do
  
  pure ()
