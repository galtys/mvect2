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

import Ledger.PG.Order

%language ElabReflection

export
hilton : Address
hilton = MkA "Street" "" "London" "SU 4X" UK (MkC "Hilton")

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
confirm_so : OrderEvent ()
confirm_so = do
 let date = "2021-11-01"
     line1 = MkFx date hilton (MkH p1 (toEX20 31.73) Nothing)
     line2 = MkFx date hilton (MkH p2 (toEX20 15.03) Nothing)
     line3 = MkFx date hilton (MkH p3 (toEX20 25.00) Nothing)  
 Confirm (MkO Sale line1)
 Confirm (MkO Sale line2)
 Confirm (MkO Sale line3) 
 Pure ()

export
test_demo2 : IO ()
test_demo2 = do
  
  pure ()
