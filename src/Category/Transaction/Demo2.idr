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
test_demo2 : IO ()
test_demo2 = do
  pure ()
