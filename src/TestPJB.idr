module TestPJB

{-
import Web.Mongoose.Types
import Web.Mongoose.FFI
-}
import Crypto.Hash.SHA256

import Data.SortedMap

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Hom
import Category.Transaction.Journal
import Category.Transaction.Types
import Category.Transaction.Demo
import Category.Transaction.Demo2
import Data.Ratio
--import Data.Zippable

import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Language.Reflection.Types

import Data.HashDB.Types
import Data.HashDB.DataIO

import PQ.Schema
--import System.FFI
import JSON

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

import Odoo.PG.BoM
import Odoo.PG.Order

import Category.Schema.Types
import Category.Schema.PJB

import Odoo.Schema.PJBRecDef
import Odoo.Schema.PJB

import Core.Context
--import System.FFI
--import Libc.Time
import System

%ambiguity_depth 10


so_id_44575 : Bits32
so_id_44575 = 44575

pjb_test : IO ()
pjb_test = do
  t0 <- time
  printLn t0  
  p <- BrowseResPartner.read_ids [1..31510] (True)
  t1 <- time
  printLn t1  

  traverse_ printLn p
  {-  
  let toPair : String -> (String,Int)
      toPair x = (x,1)
      pmap : SortedMap String Int
      pmap = fromList (map (toPair . name) p)
  t1 <- time
  traverse_ printLn (Data.SortedMap.toList pmap)
  
  printLn (t1-t0)
  t2 <- time
  printLn (lookup "Zaki3" pmap)
  t3 <- time
  printLn (t3-t2)
  
  
  so <- BrowseOrder.read_ids [21833] (True)
  printLn so
  
  inv <- BrowseAccountInvoice.read_ids [1..31510] (True)
  sp <- BrowseStockPicking.read_ids [1..31510] (True)
  av <- BrowseAccountVoucher.read_ids [1..31510] (True)  
  traverse_ printLn so
  traverse_ printLn inv
  traverse_ printLn sp  
  traverse_ printLn av
  -}
  

main : IO ()
main = do
  --test_libc_time
  pjb_test
  --test_demo2
  --test_main_x
  --db_main  
  pure ()
