module PJBGenMain

import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256

import Data.SortedMap

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Hom
import Category.Transaction.Journal
import Category.Transaction.Demo
import Category.Transaction.Types
import Data.Ratio
--import Data.Zippable

import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Language.Reflection.Types

import Data.HashDB.Types
import Data.HashDB.DataIO
{-
import PQ.Schema

import JSON

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

import Ledger.PG.BoM
import Ledger.PG.Order
-}
import Ledger.Schema.Types
import Ledger.Schema.Order
import Ledger.Schema.GenPG

-- this is generated: import Odoo.Schema.PJB

main : IO ()
main = do
  --test_libc_time
  
  generate_pjb_schema
  --db_main  
  pure ()
