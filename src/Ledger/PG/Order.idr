module Ledger.PG.Order


import Data.SortedMap

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Hom
import Category.Transaction.Journal
import Category.Transaction.Demo
import Category.Transaction.Types
import Data.Ratio
--import Data.Zippable
import JSON

import Generics.Derive
import JSON

import Control.Monad.Either

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

%language ElabReflection

record RBoM where
  constructor MkRBoM
  product_id : Bits32
  product_qty : TQty
  bom_id : (Maybe Bits32)
  pk : Bits32
        
%runElab derive "RBoM" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

-- Order and Order Line (odoo)

OT : String
OT = "sale_order"

OLT : String
OLT = "sale_order_line"

-- Order
Id_OT : Column
Id_OT = primarySerial64 Bits32 "id" (Just . cast) OT

