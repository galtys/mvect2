module Ledger.PG.Order


import Data.SortedMap

import Category.Transaction.Qty
--import Category.Transaction.Types
--import Category.Transaction.Hom
--import Category.Transaction.Journal
--import Category.Transaction.Demo
import Category.Transaction.Types
import Data.Ratio
--import Data.Zippable
import JSON

import Generics.Derive

import Control.Monad.Either

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

%language ElabReflection


public export
record Line where
  constructor MkLine
  sku : ProdKey
  qty : TQty
  --Unit of Measure
  --company pricelist is input  , "price_unit" modifies it, as a multiple
  currency : ProdKey   
  price_unit : TQty --together with discount,turn it into a function Qty->Qty
  discount : TQty   --idea, in amendments, fix price_unit and let the user change the discount   
  tax_code : TaxCode
  --reference to List Price  
                      --SubTotal ... calculated

%runElab derive "Line" [Generic, Meta, Show, Eq,RecordToJSON,RecordFromJSON]


public export
record LineExt where
  constructor MkLineExt
  sku : ProdKey
  qty : TQty
  currency : ProdKey   
  price_unit : TQty --together with discount,turn it into a function Qty->Qty
  discount : TQty   --idea, in amendments, fix price_unit and let the user change the discount   
  tax_code : List TaxCode

%runElab derive "LineExt" [Generic, Meta, Show, Eq]
--%runElab derive "LineExt" [Generic, Meta, Show, Eq,RecordToJSON,RecordFromJSON]




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

