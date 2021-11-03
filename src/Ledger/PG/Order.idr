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

-- Order and Order Line (odoo)

OT : String
OT = "sale_order"

OLT : String
OLT = "sale_order_line"

-- Order
Id_OT : Column
Id_OT = primarySerial64 Bits32 "id" (Just . cast) OT


Origin: Column
Origin = nullable String "origin" (VarChar 64) (Just . cast) cast OT

ShopID : Column
ShopID = notNull Bits32 "shop_id" BigInt (Just . cast) cast OT

ClientOrderRef: Column
ClientOrderRef = nullable String "client_order_ref" (VarChar 64) (Just . cast) cast OT

DateOrder : Column
DateOrder = notNull Date "date_order" (VarChar 10) (Just . cast) cast OT

PartnerID : Column
PartnerID = notNull Bits32 "partner_id" BigInt (Just . cast) cast OT

Note: Column
Note = nullable String "note" Text (Just . cast) cast OT

AmountTax : Column
AmountTax = nullable TQty "amount_tax" DoublePrecision (Just . cast) cast OT

StateOT : Column
StateOT = nullable String "state" Text (Just . cast) cast OT

PricelistID : Column
PricelistID = notNull Bits32 "pricelist_id" BigInt (Just . cast) cast OT

PartnerInvoiceID : Column
PartnerInvoiceID = notNull Bits32 "partner_invoice_id" BigInt (Just . cast) cast OT

AmountUntaxed : Column
AmountUntaxed = nullable TQty "amount_untaxed" DoublePrecision (Just . cast) cast OT

AmountTotal : Column
AmountTotal = nullable TQty "amount_total" DoublePrecision (Just . cast) cast OT

NameOT: Column
NameOT = nullable String "name" (VarChar 64) (Just . cast) cast OT

PartnerShippingID : Column
PartnerShippingID = notNull Bits32 "partner_shipping_id" BigInt (Just . cast) cast OT

CarrierID : Column
CarrierID = notNull Bits32 "carrier_id" BigInt (Just . cast) cast OT

RequestedDate : Column
RequestedDate = notNull Date "requested_date" (VarChar 10) (Just . cast) cast OT


