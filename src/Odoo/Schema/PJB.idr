module Odoo.Schema.PJB

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

import Category.Transaction.Types
import Data.Ratio

OT:String
OT = "sale_order"
OTax:String
OTax = "account_tax"
OLT:String
OLT = "sale_order_line"

PK_OT:Column
PK_OT=notNull Bits32 "id" (BigInt) (Just . cast) cast OT
ORIGIN_OT:Column
ORIGIN_OT=nullable String "origin" (VarChar 64) (Just . cast) cast OT
ORDER_POLICY_OT:Column
ORDER_POLICY_OT=notNull String "order_policy" (VarChar 64) (Just . cast) cast OT
DATE_ORDER_OT:Column
DATE_ORDER_OT=notNull Date "date_order" (VarChar 10) (Just . cast) cast OT
PARTNER_ID_OT:Column
PARTNER_ID_OT=notNull Bits32 "partner_id" (BigInt) (Just . cast) cast OT
AMOUNT_TAX_OT:Column
AMOUNT_TAX_OT=notNull Price "amount_tax" (DoublePrecision) (Just . toTaxA) cast OT
STATE_OT:Column
STATE_OT=notNull String "state" (Text) (Just . cast) cast OT
PARTNER_INVOICE_ID_OT:Column
PARTNER_INVOICE_ID_OT=notNull Bits32 "partner_invoice_id" (BigInt) (Just . cast) cast OT
AMOUNT_UNTAXED_OT:Column
AMOUNT_UNTAXED_OT=notNull Price "amount_untaxed" (DoublePrecision) (Just . toEX20) cast OT
AMOUNT_TOTAL_OT:Column
AMOUNT_TOTAL_OT=notNull Price "amount_total" (DoublePrecision) (Just . toINC20) cast OT
NAME_OT:Column
NAME_OT=notNull String "name" (Text) (Just . cast) cast OT
PARTNER_SHIPPING_ID_OT:Column
PARTNER_SHIPPING_ID_OT=notNull Bits32 "partner_shipping_id" (BigInt) (Just . cast) cast OT
PICKING_POLICY_OT:Column
PICKING_POLICY_OT=notNull String "picking_policy" (Text) (Just . cast) cast OT
CARRIER_ID_OT:Column
CARRIER_ID_OT=nullable Bits32 "carrier_id" (BigInt) (Just . cast) cast OT
REQUESTED_DATE_OT:Column
REQUESTED_DATE_OT=nullable Date "requested_date" (VarChar 10) (Just . cast) cast OT
--O2M

PK_OTax:Column
PK_OTax=notNull Bits32 "id" (BigInt) (Just . cast) cast OTax
NAME_OTax:Column
NAME_OTax=notNull String "name" (VarChar 64) (Just . cast) cast OTax
DESCRIPTION_OTax:Column
DESCRIPTION_OTax=nullable String "description" (VarChar 64) (Just . cast) cast OTax
AMOUNT_OTax:Column
AMOUNT_OTax=notNull TQty "amount" (DoublePrecision) (Just . cast) cast OTax
TYPE_OTax:Column
TYPE_OTax=nullable String "type" (VarChar 64) (Just . cast) cast OTax
PRICE_INCLUDE_OTax:Column
PRICE_INCLUDE_OTax=nullable Bool "price_include" (Boolean) (Just . cast) cast OTax

PK_OLT:Column
PK_OLT=notNull Bits32 "id" (BigInt) (Just . cast) cast OLT
PRICE_UNIT_OLT:Column
PRICE_UNIT_OLT=notNull TQty "price_unit" (DoublePrecision) (Just . cast) cast OLT
PRODUCT_UOM_QTY_OLT:Column
PRODUCT_UOM_QTY_OLT=notNull TQty "product_uom_qty" (DoublePrecision) (Just . cast) cast OLT
DISCOUNT_OLT:Column
DISCOUNT_OLT=nullable TQty "discount" (DoublePrecision) (Just . cast) cast OLT
DELIVERY_LINE_OLT:Column
DELIVERY_LINE_OLT=nullable Bool "delivery_line" (Boolean) (Just . cast) cast OLT
--M2O
PRODUCT_ID_OLT:Column
PRODUCT_ID_OLT=nullable Bits32 "product_id" (BigInt) (Just . cast) cast OLT
