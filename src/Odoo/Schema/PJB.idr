module Odoo.Schema.PJB

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

import Category.Transaction.Types
import Data.Ratio

OrderT:String
OrderT = "sale_order"
TaxT:String
TaxT = "account_tax"
OrderLineT:String
OrderLineT = "sale_order_line"

PK_OrderT:Column
PK_OrderT=notNull Bits32 "id" (BigInt) (Just . cast) cast OrderT
ORIGIN_OrderT:Column
ORIGIN_OrderT=nullable String "origin" (VarChar 64) (Just . cast) cast OrderT
ORDER_POLICY_OrderT:Column
ORDER_POLICY_OrderT=notNull String "order_policy" (VarChar 64) (Just . cast) cast OrderT
DATE_ORDER_OrderT:Column
DATE_ORDER_OrderT=notNull Date "date_order" (VarChar 10) (Just . cast) cast OrderT
PARTNER_ID_OrderT:Column
PARTNER_ID_OrderT=notNull Bits32 "partner_id" (BigInt) (Just . cast) cast OrderT
AMOUNT_TAX_OrderT:Column
AMOUNT_TAX_OrderT=notNull Price "amount_tax" (DoublePrecision) (Just . toTaxA) cast OrderT
STATE_OrderT:Column
STATE_OrderT=notNull String "state" (Text) (Just . cast) cast OrderT
PARTNER_INVOICE_ID_OrderT:Column
PARTNER_INVOICE_ID_OrderT=notNull Bits32 "partner_invoice_id" (BigInt) (Just . cast) cast OrderT
AMOUNT_UNTAXED_OrderT:Column
AMOUNT_UNTAXED_OrderT=notNull Price "amount_untaxed" (DoublePrecision) (Just . toEX20) cast OrderT
AMOUNT_TOTAL_OrderT:Column
AMOUNT_TOTAL_OrderT=notNull Price "amount_total" (DoublePrecision) (Just . toINC20) cast OrderT
NAME_OrderT:Column
NAME_OrderT=notNull String "name" (Text) (Just . cast) cast OrderT
PARTNER_SHIPPING_ID_OrderT:Column
PARTNER_SHIPPING_ID_OrderT=notNull Bits32 "partner_shipping_id" (BigInt) (Just . cast) cast OrderT
PICKING_POLICY_OrderT:Column
PICKING_POLICY_OrderT=notNull String "picking_policy" (Text) (Just . cast) cast OrderT
CARRIER_ID_OrderT:Column
CARRIER_ID_OrderT=nullable Bits32 "carrier_id" (BigInt) (Just . cast) cast OrderT
REQUESTED_DATE_OrderT:Column
REQUESTED_DATE_OrderT=nullable Date "requested_date" (VarChar 10) (Just . cast) cast OrderT
--O2M

PK_TaxT:Column
PK_TaxT=notNull Bits32 "id" (BigInt) (Just . cast) cast TaxT
NAME_TaxT:Column
NAME_TaxT=notNull String "name" (VarChar 64) (Just . cast) cast TaxT
DESCRIPTION_TaxT:Column
DESCRIPTION_TaxT=nullable String "description" (VarChar 64) (Just . cast) cast TaxT
AMOUNT_TaxT:Column
AMOUNT_TaxT=notNull TQty "amount" (DoublePrecision) (Just . cast) cast TaxT
TYPE_TaxT:Column
TYPE_TaxT=nullable String "type" (VarChar 64) (Just . cast) cast TaxT
PRICE_INCLUDE_TaxT:Column
PRICE_INCLUDE_TaxT=nullable Bool "price_include" (Boolean) (Just . cast) cast TaxT

PK_OrderLineT:Column
PK_OrderLineT=notNull Bits32 "id" (BigInt) (Just . cast) cast OrderLineT
PRICE_UNIT_OrderLineT:Column
PRICE_UNIT_OrderLineT=notNull TQty "price_unit" (DoublePrecision) (Just . cast) cast OrderLineT
PRODUCT_UOM_QTY_OrderLineT:Column
PRODUCT_UOM_QTY_OrderLineT=notNull TQty "product_uom_qty" (DoublePrecision) (Just . cast) cast OrderLineT
DISCOUNT_OrderLineT:Column
DISCOUNT_OrderLineT=nullable TQty "discount" (DoublePrecision) (Just . cast) cast OrderLineT
DELIVERY_LINE_OrderLineT:Column
DELIVERY_LINE_OrderLineT=nullable Bool "delivery_line" (Boolean) (Just . cast) cast OrderLineT
--M2O
PRODUCT_ID_OrderLineT:Column
PRODUCT_ID_OrderLineT=nullable Bits32 "product_id" (BigInt) (Just . cast) cast OrderLineT
