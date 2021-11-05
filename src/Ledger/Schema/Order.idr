module Ledger.Schema.Order

import Ledger.Schema.Types

-- Order
OT : TableName
OT = "sale_order"

export
Id_OT : Schema
Id_OT = Pk "Id_OT" "id" OT

Origin: Schema
Origin = Prim (MkF Nullable I_String "origin" (VarChar 64) "(Just . cast)" "cast" OT)

OrderPolicy : Schema
OrderPolicy = Prim (MkF NotNull I_String "order_policy" (VarChar 64) "(Just . cast)" "cast" OT)

ShopID : Schema
ShopID = Prim (MkF NotNull I_Bits32 "shop_id" BigInt "(Just . cast)" "cast" OT)

ClientOrderRef: Schema
ClientOrderRef = Prim (MkF Nullable I_String "client_order_ref" (VarChar 64) "(Just . cast)" "cast" OT)


DateOrder : Schema
DateOrder = Prim (MkF NotNull I_Date "date_order" (VarChar 10) "(Just . cast)" "cast" OT)

PartnerID : Schema
PartnerID = Prim (MkF NotNull I_Bits32 "partner_id" BigInt "(Just . cast)" "cast" OT)

Note: Schema
Note = Prim (MkF Nullable I_String "note" Text "(Just . cast)" "cast" OT)


FiscalPosition : Schema
FiscalPosition = Prim (MkF Nullable I_Bits32 "fiscal_position" BigInt "(Just . cast)" "cast" OT)

UserID : Schema
UserID = Prim (MkF Nullable I_Bits32 "user_id" BigInt "(Just . cast)" "cast" OT)

AmountTax : Schema
AmountTax = Prim (MkF NotNull I_Price "amount_tax" DoublePrecision "(Just . toTaxA)" "cast" OT)


StateOT : Schema
StateOT = Prim (MkF NotNull I_String "state" Text "(Just . cast)" "cast" OT)

PricelistID : Schema
PricelistID = Prim (MkF NotNull I_Bits32 "pricelist_id" BigInt "(Just . cast)" "cast" OT)

PartnerInvoiceID : Schema
PartnerInvoiceID = Prim (MkF NotNull I_Bits32 "partner_invoice_id" BigInt "(Just . cast)" "cast" OT)

AmountUntaxed : Schema
AmountUntaxed = Prim (MkF NotNull I_Price "amount_untaxed" DoublePrecision "(Just . toEX20)" "cast" OT)

DateConfirm : Schema
DateConfirm = Prim (MkF Nullable I_Date "date_confirm" (VarChar 10) "(Just . cast)" "cast" OT)

AmountTotal : Schema
AmountTotal = Prim (MkF NotNull I_Price "amount_total" DoublePrecision "(Just . toINC20)" "cast" OT)

