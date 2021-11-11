module Ledger.Schema.Order

import Ledger.Schema.Types
import Ledger.Schema.GenPG

-- Order
export
OT : TableName
OT = MkTN "OT" "sale_order" "Order" False
export
OLT : TableName
OLT = MkTN "OLT" "sale_order_line" "OrderLine" False
export
OdooTaxTable : TableName
OdooTaxTable = MkTN "OTax" "account_tax" "OrderTax" False

export
M2M_SaleTax : TableName
M2M_SaleTax = MkTN "M2M_ST" "sale_order_tax" "M2M_OrderTax" True

export
RPT : TableName
RPT = MkTN "RPT" "res_partner" "ResPartner" False

export
COT : TableName
COT = MkTN "COT" "res_country" "ResCountry" False

export
Id_Rpt : Schema
Id_Rpt = Pk "Id_Rpt" "id" RPT
export
NameRpt : Schema
NameRpt = Prim (MkF NotNull I_String "name" (VarChar 128) "(Just . cast)" "cast" RPT)
export
UseParentAddress : Schema
UseParentAddress = Prim (MkF Nullable I_Bool "use_parent_address" Boolean "(Just . cast)" "cast" RPT)
export
ActiveRpt : Schema
ActiveRpt = Prim (MkF Nullable I_Bool "active" Boolean "(Just . cast)" "cast" RPT)
export
Street : Schema
Street = Prim (MkF Nullable I_String "street" (VarChar 128) "(Just . cast)" "cast" RPT)
export
ContractRpt : Schema
ContractRpt = Prim (MkF Nullable I_Bool "contract" Boolean "(Just . cast)" "cast" RPT)
export
City : Schema
City = Prim (MkF Nullable I_String "city" (VarChar 128) "(Just . cast)" "cast" RPT)
export
Zip : Schema
Zip = Prim (MkF Nullable I_String "zip" (VarChar 128) "(Just . cast)" "cast" RPT)
export
CountryID : Schema
CountryID = Prim (MkF Nullable I_Bits32 "country_id" (BigInt) "(Just . cast)" "cast" RPT)  -- M2O COT "country_id" RPT
export
ParentID : Schema
ParentID = Prim (MkF Nullable I_Bits32 "parent_id" (BigInt) "(Just . cast)" "cast" RPT)  -- M2O RPT "parent_id" RPT
export
ChildContacts : Schema
ChildContacts = O2M "child_ids" "parent_id" RPT
export
Email : Schema
Email = Prim (MkF NotNull I_String "email" (VarChar 128) "(Just . cast)" "cast" RPT)
export
Street2 : Schema
Street2 = Prim (MkF Nullable I_String "street2" (VarChar 128) "(Just . cast)" "cast" RPT)

rpt_cols : List Schema
rpt_cols = [Id_Rpt,NameRpt,UseParentAddress,ActiveRpt,Street,ContractRpt,City,Zip,CountryID,ParentID,ChildContacts,Email,Street2]
export
ResPartner : Schema
ResPartner = Model RPT rpt_cols

----- Odoo/OpenERP Tax Code 
export
Id_Tax : Schema
Id_Tax = Pk "Id_Tax" "id" OdooTaxTable
export
NameTax : Schema
NameTax = Prim (MkF NotNull I_String "name" (VarChar 64) "(Just . cast)" "cast" OdooTaxTable)
export
DescriptionTax : Schema
DescriptionTax = Prim (MkF Nullable I_String "description" (VarChar 64) "(Just . cast)" "cast" OdooTaxTable)
export
AmountT : Schema
AmountT = Prim (MkF NotNull I_TQty "amount" DoublePrecision "(Just . cast)" "cast" OdooTaxTable)
export
TypeTax : Schema
TypeTax = Prim (MkF Nullable I_String "type" (VarChar 64) "(Just . cast)" "cast" OdooTaxTable)
export
PriceInclude : Schema
PriceInclude = Prim (MkF Nullable I_Bool "price_include" Boolean "(Just . cast)" "cast" OdooTaxTable)

export
tax_cols : List Schema
tax_cols = [Id_Tax,NameTax,DescriptionTax,AmountT,TypeTax,PriceInclude]

export
OdooTax : Schema
OdooTax = Model OdooTaxTable tax_cols

-- Order Line

export
Id_OLT : Schema
Id_OLT = Pk "Id_OLT" "id" OLT


export
PrimOrderID : Schema
PrimOrderID = M2O OT "order_id" OLT --(MkF NotNull I_Bits32 "order_id" BigInt "(Just . cast)" "cast" OLT)
export
PriceUnit : Schema
PriceUnit = Prim (MkF NotNull I_TQty "price_unit" DoublePrecision "(Just . cast)" "cast" OLT)
export
ProductUomQty : Schema
ProductUomQty = Prim (MkF NotNull I_TQty "product_uom_qty" DoublePrecision "(Just . cast)" "cast" OLT)
export
Discount : Schema
Discount = Prim (MkF Nullable I_TQty "discount" DoublePrecision "(Just . cast)" "cast" OLT)
export
ProductID : Schema
ProductID = Prim (MkF Nullable I_Bits32 "product_id" BigInt "(Just . cast)" "cast" OLT)
export
DeliveryLine : Schema
DeliveryLine = Prim (MkF Nullable I_Bool "delivery_line" Boolean "(Just . cast)" "cast" OLT)

-- "sale_order_tax"
F1:Field
F1 = (MkF NotNull I_Bits32 "order_line_id" BigInt "(Just . cast)" "cast" M2M_SaleTax)
F2:Field
F2 = (MkF NotNull I_Bits32 "tax_id" BigInt "(Just . cast)" "cast" M2M_SaleTax)
export
Taxes : Schema
Taxes = M2M "tax_ids" F1 F2 M2M_SaleTax OdooTaxTable

order_line_cols : List Schema
order_line_cols = [Id_OLT,PriceUnit,ProductUomQty,Discount,DeliveryLine,PrimOrderID,ProductID, Taxes]

export
OrderLineCols : Schema
OrderLineCols = Model OLT order_line_cols
export
OdooTaxM2M : Schema
OdooTaxM2M = Model M2M_SaleTax [ Prim F1, Prim F2]

-- Order 
export
Id_OT : Schema
Id_OT = Pk "Id_OT" "id" OT
export
Origin: Schema
Origin = Prim (MkF Nullable I_String "origin" (VarChar 64) "(Just . cast)" "cast" OT)
export
OrderPolicy : Schema
OrderPolicy = Prim (MkF NotNull I_String "order_policy" (VarChar 64) "(Just . cast)" "cast" OT)
export
ShopID : Schema
ShopID = Prim (MkF NotNull I_Bits32 "shop_id" BigInt "(Just . cast)" "cast" OT)
export
ClientOrderRef: Schema
ClientOrderRef = Prim (MkF Nullable I_String "client_order_ref" (VarChar 64) "(Just . cast)" "cast" OT)
export
DateOrder : Schema
DateOrder = Prim (MkF NotNull I_Date "date_order" (VarChar 10) "(Just . cast)" "cast" OT)
export
PartnerID : Schema
PartnerID = Prim (MkF NotNull I_Bits32 "partner_id" BigInt "(Just . cast)" "cast" OT)
export
Note: Schema
Note = Prim (MkF Nullable I_String "note" Text "(Just . cast)" "cast" OT)
export
FiscalPosition : Schema
FiscalPosition = Prim (MkF Nullable I_Bits32 "fiscal_position" BigInt "(Just . cast)" "cast" OT)
export
UserID : Schema
UserID = Prim (MkF Nullable I_Bits32 "user_id" BigInt "(Just . cast)" "cast" OT)
export
AmountTax : Schema
AmountTax = Prim (MkF NotNull I_Price "amount_tax" DoublePrecision "(Just . toTaxA)" "cast" OT)
export
StateOT : Schema
StateOT = Prim (MkF NotNull I_String "state" Text "(Just . cast)" "cast" OT)
export
PricelistID : Schema
PricelistID = Prim (MkF NotNull I_Bits32 "pricelist_id" BigInt "(Just . cast)" "cast" OT)
export
PartnerInvoiceID : Schema
PartnerInvoiceID = Prim (MkF NotNull I_Bits32 "partner_invoice_id" BigInt "(Just . cast)" "cast" OT)
export
AmountUntaxed : Schema
AmountUntaxed = Prim (MkF NotNull I_Price "amount_untaxed" DoublePrecision "(Just . toEX20)" "cast" OT)
export
DateConfirm : Schema
DateConfirm = Prim (MkF Nullable I_Date "date_confirm" (VarChar 10) "(Just . cast)" "cast" OT)
export
AmountTotal : Schema
AmountTotal = Prim (MkF NotNull I_Price "amount_total" DoublePrecision "(Just . toINC20)" "cast" OT)
export
NameOT: Schema
NameOT = Prim (MkF NotNull I_String "name" Text "(Just . cast)" "cast" OT)
--NameOT: Column
--NameOT = notNull String "name" (VarChar 64) (Just . cast) cast OT
export
PartnerShippingID : Schema
PartnerShippingID = Prim (MkF NotNull I_Bits32 "partner_shipping_id" BigInt "(Just . cast)" "cast" OT)
export
PickingPolicy : Schema
PickingPolicy = Prim (MkF NotNull I_String "picking_policy" Text "(Just . cast)" "cast" OT)
export
CarrierID : Schema
CarrierID = Prim (MkF Nullable I_Bits32 "carrier_id" BigInt "(Just . cast)" "cast" OT)
export
EffectiveDate : Schema
EffectiveDate = Prim (MkF Nullable I_Date "effective_date" (VarChar 10) "(Just . cast)" "cast" OT)
export
RequestedDate : Schema
RequestedDate = Prim (MkF Nullable I_Date "requested_date" (VarChar 10) "(Just . cast)" "cast" OT)
export
CommitmentdDate : Schema
CommitmentdDate = Prim (MkF Nullable I_Date "commitmentd_date" (VarChar 10) "(Just . cast)" "cast" OT)
export
DeliveryNotes: Schema
DeliveryNotes = Prim (MkF Nullable I_String "delivery_notes" Text "(Just . cast)" "cast" OT)
export
OrderLines : Schema
OrderLines = O2M "order_line" "order_id" OLT

so_cols : List Schema
so_cols = [Id_OT,Origin,OrderPolicy,DateOrder,PartnerID,AmountTax,StateOT,PartnerInvoiceID,AmountUntaxed,AmountTotal, NameOT,PartnerShippingID,PickingPolicy,CarrierID,OrderLines,RequestedDate]

export
SaleOrder : Schema
SaleOrder = Model OT so_cols

-- Schema

export
PJB : Schema
PJB = Sch "Odoo.Schema.PJB" [ResPartner,OdooTaxM2M, OdooTax, OrderLineCols, SaleOrder] --,,OrderLineCols]

ret_spaces : Bits32 -> String
ret_spaces x = if x==0 then "" else concat [ "  " | u<- [0..x]]

printSDoc : HasIO io => SDoc -> io ()
printSDoc (Line i t) = do
     let sp = (ret_spaces i)
     putStrLn (sp++t)
printSDoc Sep = putStrLn ""     
printSDoc (Def []) = pure ()
printSDoc (Def (x :: xs)) = do
     printSDoc x
     printSDoc (Def xs)
     
--schema2SDoc : HasIO io => Schema
export
indentSDoc : Bits32 -> SDoc -> SDoc
indentSDoc x (Line i t) = (Line (i+x) t)
indentSDoc x (Def lines) = Def [ indentSDoc x l | l <- lines]
indentSDoc x Sep = Sep


export
test_main_x : HasIO io => io ()
test_main_x = do
  
  --let tbs = Def (map tn_show (schema_tables PJB))
--  let xu = schema_show OrderLineCols
  let xu = schema_show PJB--SaleOrder
  printSDoc xu
  --printLn (length order_line_cols)
  --printSDoc $ schema_show DeliveryLine --tn_show OdooTaxTable
  --printSDoc $ schema_show Id_OLT --tn_show OdooTaxTable  
