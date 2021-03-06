module Category.Schema.PJB

import Category.Schema.Types
import Category.Schema.GenPG
import System.File.ReadWrite

export
IT : TableName
IT = MkTN "IT" "account_invoice" "AccountInvoice" False
export
ILT : TableName
ILT = MkTN "ILT" "account_invoice_line" "AccountInvoiceLine" False
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
M2M_InvoiceTax : TableName
M2M_InvoiceTax = MkTN "M2M_IT" "account_invoice_line_tax" "M2M_InvoiceTax" True

export
RPT : TableName
RPT = MkTN "RPT" "res_partner" "ResPartner" False
export
COT : TableName
COT = MkTN "COT" "res_country" "ResCountry" False
export
ACVT : TableName
ACVT = MkTN "ACVT" "account_voucher" "AccountVoucher" False
export
SPT : TableName
SPT = MkTN "SPT" "stock_picking" "StockPicking" False
export
SMT : TableName
SMT = MkTN "SMT" "stock_move" "StockMove" False

export
BoMT : TableName
BoMT = MkTN "BOM" "mrp_bom" "BoM" False

export
PT : TableName
PT = MkTN "PT" "product_template" "ProductTemplate" False

export
PP : TableName
PP = MkTN "PP" "product_product" "Product" False


export
Id_PT : Schema
Id_PT = Pk "Id_PT" "id" PT
export
NamePT: Schema
NamePT = Prim (MkF NotNull I_String "name" Text "(Just . cast)" "cast" PT)
export
ListPrice : Schema
ListPrice = Prim (MkF Nullable I_Price "list_price" DoublePrecision "(Just . toINC20)" "cast" PT)
export
ProductTemplate : Schema
ProductTemplate = Model PT [Id_PT,NamePT,ListPrice] "(True)"

--nullable Price "list_price" DoublePrecision (Just . toINC20) cast PT
export
Id_PP : Schema
Id_PP = Pk "Id_PP" "id" PP
export
ProductTmplIDF : Field
ProductTmplIDF = (MkF NotNull I_Bits32 "product_tmpl_id" (BigInt) "(Just . cast)" "cast" PP)
export
ProductTmplID:Schema
ProductTmplID = M2O PT ProductTmplIDF PP
export
TradePrice : Schema
TradePrice = Prim (MkF Nullable I_Price "trade" DoublePrecision "(Just . toEX20)" "cast" PP)
export
RetailPrice : Schema
RetailPrice = Prim (MkF Nullable I_Price "retail" DoublePrecision "(Just . toINC20)" "cast" PP)
export
ContractPrice : Schema
ContractPrice = Prim (MkF Nullable I_Price "contract" DoublePrecision "(Just . toEX20)" "cast" PP)
export
SKU: Schema
SKU = Prim (MkF NotNull I_String "default_code" Text "(Just . cast)" "cast" PP)
export
ProductProduct : Schema
ProductProduct = Model PP [Id_PP,ProductTmplID,TradePrice,RetailPrice,ContractPrice,SKU] "(True)"
export
Id_BoM : Schema
Id_BoM = Pk "Id_BoM" "id" BoMT
export
ProductQtyBoM : Schema
ProductQtyBoM = Prim (MkF NotNull I_EQty "product_qty" DoublePrecision "(Just . cast)" "cast" BoMT)

ProductID_F : Field
ProductID_F = (MkF NotNull I_Bits32 "product_id" (BigInt) "(Just . cast)" "cast" BoMT)

BoMID_F : Field
BoMID_F = (MkF Nullable I_Bits32 "bom_id" (BigInt) "(Just . cast)" "cast" BoMT)

export
ProductIDBoM : Schema
ProductIDBoM = Prim ProductID_F --M2O PP ProductID_F BoMT
export
BoMID : Schema
BoMID = Prim BoMID_F --M2O BoMT BoMID_F BoMT
export
BoMLines : Schema
BoMLines = O2M "bom_lines" BoMID_F BoMT
export
BoM : Schema
BoM = Model BoMT [Id_BoM,ProductQtyBoM,BoMID,BoMLines, ProductIDBoM] "(IsNull BomIdBOM)"


export
Id_ILT : Schema
Id_ILT = Pk "Id_ILT" "id" ILT
export
InvoiceIDF : Field
InvoiceIDF = (MkF NotNull I_Bits32 "invoice_id" (BigInt) "(Just . cast)" "cast" ILT)
export
InvoiceID : Schema
InvoiceID = M2O IT InvoiceIDF ILT
export
PriceUnitILT : Schema
PriceUnitILT = Prim (MkF NotNull I_EQty "price_unit" DoublePrecision "(Just . cast)" "cast" ILT)
export
QuantityILT : Schema
QuantityILT = Prim (MkF NotNull I_EQty "quantity" DoublePrecision "(Just . cast)" "cast" ILT)
export
NameILT: Schema
NameILT = Prim (MkF NotNull I_String "name" Text "(Just . cast)" "cast" ILT)
export
ProductIDIlt : Schema
ProductIDIlt = Prim (MkF Nullable I_Bits32 "product_id" BigInt "(Just . cast)" "cast" ILT)
-- "sale_order_tax"
F1_ilt:Field
F1_ilt = (MkF NotNull I_Bits32 "invoice_line_id" BigInt "(Just . cast)" "cast" M2M_InvoiceTax)
F2_ilt:Field
F2_ilt = (MkF NotNull I_Bits32 "tax_id" BigInt "(Just . cast)" "cast" M2M_InvoiceTax)
export
TaxesIlt : Schema
TaxesIlt = M2M "tax_ids" F1_ilt F2_ilt M2M_InvoiceTax OdooTaxTable
export
DiscountILT : Schema
DiscountILT = Prim (MkF Nullable I_EQty "discount" DoublePrecision "(Just . percent)" "cast" ILT)
export
InvoiceLine : Schema
InvoiceLine = Model ILT [Id_ILT,InvoiceID,PriceUnitILT,QuantityILT,NameILT,ProductIDIlt,TaxesIlt,DiscountILT] "(True)"
export
InvoiceTaxM2M : Schema
InvoiceTaxM2M = Model M2M_InvoiceTax [ Prim F1_ilt, Prim F2_ilt] "(True)"

export
Id_IT : Schema
Id_IT = Pk "Id_IT" "id" IT
export
OriginIT: Schema
OriginIT = Prim (MkF Nullable I_String "origin" (VarChar 64) "(Just . cast)" "cast" IT)
export
DateDue : Schema
DateDue = Prim (MkF NotNull I_Date "date_due" (VarChar 10) "(Just . cast)" "cast" IT)
export
NumberIT : Schema
NumberIT = Prim (MkF NotNull I_String "number" (VarChar 32) "(Just . cast)" "cast" IT)
export
AccountIDIT : Schema
AccountIDIT = Prim (MkF Nullable I_Bits32 "account_id" BigInt "(Just . cast)" "cast" IT)
export
PartnerIDIT : Schema
PartnerIDIT = Prim (MkF Nullable I_Bits32 "partner_id" (BigInt) "(Just . cast)" "cast" IT)
export
JournalIDIT : Schema
JournalIDIT = Prim (MkF Nullable I_Bits32 "journal_id" (BigInt) "(Just . cast)" "cast" IT)
export
AmountTaxIT : Schema
AmountTaxIT = Prim (MkF NotNull I_Price "amount_tax" DoublePrecision "(Just . toTaxA)" "cast" IT)
export
StateIT : Schema
StateIT = Prim (MkF NotNull I_String "state" Text "(Just . cast)" "cast" IT)
export
TypeIT : Schema
TypeIT = Prim (MkF Nullable I_String "type" (VarChar 64) "(Just . cast)" "cast" IT)
export
DateInvoice : Schema
DateInvoice = Prim (MkF NotNull I_Date "date_invoice" (VarChar 10) "(Just . cast)" "cast" IT)
export
AmountUntaxedIT : Schema
AmountUntaxedIT = Prim (MkF NotNull I_Price "amount_untaxed" DoublePrecision "(Just . toEX20)" "cast" IT)
export
AmountTotalIT : Schema
AmountTotalIT = Prim (MkF NotNull I_Price "amount_total" DoublePrecision "(Just . toINC20)" "cast" IT)
export
InvoiceLines : Schema
InvoiceLines = O2M "invoice_line" InvoiceIDF ILT
export
Invoice : Schema
Invoice = Model IT [Id_IT,OriginIT,DateDue,NumberIT,AccountIDIT,PartnerIDIT,JournalIDIT,AmountTaxIT,StateIT,TypeIT,DateInvoice,AmountUntaxedIT,AmountTotalIT,InvoiceLines] "(True)"


---------------------
export
Id_Smt : Schema
Id_Smt = Pk "Id_Smt" "id" SMT
export
OriginSmt: Schema
OriginSmt = Prim (MkF Nullable I_String "origin" (VarChar 64) "(Just . cast)" "cast" SMT)
export
PriceUnitSmt : Schema
PriceUnitSmt = Prim (MkF Nullable I_EQty "price_unit" DoublePrecision "(Just . cast)" "cast" SMT)
export
ProductQty : Schema
ProductQty = Prim (MkF NotNull I_EQty "product_qty" DoublePrecision "(Just . cast)" "cast" SMT)
export
ProductIDSmt : Schema
ProductIDSmt = Prim (MkF NotNull I_Bits32 "product_id" BigInt "(Just . cast)" "cast" SMT)
export
LocationIDSmt : Schema
LocationIDSmt = Prim (MkF NotNull I_Bits32 "location_id" BigInt "(Just . cast)" "cast" SMT)
export
LocationDestIDSmt : Schema
LocationDestIDSmt = Prim (MkF NotNull I_Bits32 "location_dest_id" BigInt "(Just . cast)" "cast" SMT)
export
PickingIDSmtF : Field
PickingIDSmtF = (MkF Nullable I_Bits32 "picking_id" BigInt "(Just . cast)" "cast" SMT)
export
PickingIDSmt : Schema
PickingIDSmt = Prim PickingIDSmtF
export
PurchaseLineIDSmt : Schema
PurchaseLineIDSmt = Prim (MkF NotNull I_Bits32 "purchase_line_id" BigInt "(Just . cast)" "cast" SMT)
export
SaleLineIDSmt : Schema
SaleLineIDSmt = Prim (MkF NotNull I_Bits32 "sale_line_id" BigInt "(Just . cast)" "cast" SMT)

export
StateSmt : Schema
StateSmt = Prim (MkF NotNull I_String "state" Text "(Just . cast)" "cast" SMT)
export
StockMove : Schema
StockMove = Model SMT [Id_Smt,OriginSmt,PriceUnitSmt,ProductQty,ProductIDSmt,LocationIDSmt,LocationDestIDSmt,PickingIDSmt,
                       PurchaseLineIDSmt, SaleLineIDSmt, StateSmt] "(True)"

export
Id_Spt : Schema
Id_Spt = Pk "Id_Spt" "id" SPT
export
OriginSpt: Schema
OriginSpt = Prim (MkF Nullable I_String "origin" (VarChar 64) "(Just . cast)" "cast" SPT)
export
BackorderID : Schema
BackorderID = Prim (MkF Nullable I_Bits32 "backorder_id" (BigInt) "(Just . cast)" "cast" SPT)
export
DateDone : Schema
DateDone = Prim (MkF NotNull I_Date "date_done" (VarChar 10) "(Just . cast)" "cast" SPT)
export
PartnerIDSpt : Schema
PartnerIDSpt = Prim (MkF Nullable I_Bits32 "partner_id" (BigInt) "(Just . cast)" "cast" SPT)
export
MinDate : Schema
MinDate = Prim (MkF NotNull I_Date "min_date" (VarChar 10) "(Just . cast)" "cast" SPT)
export
NameSpt : Schema
NameSpt = Prim (MkF NotNull I_String "name" (VarChar 64) "(Just . cast)" "cast" SPT)
export
StateSpt : Schema
StateSpt = Prim (MkF NotNull I_String "state" Text "(Just . cast)" "cast" SPT)
export
PickingMoves : Schema
PickingMoves = O2M "move_ids" PickingIDSmtF SMT
export
StockPicking : Schema
StockPicking = Model SPT [Id_Spt,OriginSpt,BackorderID,DateDone,PartnerIDSpt,MinDate,NameSpt,StateSpt,PickingMoves] "(True)"



export
Id_Acvt : Schema
Id_Acvt = Pk "Id_Acvt" "id" ACVT
export
NumberAcvt : Schema
NumberAcvt = Prim (MkF NotNull I_String "number" (VarChar 32) "(Just . cast)" "cast" ACVT)
export
PartnerIDAC : Schema
PartnerIDAC = Prim (MkF Nullable I_Bits32 "partner_id" (BigInt) "(Just . cast)" "cast" ACVT)
export
JournalID : Schema
JournalID = Prim (MkF Nullable I_Bits32 "journal_id" (BigInt) "(Just . cast)" "cast" ACVT)
export
AmountACVT : Schema
AmountACVT = Prim (MkF NotNull I_EQty "amount" DoublePrecision "(Just . cast)" "cast" ACVT)
acv_cols : List Schema
acv_cols = [Id_Acvt, NumberAcvt, PartnerIDAC,JournalID,AmountACVT]
export
AccountVoucher : Schema
AccountVoucher = Model ACVT acv_cols "(True)"


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
ParentIDF : Field
ParentIDF = (MkF Nullable I_Bits32 "parent_id" (BigInt) "(Just . cast)" "cast" RPT)
export
ParentID : Schema
ParentID = Prim ParentIDF  -- M2O RPT "parent_id" RPT
{-
        'invoice_ids':fields.one2many('account.invoice','partner_id','History Invoices' ),
        
        'sales_orders':fields.one2many('sale.order','partner_id','sales orders'),

        'payments':fields.one2many('account.voucher','partner_id','Payments'),#should change the names 
        'deliverey_orders':fields.one2many('stock.picking.out','partner_id','Delivery Orders'),#the names should be appear different for supplier and customer        
        -}
export
ChildContacts : Schema
ChildContacts = O2M "child_ids" ParentIDF RPT
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
ResPartner = Model RPT rpt_cols "(True)"

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
AmountT = Prim (MkF NotNull I_EQty "amount" DoublePrecision "(Just . cast)" "cast" OdooTaxTable)
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
OdooTax = Model OdooTaxTable tax_cols "(True)"

-- Order Line

export
Id_OLT : Schema
Id_OLT = Pk "Id_OLT" "id" OLT

export
PrimOrderIDF : Field
PrimOrderIDF = (MkF NotNull I_Bits32 "order_id" (BigInt) "(Just . cast)" "cast" OLT)
export
PrimOrderID : Schema
PrimOrderID = Prim PrimOrderIDF --M2O OT PrimOrderIDF OLT

export
PriceUnit : Schema
PriceUnit = Prim (MkF NotNull I_EQty "price_unit" DoublePrecision "(Just . cast)" "cast" OLT)
export
ProductUomQty : Schema
ProductUomQty = Prim (MkF NotNull I_EQty "product_uom_qty" DoublePrecision "(Just . cast)" "cast" OLT)
export
Discount : Schema
Discount = Prim (MkF Nullable I_EQty "discount" DoublePrecision "(Just . percent)" "cast" OLT)
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
OrderLineCols = Model OLT order_line_cols "(True)"
export
OdooTaxM2M : Schema
OdooTaxM2M = Model M2M_SaleTax [ Prim F1, Prim F2] "(True)"

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
OrderLines = O2M "order_line" PrimOrderIDF OLT
{-
        'invoice_ids': fields.many2many('account.invoice', 'sale_order_invoice_rel', 'order_id', 'invoice_id', 'Invoices', readonly=True, help="This is the list of invoices that have been generated for this sales order. The same sales order may have been invoiced in several times (by line for example)."),
        
        'picking_ids': fields.one2many('stock.picking.out', 'sale_id', 'Related Picking', readonly=True, help="This is a list of delivery orders that has been generated for this sales order."),        
 -}

so_cols : List Schema
so_cols = [Id_OT,Origin,OrderPolicy,DateOrder,PartnerID,AmountTax,StateOT,PartnerInvoiceID,AmountUntaxed,AmountTotal, NameOT,PartnerShippingID,PickingPolicy,CarrierID,OrderLines,RequestedDate]

export
SaleOrder : Schema
SaleOrder = Model OT so_cols "(True)"  -- #"((StateOT /= "cancel")&&(StateOT /= "draft"))"#  --

-- Schema

export
PJB : Schema
PJB = Sch "Odoo.Schema.PJB" [ProductTemplate,ProductProduct,BoM, ResPartner,OdooTaxM2M, OdooTax, OrderLineCols, SaleOrder, AccountVoucher,StockMove,StockPicking,
                             InvoiceTaxM2M,InvoiceLine,Invoice] --,,OrderLineCols]

ret_spaces : Bits32 -> String
ret_spaces x = if x==0 then "" else concat [ "  " | u<- [0..x]]

export
strFromSDoc : SDoc -> String
strFromSDoc (Line i t) =
     let sp = (ret_spaces i) in (sp++t++"\n")

strFromSDoc Sep = "\n"
strFromSDoc (Def []) = ""
strFromSDoc (Def (x :: xs)) =
  let r_x = strFromSDoc x
      r_dx = strFromSDoc (Def xs) in (r_x++r_dx)
export
saveSchema_source : HasIO io => String -> SDoc -> io (Either String ())
saveSchema_source fn schema = do
  let ret =strFromSDoc  schema
  Right ret <- writeFile fn ret
     | Left err => pure $ Left $ show err
  pure $ Right ()

export
generate_pjb_schema : HasIO io => io ()
generate_pjb_schema = do
  ret <- saveSchema_source "src/Odoo/Schema/PJBRecDef.idr" (showSchemaRecDef PJB)  
  printLn ret
  
  ret <- saveSchema_source "src/Odoo/Schema/PJB.idr" (showSchemaDef PJB)  
  printLn ret
  
  let msa = genSchemaTree "" PJB
  traverse_ printLn msa 
  
  --printSDoc $ schema_show PJB
  --printLn (length order_line_cols)
  --printSDoc $ schema_show DeliveryLine --tn_show OdooTaxTable
  --printSDoc $ schema_show Id_OLT --tn_show OdooTaxTable  
