module Odoo.Schema.PJB

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

import Category.Transaction.Types
import Data.Ratio

import Generics.Derive

import JSON

import Odoo.PG.Config
import Control.Monad.Either

import Odoo.Schema.PJBRecDef

%language ElabReflection
PT:String
PT = "product_template"
PP:String
PP = "product_product"
BOM:String
BOM = "mrp_bom"
RPT:String
RPT = "res_partner"
OTax:String
OTax = "account_tax"
OLT:String
OLT = "sale_order_line"
M2M_ST:String
M2M_ST = "sale_order_tax"
OT:String
OT = "sale_order"
ACVT:String
ACVT = "account_voucher"
SMT:String
SMT = "stock_move"
SPT:String
SPT = "stock_picking"
ILT:String
ILT = "account_invoice_line"
M2M_IT:String
M2M_IT = "account_invoice_line_tax"
IT:String
IT = "account_invoice"

PkPT:Column
PkPT=notNull Bits32 "id" (BigInt) (Just . cast) cast PT
NamePT:Column
NamePT=notNull String "name" (Text) (Just . cast) cast PT
ListPricePT:Column
ListPricePT=nullable Product "list_price" (DoublePrecision) (Just . toINC20) cast PT

PkPP:Column
PkPP=notNull Bits32 "id" (BigInt) (Just . cast) cast PP
ProductTmplIdPP:Column
ProductTmplIdPP=notNull Bits32 "product_tmpl_id" (BigInt) (Just . cast) cast PP
TradePP:Column
TradePP=nullable Product "trade" (DoublePrecision) (Just . toEX20) cast PP
RetailPP:Column
RetailPP=nullable Product "retail" (DoublePrecision) (Just . toINC20) cast PP
ContractPP:Column
ContractPP=nullable Product "contract" (DoublePrecision) (Just . toEX20) cast PP
DefaultCodePP:Column
DefaultCodePP=notNull String "default_code" (Text) (Just . cast) cast PP

PkBOM:Column
PkBOM=notNull Bits32 "id" (BigInt) (Just . cast) cast BOM
ProductQtyBOM:Column
ProductQtyBOM=notNull EQty "product_qty" (DoublePrecision) (Just . cast) cast BOM
BomIdBOM:Column
BomIdBOM=nullable Bits32 "bom_id" (BigInt) (Just . cast) cast BOM
--O2M
ProductIdBOM:Column
ProductIdBOM=notNull Bits32 "product_id" (BigInt) (Just . cast) cast BOM

PkRPT:Column
PkRPT=notNull Bits32 "id" (BigInt) (Just . cast) cast RPT
NameRPT:Column
NameRPT=notNull String "name" (VarChar 128) (Just . cast) cast RPT
UseParentAddressRPT:Column
UseParentAddressRPT=nullable Bool "use_parent_address" (Boolean) (Just . cast) cast RPT
ActiveRPT:Column
ActiveRPT=nullable Bool "active" (Boolean) (Just . cast) cast RPT
StreetRPT:Column
StreetRPT=nullable String "street" (VarChar 128) (Just . cast) cast RPT
ContractRPT:Column
ContractRPT=nullable Bool "contract" (Boolean) (Just . cast) cast RPT
CityRPT:Column
CityRPT=nullable String "city" (VarChar 128) (Just . cast) cast RPT
ZipRPT:Column
ZipRPT=nullable String "zip" (VarChar 128) (Just . cast) cast RPT
CountryIdRPT:Column
CountryIdRPT=nullable Bits32 "country_id" (BigInt) (Just . cast) cast RPT
ParentIdRPT:Column
ParentIdRPT=nullable Bits32 "parent_id" (BigInt) (Just . cast) cast RPT
--O2M
EmailRPT:Column
EmailRPT=notNull String "email" (VarChar 128) (Just . cast) cast RPT
Street2RPT:Column
Street2RPT=nullable String "street2" (VarChar 128) (Just . cast) cast RPT

OrderLineIdM2M_ST:Column
OrderLineIdM2M_ST=notNull Bits32 "order_line_id" (BigInt) (Just . cast) cast M2M_ST
TaxIdM2M_ST:Column
TaxIdM2M_ST=notNull Bits32 "tax_id" (BigInt) (Just . cast) cast M2M_ST

PkOTax:Column
PkOTax=notNull Bits32 "id" (BigInt) (Just . cast) cast OTax
NameOTax:Column
NameOTax=notNull String "name" (VarChar 64) (Just . cast) cast OTax
DescriptionOTax:Column
DescriptionOTax=nullable String "description" (VarChar 64) (Just . cast) cast OTax
AmountOTax:Column
AmountOTax=notNull EQty "amount" (DoublePrecision) (Just . cast) cast OTax
TypeOTax:Column
TypeOTax=nullable String "type" (VarChar 64) (Just . cast) cast OTax
PriceIncludeOTax:Column
PriceIncludeOTax=nullable Bool "price_include" (Boolean) (Just . cast) cast OTax

PkOLT:Column
PkOLT=notNull Bits32 "id" (BigInt) (Just . cast) cast OLT
PriceUnitOLT:Column
PriceUnitOLT=notNull EQty "price_unit" (DoublePrecision) (Just . cast) cast OLT
ProductUomQtyOLT:Column
ProductUomQtyOLT=notNull EQty "product_uom_qty" (DoublePrecision) (Just . cast) cast OLT
DiscountOLT:Column
DiscountOLT=nullable EQty "discount" (DoublePrecision) (Just . percent) cast OLT
DeliveryLineOLT:Column
DeliveryLineOLT=nullable Bool "delivery_line" (Boolean) (Just . cast) cast OLT
OrderIdOLT:Column
OrderIdOLT=notNull Bits32 "order_id" (BigInt) (Just . cast) cast OLT
ProductIdOLT:Column
ProductIdOLT=nullable Bits32 "product_id" (BigInt) (Just . cast) cast OLT
--M2M

PkOT:Column
PkOT=notNull Bits32 "id" (BigInt) (Just . cast) cast OT
OriginOT:Column
OriginOT=nullable String "origin" (VarChar 64) (Just . cast) cast OT
OrderPolicyOT:Column
OrderPolicyOT=notNull String "order_policy" (VarChar 64) (Just . cast) cast OT
DateOrderOT:Column
DateOrderOT=notNull Date "date_order" (VarChar 10) (Just . cast) cast OT
PartnerIdOT:Column
PartnerIdOT=notNull Bits32 "partner_id" (BigInt) (Just . cast) cast OT
AmountTaxOT:Column
AmountTaxOT=notNull Product "amount_tax" (DoublePrecision) (Just . toTaxA) cast OT
StateOT:Column
StateOT=notNull String "state" (Text) (Just . cast) cast OT
PartnerInvoiceIdOT:Column
PartnerInvoiceIdOT=notNull Bits32 "partner_invoice_id" (BigInt) (Just . cast) cast OT
AmountUntaxedOT:Column
AmountUntaxedOT=notNull Product "amount_untaxed" (DoublePrecision) (Just . toEX20) cast OT
AmountTotalOT:Column
AmountTotalOT=notNull Product "amount_total" (DoublePrecision) (Just . toINC20) cast OT
NameOT:Column
NameOT=notNull String "name" (Text) (Just . cast) cast OT
PartnerShippingIdOT:Column
PartnerShippingIdOT=notNull Bits32 "partner_shipping_id" (BigInt) (Just . cast) cast OT
PickingPolicyOT:Column
PickingPolicyOT=notNull String "picking_policy" (Text) (Just . cast) cast OT
CarrierIdOT:Column
CarrierIdOT=nullable Bits32 "carrier_id" (BigInt) (Just . cast) cast OT
--O2M
RequestedDateOT:Column
RequestedDateOT=nullable Date "requested_date" (VarChar 10) (Just . cast) cast OT

PkACVT:Column
PkACVT=notNull Bits32 "id" (BigInt) (Just . cast) cast ACVT
NumberACVT:Column
NumberACVT=notNull String "number" (VarChar 32) (Just . cast) cast ACVT
PartnerIdACVT:Column
PartnerIdACVT=nullable Bits32 "partner_id" (BigInt) (Just . cast) cast ACVT
JournalIdACVT:Column
JournalIdACVT=nullable Bits32 "journal_id" (BigInt) (Just . cast) cast ACVT
AmountACVT:Column
AmountACVT=notNull EQty "amount" (DoublePrecision) (Just . cast) cast ACVT

PkSMT:Column
PkSMT=notNull Bits32 "id" (BigInt) (Just . cast) cast SMT
OriginSMT:Column
OriginSMT=nullable String "origin" (VarChar 64) (Just . cast) cast SMT
PriceUnitSMT:Column
PriceUnitSMT=nullable EQty "price_unit" (DoublePrecision) (Just . cast) cast SMT
ProductQtySMT:Column
ProductQtySMT=notNull EQty "product_qty" (DoublePrecision) (Just . cast) cast SMT
ProductIdSMT:Column
ProductIdSMT=notNull Bits32 "product_id" (BigInt) (Just . cast) cast SMT
LocationIdSMT:Column
LocationIdSMT=notNull Bits32 "location_id" (BigInt) (Just . cast) cast SMT
LocationDestIdSMT:Column
LocationDestIdSMT=notNull Bits32 "location_dest_id" (BigInt) (Just . cast) cast SMT
PickingIdSMT:Column
PickingIdSMT=nullable Bits32 "picking_id" (BigInt) (Just . cast) cast SMT
PurchaseLineIdSMT:Column
PurchaseLineIdSMT=notNull Bits32 "purchase_line_id" (BigInt) (Just . cast) cast SMT
SaleLineIdSMT:Column
SaleLineIdSMT=notNull Bits32 "sale_line_id" (BigInt) (Just . cast) cast SMT
StateSMT:Column
StateSMT=notNull String "state" (Text) (Just . cast) cast SMT

PkSPT:Column
PkSPT=notNull Bits32 "id" (BigInt) (Just . cast) cast SPT
OriginSPT:Column
OriginSPT=nullable String "origin" (VarChar 64) (Just . cast) cast SPT
BackorderIdSPT:Column
BackorderIdSPT=nullable Bits32 "backorder_id" (BigInt) (Just . cast) cast SPT
DateDoneSPT:Column
DateDoneSPT=notNull Date "date_done" (VarChar 10) (Just . cast) cast SPT
PartnerIdSPT:Column
PartnerIdSPT=nullable Bits32 "partner_id" (BigInt) (Just . cast) cast SPT
MinDateSPT:Column
MinDateSPT=notNull Date "min_date" (VarChar 10) (Just . cast) cast SPT
NameSPT:Column
NameSPT=notNull String "name" (VarChar 64) (Just . cast) cast SPT
StateSPT:Column
StateSPT=notNull String "state" (Text) (Just . cast) cast SPT
--O2M

InvoiceLineIdM2M_IT:Column
InvoiceLineIdM2M_IT=notNull Bits32 "invoice_line_id" (BigInt) (Just . cast) cast M2M_IT
TaxIdM2M_IT:Column
TaxIdM2M_IT=notNull Bits32 "tax_id" (BigInt) (Just . cast) cast M2M_IT

PkILT:Column
PkILT=notNull Bits32 "id" (BigInt) (Just . cast) cast ILT
InvoiceIdILT:Column
InvoiceIdILT=notNull Bits32 "invoice_id" (BigInt) (Just . cast) cast ILT
PriceUnitILT:Column
PriceUnitILT=notNull EQty "price_unit" (DoublePrecision) (Just . cast) cast ILT
QuantityILT:Column
QuantityILT=notNull EQty "quantity" (DoublePrecision) (Just . cast) cast ILT
NameILT:Column
NameILT=notNull String "name" (Text) (Just . cast) cast ILT
ProductIdILT:Column
ProductIdILT=nullable Bits32 "product_id" (BigInt) (Just . cast) cast ILT
--M2M
DiscountILT:Column
DiscountILT=nullable EQty "discount" (DoublePrecision) (Just . percent) cast ILT

PkIT:Column
PkIT=notNull Bits32 "id" (BigInt) (Just . cast) cast IT
OriginIT:Column
OriginIT=nullable String "origin" (VarChar 64) (Just . cast) cast IT
DateDueIT:Column
DateDueIT=notNull Date "date_due" (VarChar 10) (Just . cast) cast IT
NumberIT:Column
NumberIT=notNull String "number" (VarChar 32) (Just . cast) cast IT
AccountIdIT:Column
AccountIdIT=nullable Bits32 "account_id" (BigInt) (Just . cast) cast IT
PartnerIdIT:Column
PartnerIdIT=nullable Bits32 "partner_id" (BigInt) (Just . cast) cast IT
JournalIdIT:Column
JournalIdIT=nullable Bits32 "journal_id" (BigInt) (Just . cast) cast IT
AmountTaxIT:Column
AmountTaxIT=notNull Product "amount_tax" (DoublePrecision) (Just . toTaxA) cast IT
StateIT:Column
StateIT=notNull String "state" (Text) (Just . cast) cast IT
TypeIT:Column
TypeIT=nullable String "type" (VarChar 64) (Just . cast) cast IT
DateInvoiceIT:Column
DateInvoiceIT=notNull Date "date_invoice" (VarChar 10) (Just . cast) cast IT
AmountUntaxedIT:Column
AmountUntaxedIT=notNull Product "amount_untaxed" (DoublePrecision) (Just . toEX20) cast IT
AmountTotalIT:Column
AmountTotalIT=notNull Product "amount_total" (DoublePrecision) (Just . toINC20) cast IT
--O2M
namespace PrimProductTemplate
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkPT, NamePT, ListPricePT]

      public export
      PT_NP : Table
      PT_NP = MkTable "product_template" PrimProductTemplate.PrimCols


      export
      toRecord : GetRow PrimProductTemplate.PrimCols -> PrimProductTemplate.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimProductTemplate.RecordModel )
      read_records_c c op = do
          rows <- get c PT_NP (columns PT_NP) (PrimProductTemplate.domain&&op)
          let ret_s = [ PrimProductTemplate.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimProductTemplate.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimProductTemplate.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimProductTemplate.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimProductTemplate.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimProductTemplate.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimProductTemplate.main_runET op))
          pure l1
namespace PrimProduct
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkPP, ProductTmplIdPP, TradePP, RetailPP, ContractPP, DefaultCodePP]

      public export
      PP_NP : Table
      PP_NP = MkTable "product_product" PrimProduct.PrimCols


      export
      toRecord : GetRow PrimProduct.PrimCols -> PrimProduct.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimProduct.RecordModel )
      read_records_c c op = do
          rows <- get c PP_NP (columns PP_NP) (PrimProduct.domain&&op)
          let ret_s = [ PrimProduct.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimProduct.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimProduct.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimProduct.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimProduct.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimProduct.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimProduct.main_runET op))
          pure l1
namespace PrimBoM
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkBOM, ProductQtyBOM, BomIdBOM, ProductIdBOM]

      public export
      BOM_NP : Table
      BOM_NP = MkTable "mrp_bom" PrimBoM.PrimCols


      export
      toRecord : GetRow PrimBoM.PrimCols -> PrimBoM.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimBoM.RecordModel )
      read_records_c c op = do
          rows <- get c BOM_NP (columns BOM_NP) (PrimBoM.domain&&op)
          let ret_s = [ PrimBoM.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimBoM.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimBoM.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimBoM.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimBoM.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimBoM.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimBoM.main_runET op))
          pure l1
namespace PrimResPartner
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkRPT, NameRPT, UseParentAddressRPT, ActiveRPT, StreetRPT, ContractRPT, CityRPT, ZipRPT, CountryIdRPT, ParentIdRPT, EmailRPT, Street2RPT]

      public export
      RPT_NP : Table
      RPT_NP = MkTable "res_partner" PrimResPartner.PrimCols


      export
      toRecord : GetRow PrimResPartner.PrimCols -> PrimResPartner.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimResPartner.RecordModel )
      read_records_c c op = do
          rows <- get c RPT_NP (columns RPT_NP) (PrimResPartner.domain&&op)
          let ret_s = [ PrimResPartner.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimResPartner.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimResPartner.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimResPartner.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimResPartner.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimResPartner.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimResPartner.main_runET op))
          pure l1
namespace PrimM2M_OrderTax
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [OrderLineIdM2M_ST, TaxIdM2M_ST]

      public export
      M2M_ST_NP : Table
      M2M_ST_NP = MkTable "sale_order_tax" PrimM2M_OrderTax.PrimCols


      export
      toRecord : GetRow PrimM2M_OrderTax.PrimCols -> PrimM2M_OrderTax.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimM2M_OrderTax.RecordModel )
      read_records_c c op = do
          rows <- get c M2M_ST_NP (columns M2M_ST_NP) (PrimM2M_OrderTax.domain&&op)
          let ret_s = [ PrimM2M_OrderTax.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimM2M_OrderTax.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimM2M_OrderTax.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimM2M_OrderTax.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimM2M_OrderTax.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimM2M_OrderTax.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimM2M_OrderTax.main_runET op))
          pure l1
namespace PrimOrderTax
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkOTax, NameOTax, DescriptionOTax, AmountOTax, TypeOTax, PriceIncludeOTax]

      public export
      OTax_NP : Table
      OTax_NP = MkTable "account_tax" PrimOrderTax.PrimCols


      export
      toRecord : GetRow PrimOrderTax.PrimCols -> PrimOrderTax.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimOrderTax.RecordModel )
      read_records_c c op = do
          rows <- get c OTax_NP (columns OTax_NP) (PrimOrderTax.domain&&op)
          let ret_s = [ PrimOrderTax.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimOrderTax.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimOrderTax.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimOrderTax.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimOrderTax.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimOrderTax.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimOrderTax.main_runET op))
          pure l1
namespace PrimOrderLine
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkOLT, PriceUnitOLT, ProductUomQtyOLT, DiscountOLT, DeliveryLineOLT, OrderIdOLT, ProductIdOLT]

      public export
      OLT_NP : Table
      OLT_NP = MkTable "sale_order_line" PrimOrderLine.PrimCols


      export
      toRecord : GetRow PrimOrderLine.PrimCols -> PrimOrderLine.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimOrderLine.RecordModel )
      read_records_c c op = do
          rows <- get c OLT_NP (columns OLT_NP) (PrimOrderLine.domain&&op)
          let ret_s = [ PrimOrderLine.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimOrderLine.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimOrderLine.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimOrderLine.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimOrderLine.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimOrderLine.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimOrderLine.main_runET op))
          pure l1
namespace PrimOrder
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkOT, OriginOT, OrderPolicyOT, DateOrderOT, PartnerIdOT, AmountTaxOT, StateOT, PartnerInvoiceIdOT, AmountUntaxedOT, AmountTotalOT, NameOT, PartnerShippingIdOT, PickingPolicyOT, CarrierIdOT, RequestedDateOT]

      public export
      OT_NP : Table
      OT_NP = MkTable "sale_order" PrimOrder.PrimCols


      export
      toRecord : GetRow PrimOrder.PrimCols -> PrimOrder.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimOrder.RecordModel )
      read_records_c c op = do
          rows <- get c OT_NP (columns OT_NP) (PrimOrder.domain&&op)
          let ret_s = [ PrimOrder.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimOrder.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimOrder.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimOrder.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimOrder.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimOrder.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimOrder.main_runET op))
          pure l1
namespace PrimAccountVoucher
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkACVT, NumberACVT, PartnerIdACVT, JournalIdACVT, AmountACVT]

      public export
      ACVT_NP : Table
      ACVT_NP = MkTable "account_voucher" PrimAccountVoucher.PrimCols


      export
      toRecord : GetRow PrimAccountVoucher.PrimCols -> PrimAccountVoucher.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimAccountVoucher.RecordModel )
      read_records_c c op = do
          rows <- get c ACVT_NP (columns ACVT_NP) (PrimAccountVoucher.domain&&op)
          let ret_s = [ PrimAccountVoucher.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimAccountVoucher.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimAccountVoucher.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimAccountVoucher.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimAccountVoucher.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimAccountVoucher.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimAccountVoucher.main_runET op))
          pure l1
namespace PrimStockMove
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkSMT, OriginSMT, PriceUnitSMT, ProductQtySMT, ProductIdSMT, LocationIdSMT, LocationDestIdSMT, PickingIdSMT, PurchaseLineIdSMT, SaleLineIdSMT, StateSMT]

      public export
      SMT_NP : Table
      SMT_NP = MkTable "stock_move" PrimStockMove.PrimCols


      export
      toRecord : GetRow PrimStockMove.PrimCols -> PrimStockMove.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimStockMove.RecordModel )
      read_records_c c op = do
          rows <- get c SMT_NP (columns SMT_NP) (PrimStockMove.domain&&op)
          let ret_s = [ PrimStockMove.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimStockMove.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimStockMove.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimStockMove.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimStockMove.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimStockMove.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimStockMove.main_runET op))
          pure l1
namespace PrimStockPicking
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkSPT, OriginSPT, BackorderIdSPT, DateDoneSPT, PartnerIdSPT, MinDateSPT, NameSPT, StateSPT]

      public export
      SPT_NP : Table
      SPT_NP = MkTable "stock_picking" PrimStockPicking.PrimCols


      export
      toRecord : GetRow PrimStockPicking.PrimCols -> PrimStockPicking.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimStockPicking.RecordModel )
      read_records_c c op = do
          rows <- get c SPT_NP (columns SPT_NP) (PrimStockPicking.domain&&op)
          let ret_s = [ PrimStockPicking.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimStockPicking.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimStockPicking.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimStockPicking.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimStockPicking.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimStockPicking.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimStockPicking.main_runET op))
          pure l1
namespace PrimM2M_InvoiceTax
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [InvoiceLineIdM2M_IT, TaxIdM2M_IT]

      public export
      M2M_IT_NP : Table
      M2M_IT_NP = MkTable "account_invoice_line_tax" PrimM2M_InvoiceTax.PrimCols


      export
      toRecord : GetRow PrimM2M_InvoiceTax.PrimCols -> PrimM2M_InvoiceTax.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimM2M_InvoiceTax.RecordModel )
      read_records_c c op = do
          rows <- get c M2M_IT_NP (columns M2M_IT_NP) (PrimM2M_InvoiceTax.domain&&op)
          let ret_s = [ PrimM2M_InvoiceTax.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimM2M_InvoiceTax.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimM2M_InvoiceTax.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimM2M_InvoiceTax.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimM2M_InvoiceTax.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimM2M_InvoiceTax.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimM2M_InvoiceTax.main_runET op))
          pure l1
namespace PrimAccountInvoiceLine
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkILT, InvoiceIdILT, PriceUnitILT, QuantityILT, NameILT, ProductIdILT, DiscountILT]

      public export
      ILT_NP : Table
      ILT_NP = MkTable "account_invoice_line" PrimAccountInvoiceLine.PrimCols


      export
      toRecord : GetRow PrimAccountInvoiceLine.PrimCols -> PrimAccountInvoiceLine.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimAccountInvoiceLine.RecordModel )
      read_records_c c op = do
          rows <- get c ILT_NP (columns ILT_NP) (PrimAccountInvoiceLine.domain&&op)
          let ret_s = [ PrimAccountInvoiceLine.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimAccountInvoiceLine.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimAccountInvoiceLine.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimAccountInvoiceLine.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimAccountInvoiceLine.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimAccountInvoiceLine.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimAccountInvoiceLine.main_runET op))
          pure l1
namespace PrimAccountInvoice
      domain : Op
      domain = (True)
      export
      PrimCols : List Column
      PrimCols = [PkIT, OriginIT, DateDueIT, NumberIT, AccountIdIT, PartnerIdIT, JournalIdIT, AmountTaxIT, StateIT, TypeIT, DateInvoiceIT, AmountUntaxedIT, AmountTotalIT]

      public export
      IT_NP : Table
      IT_NP = MkTable "account_invoice" PrimAccountInvoice.PrimCols


      export
      toRecord : GetRow PrimAccountInvoice.PrimCols -> PrimAccountInvoice.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List PrimAccountInvoice.RecordModel )
      read_records_c c op = do
          rows <- get c IT_NP (columns IT_NP) (PrimAccountInvoice.domain&&op)
          let ret_s = [ PrimAccountInvoice.toRecord ox | ox <- rows]
          pure ret_s

      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List PrimAccountInvoice.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- PrimAccountInvoice.read_records_c c op
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List PrimAccountInvoice.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (PrimAccountInvoice.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List PrimAccountInvoice.RecordModel )
      read op = do
          l1 <- (liftIO $ (PrimAccountInvoice.main_runET op))
          pure l1

namespace BrowseProductTemplate
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseProductTemplate.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimProductTemplate.RecordModel) ->io (List  BrowseProductTemplate.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimProductTemplate.MkRecordModel pk name list_price)::xs) = do
            let ret =(BrowseProductTemplate.MkRecordModel pk name list_price)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseProductTemplate.RecordModel)
          ret_x = do
            rows <- PrimProductTemplate.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseProductTemplate.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseProductTemplate.read_records_c c (op && BrowseProductTemplate.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseProductTemplate.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseProductTemplate.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseProductTemplate.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseProductTemplate.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseProductTemplate.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkPT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseProductTemplate.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseProductTemplate.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseProductTemplate.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseProductTemplate.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseProductTemplate.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseProductTemplate.main_runET_ids xs op))
          pure l1

namespace BrowseProduct
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseProduct.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimProduct.RecordModel) ->io (List  BrowseProduct.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimProduct.MkRecordModel pk product_tmpl_id trade retail contract default_code)::xs) = do
            let muf_m2o = ((PkPT==(cast product_tmpl_id))) --&&op
            product_tmpl_id <- PrimProductTemplate.read_records_c c muf_m2o
            let ret =(BrowseProduct.MkRecordModel pk product_tmpl_id trade retail contract default_code)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseProduct.RecordModel)
          ret_x = do
            rows <- PrimProduct.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseProduct.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseProduct.read_records_c c (op && BrowseProduct.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseProduct.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseProduct.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseProduct.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseProduct.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseProduct.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkPP==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseProduct.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseProduct.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseProduct.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseProduct.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseProduct.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseProduct.main_runET_ids xs op))
          pure l1

namespace BrowseBoM
      domain : Op
      domain = (IsNull BomIdBOM)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseBoM.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimBoM.RecordModel) ->io (List  BrowseBoM.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimBoM.MkRecordModel pk product_qty bom_id product_id)::xs) = do
            bom_lines <- BrowseBoM.read_records_c c ((BomIdBOM==Just(cast pk)))
            let ret =(BrowseBoM.MkRecordModel pk product_qty bom_id bom_lines product_id)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseBoM.RecordModel)
          ret_x = do
            rows <- PrimBoM.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseBoM.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseBoM.read_records_c c (op && BrowseBoM.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseBoM.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseBoM.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseBoM.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseBoM.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseBoM.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkBOM==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseBoM.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseBoM.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseBoM.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseBoM.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseBoM.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseBoM.main_runET_ids xs op))
          pure l1

namespace BrowseResPartner
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseResPartner.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimResPartner.RecordModel) ->io (List  BrowseResPartner.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimResPartner.MkRecordModel pk name use_parent_address active street contract city zip country_id parent_id email street2)::xs) = do
            child_ids <- BrowseResPartner.read_records_c c ((ParentIdRPT==Just(cast pk)))
            let ret =(BrowseResPartner.MkRecordModel pk name use_parent_address active street contract city zip country_id parent_id child_ids email street2)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseResPartner.RecordModel)
          ret_x = do
            rows <- PrimResPartner.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseResPartner.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseResPartner.read_records_c c (op && BrowseResPartner.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseResPartner.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseResPartner.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseResPartner.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseResPartner.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseResPartner.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkRPT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseResPartner.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseResPartner.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseResPartner.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseResPartner.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseResPartner.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseResPartner.main_runET_ids xs op))
          pure l1

namespace BrowseM2M_OrderTax
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = True
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseM2M_OrderTax.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimM2M_OrderTax.RecordModel) ->io (List  BrowseM2M_OrderTax.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimM2M_OrderTax.MkRecordModel order_line_id tax_id)::xs) = do
            let ret =(BrowseM2M_OrderTax.MkRecordModel order_line_id tax_id)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseM2M_OrderTax.RecordModel)
          ret_x = do
            rows <- PrimM2M_OrderTax.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseM2M_OrderTax.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseM2M_OrderTax.read_records_c c (op && BrowseM2M_OrderTax.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseM2M_OrderTax.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseM2M_OrderTax.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseM2M_OrderTax.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseM2M_OrderTax.main_runET op))
          pure l1

namespace BrowseOrderTax
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseOrderTax.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimOrderTax.RecordModel) ->io (List  BrowseOrderTax.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrderTax.MkRecordModel pk name description amount type price_include)::xs) = do
            let ret =(BrowseOrderTax.MkRecordModel pk name description amount type price_include)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseOrderTax.RecordModel)
          ret_x = do
            rows <- PrimOrderTax.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseOrderTax.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseOrderTax.read_records_c c (op && BrowseOrderTax.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseOrderTax.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseOrderTax.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseOrderTax.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseOrderTax.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseOrderTax.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkOTax==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseOrderTax.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseOrderTax.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseOrderTax.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseOrderTax.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseOrderTax.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseOrderTax.main_runET_ids xs op))
          pure l1

namespace BrowseOrderLine
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseOrderLine.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimOrderLine.RecordModel) ->io (List  BrowseOrderLine.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrderLine.MkRecordModel pk price_unit product_uom_qty discount delivery_line order_id product_id)::xs) = do
            let muf_m2m = ((JC PkOTax TaxIdM2M_ST)&&(OrderLineIdM2M_ST==(cast pk)))
            tax_ids_np<-getJoin c OTax_NP M2M_ST_NP (columns OTax_NP) muf_m2m
            let tax_ids=[PrimOrderTax.toRecord ox |ox <-tax_ids_np]
            let ret =(BrowseOrderLine.MkRecordModel pk price_unit product_uom_qty discount delivery_line order_id product_id tax_ids)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseOrderLine.RecordModel)
          ret_x = do
            rows <- PrimOrderLine.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseOrderLine.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseOrderLine.read_records_c c (op && BrowseOrderLine.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseOrderLine.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseOrderLine.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseOrderLine.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseOrderLine.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseOrderLine.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkOLT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseOrderLine.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseOrderLine.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseOrderLine.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseOrderLine.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseOrderLine.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseOrderLine.main_runET_ids xs op))
          pure l1

namespace BrowseOrder
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseOrder.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimOrder.RecordModel) ->io (List  BrowseOrder.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id requested_date)::xs) = do
            order_line <- BrowseOrderLine.read_records_c c ((OrderIdOLT==(cast pk)))
            let ret =(BrowseOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id order_line requested_date)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseOrder.RecordModel)
          ret_x = do
            rows <- PrimOrder.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseOrder.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseOrder.read_records_c c (op && BrowseOrder.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseOrder.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseOrder.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseOrder.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseOrder.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseOrder.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkOT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseOrder.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseOrder.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseOrder.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseOrder.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseOrder.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseOrder.main_runET_ids xs op))
          pure l1

namespace BrowseAccountVoucher
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseAccountVoucher.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimAccountVoucher.RecordModel) ->io (List  BrowseAccountVoucher.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimAccountVoucher.MkRecordModel pk number partner_id journal_id amount)::xs) = do
            let ret =(BrowseAccountVoucher.MkRecordModel pk number partner_id journal_id amount)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseAccountVoucher.RecordModel)
          ret_x = do
            rows <- PrimAccountVoucher.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseAccountVoucher.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseAccountVoucher.read_records_c c (op && BrowseAccountVoucher.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseAccountVoucher.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseAccountVoucher.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseAccountVoucher.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseAccountVoucher.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseAccountVoucher.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkACVT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseAccountVoucher.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseAccountVoucher.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseAccountVoucher.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseAccountVoucher.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseAccountVoucher.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseAccountVoucher.main_runET_ids xs op))
          pure l1

namespace BrowseStockMove
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseStockMove.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimStockMove.RecordModel) ->io (List  BrowseStockMove.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimStockMove.MkRecordModel pk origin price_unit product_qty product_id location_id location_dest_id picking_id purchase_line_id sale_line_id state)::xs) = do
            let ret =(BrowseStockMove.MkRecordModel pk origin price_unit product_qty product_id location_id location_dest_id picking_id purchase_line_id sale_line_id state)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseStockMove.RecordModel)
          ret_x = do
            rows <- PrimStockMove.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseStockMove.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseStockMove.read_records_c c (op && BrowseStockMove.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseStockMove.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseStockMove.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseStockMove.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseStockMove.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseStockMove.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkSMT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseStockMove.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseStockMove.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseStockMove.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseStockMove.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseStockMove.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseStockMove.main_runET_ids xs op))
          pure l1

namespace BrowseStockPicking
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseStockPicking.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimStockPicking.RecordModel) ->io (List  BrowseStockPicking.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimStockPicking.MkRecordModel pk origin backorder_id date_done partner_id min_date name state)::xs) = do
            move_ids <- BrowseStockMove.read_records_c c ((PickingIdSMT==Just(cast pk)))
            let ret =(BrowseStockPicking.MkRecordModel pk origin backorder_id date_done partner_id min_date name state move_ids)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseStockPicking.RecordModel)
          ret_x = do
            rows <- PrimStockPicking.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseStockPicking.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseStockPicking.read_records_c c (op && BrowseStockPicking.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseStockPicking.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseStockPicking.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseStockPicking.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseStockPicking.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseStockPicking.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkSPT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseStockPicking.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseStockPicking.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseStockPicking.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseStockPicking.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseStockPicking.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseStockPicking.main_runET_ids xs op))
          pure l1

namespace BrowseM2M_InvoiceTax
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = True
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseM2M_InvoiceTax.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimM2M_InvoiceTax.RecordModel) ->io (List  BrowseM2M_InvoiceTax.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimM2M_InvoiceTax.MkRecordModel invoice_line_id tax_id)::xs) = do
            let ret =(BrowseM2M_InvoiceTax.MkRecordModel invoice_line_id tax_id)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseM2M_InvoiceTax.RecordModel)
          ret_x = do
            rows <- PrimM2M_InvoiceTax.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseM2M_InvoiceTax.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseM2M_InvoiceTax.read_records_c c (op && BrowseM2M_InvoiceTax.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseM2M_InvoiceTax.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseM2M_InvoiceTax.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseM2M_InvoiceTax.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseM2M_InvoiceTax.main_runET op))
          pure l1

namespace BrowseAccountInvoiceLine
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseAccountInvoiceLine.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimAccountInvoiceLine.RecordModel) ->io (List  BrowseAccountInvoiceLine.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimAccountInvoiceLine.MkRecordModel pk invoice_id price_unit quantity name product_id discount)::xs) = do
            let muf_m2m = ((JC PkOTax TaxIdM2M_IT)&&(InvoiceLineIdM2M_IT==(cast pk)))
            tax_ids_np<-getJoin c OTax_NP M2M_IT_NP (columns OTax_NP) muf_m2m
            let tax_ids=[PrimOrderTax.toRecord ox |ox <-tax_ids_np]
            let muf_m2o = ((PkIT==(cast invoice_id))) --&&op
            invoice_id <- PrimAccountInvoice.read_records_c c muf_m2o
            let ret =(BrowseAccountInvoiceLine.MkRecordModel pk invoice_id price_unit quantity name product_id tax_ids discount)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseAccountInvoiceLine.RecordModel)
          ret_x = do
            rows <- PrimAccountInvoiceLine.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseAccountInvoiceLine.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseAccountInvoiceLine.read_records_c c (op && BrowseAccountInvoiceLine.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseAccountInvoiceLine.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseAccountInvoiceLine.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseAccountInvoiceLine.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseAccountInvoiceLine.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseAccountInvoiceLine.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkILT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseAccountInvoiceLine.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseAccountInvoiceLine.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseAccountInvoiceLine.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseAccountInvoiceLine.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseAccountInvoiceLine.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseAccountInvoiceLine.main_runET_ids xs op))
          pure l1

namespace BrowseAccountInvoice
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List BrowseAccountInvoice.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimAccountInvoice.RecordModel) ->io (List  BrowseAccountInvoice.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimAccountInvoice.MkRecordModel pk origin date_due number account_id partner_id journal_id amount_tax state type date_invoice amount_untaxed amount_total)::xs) = do
            invoice_line <- BrowseAccountInvoiceLine.read_records_c c ((InvoiceIdILT==(cast pk)))
            let ret =(BrowseAccountInvoice.MkRecordModel pk origin date_due number account_id partner_id journal_id amount_tax state type date_invoice amount_untaxed amount_total invoice_line)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List BrowseAccountInvoice.RecordModel)
          ret_x = do
            rows <- PrimAccountInvoice.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List BrowseAccountInvoice.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- BrowseAccountInvoice.read_records_c c (op && BrowseAccountInvoice.domain)
          finish c
          pure ret

      export
      main_runET : (op:Op) -> IO (List BrowseAccountInvoice.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (BrowseAccountInvoice.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List BrowseAccountInvoice.RecordModel )
      read op = do
          l1 <- (liftIO $ (BrowseAccountInvoice.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List BrowseAccountInvoice.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkIT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List BrowseAccountInvoice.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- BrowseAccountInvoice.read_records_c_ids c xs op
          finish c
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List BrowseAccountInvoice.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (BrowseAccountInvoice.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List BrowseAccountInvoice.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (BrowseAccountInvoice.main_runET_ids xs op))
          pure l1
