module Odoo.Schema.PJB

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

import Category.Transaction.Types
import Data.Ratio

import Generics.Derive

import JSON

import Ledger.PG.Config
import Control.Monad.Either

%language ElabReflection
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
AmountTaxOT=notNull Price "amount_tax" (DoublePrecision) (Just . toTaxA) cast OT
StateOT:Column
StateOT=notNull String "state" (Text) (Just . cast) cast OT
PartnerInvoiceIdOT:Column
PartnerInvoiceIdOT=notNull Bits32 "partner_invoice_id" (BigInt) (Just . cast) cast OT
AmountUntaxedOT:Column
AmountUntaxedOT=notNull Price "amount_untaxed" (DoublePrecision) (Just . toEX20) cast OT
AmountTotalOT:Column
AmountTotalOT=notNull Price "amount_total" (DoublePrecision) (Just . toINC20) cast OT
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
PriceUnitSMT=notNull EQty "price_unit" (DoublePrecision) (Just . cast) cast SMT
ProductQtySMT:Column
ProductQtySMT=notNull EQty "product_qty" (DoublePrecision) (Just . cast) cast SMT
ProductIdSMT:Column
ProductIdSMT=nullable Bits32 "product_id" (BigInt) (Just . cast) cast SMT
LocationIdSMT:Column
LocationIdSMT=nullable Bits32 "location_id" (BigInt) (Just . cast) cast SMT
LocationDestIdSMT:Column
LocationDestIdSMT=nullable Bits32 "location_dest_id" (BigInt) (Just . cast) cast SMT
PickingIdSMT:Column
PickingIdSMT=nullable Bits32 "picking_id" (BigInt) (Just . cast) cast SMT
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
AmountTaxIT=notNull Price "amount_tax" (DoublePrecision) (Just . toTaxA) cast IT
StateIT:Column
StateIT=notNull String "state" (Text) (Just . cast) cast IT
TypeIT:Column
TypeIT=nullable String "type" (VarChar 64) (Just . cast) cast IT
DateInvoiceIT:Column
DateInvoiceIT=notNull Date "date_invoice" (VarChar 10) (Just . cast) cast IT
AmountUntaxedIT:Column
AmountUntaxedIT=notNull Price "amount_untaxed" (DoublePrecision) (Just . toEX20) cast IT
AmountTotalIT:Column
AmountTotalIT=notNull Price "amount_total" (DoublePrecision) (Just . toINC20) cast IT
--O2M

namespace PrimResPartner
      domain : Op
      domain = (True)
      PrimCols : List Column
      PrimCols = [PkRPT, NameRPT, UseParentAddressRPT, ActiveRPT, StreetRPT, ContractRPT, CityRPT, ZipRPT, CountryIdRPT, ParentIdRPT, EmailRPT, Street2RPT]

      RPT_NP : Table
      RPT_NP = MkTable "res_partner" PrimResPartner.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkRPT)
          name:(idrisTpe NameRPT)
          use_parent_address:(idrisTpe UseParentAddressRPT)
          active:(idrisTpe ActiveRPT)
          street:(idrisTpe StreetRPT)
          contract:(idrisTpe ContractRPT)
          city:(idrisTpe CityRPT)
          zip:(idrisTpe ZipRPT)
          country_id:(idrisTpe CountryIdRPT)
          parent_id:(idrisTpe ParentIdRPT)
          --O2M
          email:(idrisTpe EmailRPT)
          street2:(idrisTpe Street2RPT)
      %runElab derive "PrimResPartner.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [OrderLineIdM2M_ST, TaxIdM2M_ST]

      M2M_ST_NP : Table
      M2M_ST_NP = MkTable "sale_order_tax" PrimM2M_OrderTax.PrimCols

      record RecordModel where
          constructor MkRecordModel
          order_line_id:(idrisTpe OrderLineIdM2M_ST)
          tax_id:(idrisTpe TaxIdM2M_ST)
      %runElab derive "PrimM2M_OrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [PkOTax, NameOTax, DescriptionOTax, AmountOTax, TypeOTax, PriceIncludeOTax]

      OTax_NP : Table
      OTax_NP = MkTable "account_tax" PrimOrderTax.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkOTax)
          name:(idrisTpe NameOTax)
          description:(idrisTpe DescriptionOTax)
          amount:(idrisTpe AmountOTax)
          type:(idrisTpe TypeOTax)
          price_include:(idrisTpe PriceIncludeOTax)
      %runElab derive "PrimOrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [PkOLT, PriceUnitOLT, ProductUomQtyOLT, DiscountOLT, DeliveryLineOLT, OrderIdOLT, ProductIdOLT]

      OLT_NP : Table
      OLT_NP = MkTable "sale_order_line" PrimOrderLine.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkOLT)
          price_unit:(idrisTpe PriceUnitOLT)
          product_uom_qty:(idrisTpe ProductUomQtyOLT)
          discount:(idrisTpe DiscountOLT)
          delivery_line:(idrisTpe DeliveryLineOLT)
          order_id:(idrisTpe OrderIdOLT)
          product_id:(idrisTpe ProductIdOLT)
          --M2M
      %runElab derive "PrimOrderLine.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [PkOT, OriginOT, OrderPolicyOT, DateOrderOT, PartnerIdOT, AmountTaxOT, StateOT, PartnerInvoiceIdOT, AmountUntaxedOT, AmountTotalOT, NameOT, PartnerShippingIdOT, PickingPolicyOT, CarrierIdOT, RequestedDateOT]

      OT_NP : Table
      OT_NP = MkTable "sale_order" PrimOrder.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkOT)
          origin:(idrisTpe OriginOT)
          order_policy:(idrisTpe OrderPolicyOT)
          date_order:(idrisTpe DateOrderOT)
          partner_id:(idrisTpe PartnerIdOT)
          amount_tax:(idrisTpe AmountTaxOT)
          state:(idrisTpe StateOT)
          partner_invoice_id:(idrisTpe PartnerInvoiceIdOT)
          amount_untaxed:(idrisTpe AmountUntaxedOT)
          amount_total:(idrisTpe AmountTotalOT)
          name:(idrisTpe NameOT)
          partner_shipping_id:(idrisTpe PartnerShippingIdOT)
          picking_policy:(idrisTpe PickingPolicyOT)
          carrier_id:(idrisTpe CarrierIdOT)
          --O2M
          requested_date:(idrisTpe RequestedDateOT)
      %runElab derive "PrimOrder.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [PkACVT, NumberACVT, PartnerIdACVT, JournalIdACVT, AmountACVT]

      ACVT_NP : Table
      ACVT_NP = MkTable "account_voucher" PrimAccountVoucher.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkACVT)
          number:(idrisTpe NumberACVT)
          partner_id:(idrisTpe PartnerIdACVT)
          journal_id:(idrisTpe JournalIdACVT)
          amount:(idrisTpe AmountACVT)
      %runElab derive "PrimAccountVoucher.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [PkSMT, OriginSMT, PriceUnitSMT, ProductQtySMT, ProductIdSMT, LocationIdSMT, LocationDestIdSMT, PickingIdSMT, StateSMT]

      SMT_NP : Table
      SMT_NP = MkTable "stock_move" PrimStockMove.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkSMT)
          origin:(idrisTpe OriginSMT)
          price_unit:(idrisTpe PriceUnitSMT)
          product_qty:(idrisTpe ProductQtySMT)
          product_id:(idrisTpe ProductIdSMT)
          location_id:(idrisTpe LocationIdSMT)
          location_dest_id:(idrisTpe LocationDestIdSMT)
          picking_id:(idrisTpe PickingIdSMT)
          state:(idrisTpe StateSMT)
      %runElab derive "PrimStockMove.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [PkSPT, OriginSPT, BackorderIdSPT, DateDoneSPT, PartnerIdSPT, MinDateSPT, NameSPT, StateSPT]

      SPT_NP : Table
      SPT_NP = MkTable "stock_picking" PrimStockPicking.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkSPT)
          origin:(idrisTpe OriginSPT)
          backorder_id:(idrisTpe BackorderIdSPT)
          date_done:(idrisTpe DateDoneSPT)
          partner_id:(idrisTpe PartnerIdSPT)
          min_date:(idrisTpe MinDateSPT)
          name:(idrisTpe NameSPT)
          state:(idrisTpe StateSPT)
          --O2M
      %runElab derive "PrimStockPicking.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [InvoiceLineIdM2M_IT, TaxIdM2M_IT]

      M2M_IT_NP : Table
      M2M_IT_NP = MkTable "account_invoice_line_tax" PrimM2M_InvoiceTax.PrimCols

      record RecordModel where
          constructor MkRecordModel
          invoice_line_id:(idrisTpe InvoiceLineIdM2M_IT)
          tax_id:(idrisTpe TaxIdM2M_IT)
      %runElab derive "PrimM2M_InvoiceTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [PkILT, InvoiceIdILT, PriceUnitILT, QuantityILT, NameILT, ProductIdILT, DiscountILT]

      ILT_NP : Table
      ILT_NP = MkTable "account_invoice_line" PrimAccountInvoiceLine.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkILT)
          invoice_id:(idrisTpe InvoiceIdILT)
          price_unit:(idrisTpe PriceUnitILT)
          quantity:(idrisTpe QuantityILT)
          name:(idrisTpe NameILT)
          product_id:(idrisTpe ProductIdILT)
          --M2M
          discount:(idrisTpe DiscountILT)
      %runElab derive "PrimAccountInvoiceLine.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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
      PrimCols : List Column
      PrimCols = [PkIT, OriginIT, DateDueIT, NumberIT, AccountIdIT, PartnerIdIT, JournalIdIT, AmountTaxIT, StateIT, TypeIT, DateInvoiceIT, AmountUntaxedIT, AmountTotalIT]

      IT_NP : Table
      IT_NP = MkTable "account_invoice" PrimAccountInvoice.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkIT)
          origin:(idrisTpe OriginIT)
          date_due:(idrisTpe DateDueIT)
          number:(idrisTpe NumberIT)
          account_id:(idrisTpe AccountIdIT)
          partner_id:(idrisTpe PartnerIdIT)
          journal_id:(idrisTpe JournalIdIT)
          amount_tax:(idrisTpe AmountTaxIT)
          state:(idrisTpe StateIT)
          type:(idrisTpe TypeIT)
          date_invoice:(idrisTpe DateInvoiceIT)
          amount_untaxed:(idrisTpe AmountUntaxedIT)
          amount_total:(idrisTpe AmountTotalIT)
          --O2M
      %runElab derive "PrimAccountInvoice.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

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

namespace O2MResPartner
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkRPT)
          name:(idrisTpe NameRPT)
          use_parent_address:(idrisTpe UseParentAddressRPT)
          active:(idrisTpe ActiveRPT)
          street:(idrisTpe StreetRPT)
          contract:(idrisTpe ContractRPT)
          city:(idrisTpe CityRPT)
          zip:(idrisTpe ZipRPT)
          country_id:(idrisTpe CountryIdRPT)
          parent_id:(idrisTpe ParentIdRPT)
          child_ids:List O2MResPartner.RecordModel
          email:(idrisTpe EmailRPT)
          street2:(idrisTpe Street2RPT)
      %runElab derive "O2MResPartner.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MResPartner.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimResPartner.RecordModel) ->io (List  O2MResPartner.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimResPartner.MkRecordModel pk name use_parent_address active street contract city zip country_id parent_id email street2)::xs) = do
            child_ids <- O2MResPartner.read_records_c c ((ParentIdRPT==Just(cast pk)))
            let ret =(O2MResPartner.MkRecordModel pk name use_parent_address active street contract city zip country_id parent_id child_ids email street2)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MResPartner.RecordModel)
          ret_x = do
            rows <- PrimResPartner.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MResPartner.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MResPartner.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MResPartner.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MResPartner.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MResPartner.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MResPartner.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List O2MResPartner.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkRPT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List O2MResPartner.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- O2MResPartner.read_records_c_ids c xs op
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List O2MResPartner.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (O2MResPartner.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List O2MResPartner.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (O2MResPartner.main_runET_ids xs op))
          pure l1

namespace O2MM2M_OrderTax
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = True
      record RecordModel where
          constructor MkRecordModel
          order_line_id:(idrisTpe OrderLineIdM2M_ST)
          tax_id:(idrisTpe TaxIdM2M_ST)
      %runElab derive "O2MM2M_OrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MM2M_OrderTax.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimM2M_OrderTax.RecordModel) ->io (List  O2MM2M_OrderTax.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimM2M_OrderTax.MkRecordModel order_line_id tax_id)::xs) = do
            let ret =(O2MM2M_OrderTax.MkRecordModel order_line_id tax_id)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MM2M_OrderTax.RecordModel)
          ret_x = do
            rows <- PrimM2M_OrderTax.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MM2M_OrderTax.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MM2M_OrderTax.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MM2M_OrderTax.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MM2M_OrderTax.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MM2M_OrderTax.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MM2M_OrderTax.main_runET op))
          pure l1

namespace O2MOrderTax
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkOTax)
          name:(idrisTpe NameOTax)
          description:(idrisTpe DescriptionOTax)
          amount:(idrisTpe AmountOTax)
          type:(idrisTpe TypeOTax)
          price_include:(idrisTpe PriceIncludeOTax)
      %runElab derive "O2MOrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MOrderTax.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimOrderTax.RecordModel) ->io (List  O2MOrderTax.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrderTax.MkRecordModel pk name description amount type price_include)::xs) = do
            let ret =(O2MOrderTax.MkRecordModel pk name description amount type price_include)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MOrderTax.RecordModel)
          ret_x = do
            rows <- PrimOrderTax.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MOrderTax.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MOrderTax.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MOrderTax.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MOrderTax.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MOrderTax.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MOrderTax.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List O2MOrderTax.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkOTax==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List O2MOrderTax.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- O2MOrderTax.read_records_c_ids c xs op
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List O2MOrderTax.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (O2MOrderTax.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List O2MOrderTax.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (O2MOrderTax.main_runET_ids xs op))
          pure l1

namespace O2MOrderLine
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkOLT)
          price_unit:(idrisTpe PriceUnitOLT)
          product_uom_qty:(idrisTpe ProductUomQtyOLT)
          discount:(idrisTpe DiscountOLT)
          delivery_line:(idrisTpe DeliveryLineOLT)
          order_id:List PrimOrder.RecordModel
          product_id:(idrisTpe ProductIdOLT)
          tax_ids:List PrimOrderTax.RecordModel
      %runElab derive "O2MOrderLine.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MOrderLine.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimOrderLine.RecordModel) ->io (List  O2MOrderLine.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrderLine.MkRecordModel pk price_unit product_uom_qty discount delivery_line order_id product_id)::xs) = do
            let muf_m2m = ((JC PkOTax TaxIdM2M_ST)&&(OrderLineIdM2M_ST==(cast pk)))
            tax_ids_np<-getJoin c OTax_NP M2M_ST_NP (columns OTax_NP) muf_m2m
            let tax_ids=[PrimOrderTax.toRecord ox |ox <-tax_ids_np]
            let muf_m2o = ((PkOT==(cast order_id))) --&&op
            order_id <- PrimOrder.read_records_c c muf_m2o
            let ret =(O2MOrderLine.MkRecordModel pk price_unit product_uom_qty discount delivery_line order_id product_id tax_ids)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MOrderLine.RecordModel)
          ret_x = do
            rows <- PrimOrderLine.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MOrderLine.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MOrderLine.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MOrderLine.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MOrderLine.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MOrderLine.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MOrderLine.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List O2MOrderLine.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkOLT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List O2MOrderLine.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- O2MOrderLine.read_records_c_ids c xs op
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List O2MOrderLine.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (O2MOrderLine.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List O2MOrderLine.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (O2MOrderLine.main_runET_ids xs op))
          pure l1

namespace O2MOrder
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkOT)
          origin:(idrisTpe OriginOT)
          order_policy:(idrisTpe OrderPolicyOT)
          date_order:(idrisTpe DateOrderOT)
          partner_id:(idrisTpe PartnerIdOT)
          amount_tax:(idrisTpe AmountTaxOT)
          state:(idrisTpe StateOT)
          partner_invoice_id:(idrisTpe PartnerInvoiceIdOT)
          amount_untaxed:(idrisTpe AmountUntaxedOT)
          amount_total:(idrisTpe AmountTotalOT)
          name:(idrisTpe NameOT)
          partner_shipping_id:(idrisTpe PartnerShippingIdOT)
          picking_policy:(idrisTpe PickingPolicyOT)
          carrier_id:(idrisTpe CarrierIdOT)
          order_line:List O2MOrderLine.RecordModel
          requested_date:(idrisTpe RequestedDateOT)
      %runElab derive "O2MOrder.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MOrder.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimOrder.RecordModel) ->io (List  O2MOrder.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id requested_date)::xs) = do
            order_line <- O2MOrderLine.read_records_c c ((OrderIdOLT==(cast pk)))
            let ret =(O2MOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id order_line requested_date)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MOrder.RecordModel)
          ret_x = do
            rows <- PrimOrder.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MOrder.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MOrder.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MOrder.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MOrder.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MOrder.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MOrder.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List O2MOrder.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkOT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List O2MOrder.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- O2MOrder.read_records_c_ids c xs op
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List O2MOrder.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (O2MOrder.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List O2MOrder.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (O2MOrder.main_runET_ids xs op))
          pure l1

namespace O2MAccountVoucher
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkACVT)
          number:(idrisTpe NumberACVT)
          partner_id:(idrisTpe PartnerIdACVT)
          journal_id:(idrisTpe JournalIdACVT)
          amount:(idrisTpe AmountACVT)
      %runElab derive "O2MAccountVoucher.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MAccountVoucher.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimAccountVoucher.RecordModel) ->io (List  O2MAccountVoucher.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimAccountVoucher.MkRecordModel pk number partner_id journal_id amount)::xs) = do
            let ret =(O2MAccountVoucher.MkRecordModel pk number partner_id journal_id amount)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MAccountVoucher.RecordModel)
          ret_x = do
            rows <- PrimAccountVoucher.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MAccountVoucher.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MAccountVoucher.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MAccountVoucher.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MAccountVoucher.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MAccountVoucher.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MAccountVoucher.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List O2MAccountVoucher.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkACVT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List O2MAccountVoucher.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- O2MAccountVoucher.read_records_c_ids c xs op
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List O2MAccountVoucher.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (O2MAccountVoucher.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List O2MAccountVoucher.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (O2MAccountVoucher.main_runET_ids xs op))
          pure l1

namespace O2MStockMove
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkSMT)
          origin:(idrisTpe OriginSMT)
          price_unit:(idrisTpe PriceUnitSMT)
          product_qty:(idrisTpe ProductQtySMT)
          product_id:(idrisTpe ProductIdSMT)
          location_id:(idrisTpe LocationIdSMT)
          location_dest_id:(idrisTpe LocationDestIdSMT)
          picking_id:(idrisTpe PickingIdSMT)
          state:(idrisTpe StateSMT)
      %runElab derive "O2MStockMove.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MStockMove.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimStockMove.RecordModel) ->io (List  O2MStockMove.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimStockMove.MkRecordModel pk origin price_unit product_qty product_id location_id location_dest_id picking_id state)::xs) = do
            let ret =(O2MStockMove.MkRecordModel pk origin price_unit product_qty product_id location_id location_dest_id picking_id state)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MStockMove.RecordModel)
          ret_x = do
            rows <- PrimStockMove.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MStockMove.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MStockMove.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MStockMove.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MStockMove.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MStockMove.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MStockMove.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List O2MStockMove.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkSMT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List O2MStockMove.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- O2MStockMove.read_records_c_ids c xs op
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List O2MStockMove.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (O2MStockMove.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List O2MStockMove.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (O2MStockMove.main_runET_ids xs op))
          pure l1

namespace O2MStockPicking
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkSPT)
          origin:(idrisTpe OriginSPT)
          backorder_id:(idrisTpe BackorderIdSPT)
          date_done:(idrisTpe DateDoneSPT)
          partner_id:(idrisTpe PartnerIdSPT)
          min_date:(idrisTpe MinDateSPT)
          name:(idrisTpe NameSPT)
          state:(idrisTpe StateSPT)
          move_ids:List O2MStockMove.RecordModel
      %runElab derive "O2MStockPicking.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MStockPicking.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimStockPicking.RecordModel) ->io (List  O2MStockPicking.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimStockPicking.MkRecordModel pk origin backorder_id date_done partner_id min_date name state)::xs) = do
            move_ids <- O2MStockMove.read_records_c c ((PickingIdSMT==Just(cast pk)))
            let ret =(O2MStockPicking.MkRecordModel pk origin backorder_id date_done partner_id min_date name state move_ids)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MStockPicking.RecordModel)
          ret_x = do
            rows <- PrimStockPicking.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MStockPicking.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MStockPicking.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MStockPicking.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MStockPicking.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MStockPicking.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MStockPicking.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List O2MStockPicking.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkSPT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List O2MStockPicking.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- O2MStockPicking.read_records_c_ids c xs op
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List O2MStockPicking.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (O2MStockPicking.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List O2MStockPicking.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (O2MStockPicking.main_runET_ids xs op))
          pure l1

namespace O2MM2M_InvoiceTax
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = True
      record RecordModel where
          constructor MkRecordModel
          invoice_line_id:(idrisTpe InvoiceLineIdM2M_IT)
          tax_id:(idrisTpe TaxIdM2M_IT)
      %runElab derive "O2MM2M_InvoiceTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MM2M_InvoiceTax.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimM2M_InvoiceTax.RecordModel) ->io (List  O2MM2M_InvoiceTax.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimM2M_InvoiceTax.MkRecordModel invoice_line_id tax_id)::xs) = do
            let ret =(O2MM2M_InvoiceTax.MkRecordModel invoice_line_id tax_id)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MM2M_InvoiceTax.RecordModel)
          ret_x = do
            rows <- PrimM2M_InvoiceTax.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MM2M_InvoiceTax.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MM2M_InvoiceTax.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MM2M_InvoiceTax.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MM2M_InvoiceTax.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MM2M_InvoiceTax.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MM2M_InvoiceTax.main_runET op))
          pure l1

namespace O2MAccountInvoiceLine
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkILT)
          invoice_id:List PrimAccountInvoice.RecordModel
          price_unit:(idrisTpe PriceUnitILT)
          quantity:(idrisTpe QuantityILT)
          name:(idrisTpe NameILT)
          product_id:(idrisTpe ProductIdILT)
          tax_ids:List PrimOrderTax.RecordModel
          discount:(idrisTpe DiscountILT)
      %runElab derive "O2MAccountInvoiceLine.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MAccountInvoiceLine.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimAccountInvoiceLine.RecordModel) ->io (List  O2MAccountInvoiceLine.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimAccountInvoiceLine.MkRecordModel pk invoice_id price_unit quantity name product_id discount)::xs) = do
            let muf_m2m = ((JC PkOTax TaxIdM2M_IT)&&(InvoiceLineIdM2M_IT==(cast pk)))
            tax_ids_np<-getJoin c OTax_NP M2M_IT_NP (columns OTax_NP) muf_m2m
            let tax_ids=[PrimOrderTax.toRecord ox |ox <-tax_ids_np]
            let muf_m2o = ((PkIT==(cast invoice_id))) --&&op
            invoice_id <- PrimAccountInvoice.read_records_c c muf_m2o
            let ret =(O2MAccountInvoiceLine.MkRecordModel pk invoice_id price_unit quantity name product_id tax_ids discount)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MAccountInvoiceLine.RecordModel)
          ret_x = do
            rows <- PrimAccountInvoiceLine.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MAccountInvoiceLine.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MAccountInvoiceLine.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MAccountInvoiceLine.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MAccountInvoiceLine.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MAccountInvoiceLine.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MAccountInvoiceLine.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List O2MAccountInvoiceLine.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkILT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List O2MAccountInvoiceLine.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- O2MAccountInvoiceLine.read_records_c_ids c xs op
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List O2MAccountInvoiceLine.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (O2MAccountInvoiceLine.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List O2MAccountInvoiceLine.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (O2MAccountInvoiceLine.main_runET_ids xs op))
          pure l1

namespace O2MAccountInvoice
      domain : Op
      domain = (True)
      isM2M_tab : Bool
      isM2M_tab = False
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PkIT)
          origin:(idrisTpe OriginIT)
          date_due:(idrisTpe DateDueIT)
          number:(idrisTpe NumberIT)
          account_id:(idrisTpe AccountIdIT)
          partner_id:(idrisTpe PartnerIdIT)
          journal_id:(idrisTpe JournalIdIT)
          amount_tax:(idrisTpe AmountTaxIT)
          state:(idrisTpe StateIT)
          type:(idrisTpe TypeIT)
          date_invoice:(idrisTpe DateInvoiceIT)
          amount_untaxed:(idrisTpe AmountUntaxedIT)
          amount_total:(idrisTpe AmountTotalIT)
          invoice_line:List O2MAccountInvoiceLine.RecordModel
      %runElab derive "O2MAccountInvoice.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MAccountInvoice.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimAccountInvoice.RecordModel) ->io (List  O2MAccountInvoice.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimAccountInvoice.MkRecordModel pk origin date_due number account_id partner_id journal_id amount_tax state type date_invoice amount_untaxed amount_total)::xs) = do
            invoice_line <- O2MAccountInvoiceLine.read_records_c c ((InvoiceIdILT==(cast pk)))
            let ret =(O2MAccountInvoice.MkRecordModel pk origin date_due number account_id partner_id journal_id amount_tax state type date_invoice amount_untaxed amount_total invoice_line)
            ret_xs <- add_lines xs
            pure ([ret]++ret_xs)

          ret_x : io (List O2MAccountInvoice.RecordModel)
          ret_x = do
            rows <- PrimAccountInvoice.read_records_c c op
            ret1 <- add_lines rows
            pure ret1
      export
      read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List O2MAccountInvoice.RecordModel )
      read_records op = do
          c <- connect DB_URI
          ret <- O2MAccountInvoice.read_records_c c op
          pure ret

      export
      main_runET : (op:Op) -> IO (List O2MAccountInvoice.RecordModel )
      main_runET op = do 
          Left err <- runEitherT (O2MAccountInvoice.read_records op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read : HasIO io => (op:Op) -> io (List O2MAccountInvoice.RecordModel )
      read op = do
          l1 <- (liftIO $ (O2MAccountInvoice.main_runET op))
          pure l1
      export
      read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List O2MAccountInvoice.RecordModel )
      read_records_c_ids c [] op  = pure []
      read_records_c_ids c (x::xs) op = do
          r <- read_records_c c (( PkIT==(cast x))&&op) 
          r_xs <- read_records_c_ids c xs op
          pure (r++r_xs)
      export
      read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List O2MAccountInvoice.RecordModel )
      read_records_ids xs op = do
          c <- connect DB_URI
          ret <- O2MAccountInvoice.read_records_c_ids c xs op
          pure ret

      export
      main_runET_ids : List Bits32 -> (op:Op) -> IO (List O2MAccountInvoice.RecordModel )
      main_runET_ids xs op = do 
          Left err <- runEitherT (O2MAccountInvoice.read_records_ids xs op {io = EitherT SQLError IO} )
            | Right l1 => pure l1
          printLn err
          pure []

      export
      read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List O2MAccountInvoice.RecordModel )
      read_ids xs op = do
          l1 <- (liftIO $ (O2MAccountInvoice.main_runET_ids xs op))
          pure l1