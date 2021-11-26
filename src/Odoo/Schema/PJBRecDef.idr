module Odoo.Schema.PJB
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

namespace PrimResPartner

      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          name:String
          use_parent_address:(Maybe Bool)
          active:(Maybe Bool)
          street:(Maybe String)
          contract:(Maybe Bool)
          city:(Maybe String)
          zip:(Maybe String)
          country_id:(Maybe Bits32)
          parent_id:(Maybe Bits32)
          --O2M
          email:String
          street2:(Maybe String)
      %runElab derive "PrimResPartner.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimResPartner.PrimCols -> PrimResPartner.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimM2M_OrderTax

      record RecordModel where
          constructor MkRecordModel
          order_line_id:Bits32
          tax_id:Bits32
      %runElab derive "PrimM2M_OrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimM2M_OrderTax.PrimCols -> PrimM2M_OrderTax.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimOrderTax

      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          name:String
          description:(Maybe String)
          amount:EQty
          type:(Maybe String)
          price_include:(Maybe Bool)
      %runElab derive "PrimOrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimOrderTax.PrimCols -> PrimOrderTax.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimOrderLine

      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          price_unit:EQty
          product_uom_qty:EQty
          discount:(Maybe EQty)
          delivery_line:(Maybe Bool)
          order_id:Bits32
          product_id:(Maybe Bits32)
          --M2M
      %runElab derive "PrimOrderLine.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimOrderLine.PrimCols -> PrimOrderLine.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimOrder

      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          origin:(Maybe String)
          order_policy:String
          date_order:Date
          partner_id:Bits32
          amount_tax:Price
          state:String
          partner_invoice_id:Bits32
          amount_untaxed:Price
          amount_total:Price
          name:String
          partner_shipping_id:Bits32
          picking_policy:String
          carrier_id:(Maybe Bits32)
          --O2M
          requested_date:(Maybe Date)
      %runElab derive "PrimOrder.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimOrder.PrimCols -> PrimOrder.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimAccountVoucher

      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          number:String
          partner_id:(Maybe Bits32)
          journal_id:(Maybe Bits32)
          amount:EQty
      %runElab derive "PrimAccountVoucher.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimAccountVoucher.PrimCols -> PrimAccountVoucher.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimStockMove

      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          origin:(Maybe String)
          price_unit:EQty
          product_qty:EQty
          product_id:(Maybe Bits32)
          location_id:(Maybe Bits32)
          location_dest_id:(Maybe Bits32)
          picking_id:(Maybe Bits32)
          state:String
      %runElab derive "PrimStockMove.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimStockMove.PrimCols -> PrimStockMove.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimStockPicking

      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          origin:(Maybe String)
          backorder_id:(Maybe Bits32)
          date_done:Date
          partner_id:(Maybe Bits32)
          min_date:Date
          name:String
          state:String
          --O2M
      %runElab derive "PrimStockPicking.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimStockPicking.PrimCols -> PrimStockPicking.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimM2M_InvoiceTax

      record RecordModel where
          constructor MkRecordModel
          invoice_line_id:Bits32
          tax_id:Bits32
      %runElab derive "PrimM2M_InvoiceTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimM2M_InvoiceTax.PrimCols -> PrimM2M_InvoiceTax.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimAccountInvoiceLine

      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          invoice_id:Bits32
          price_unit:EQty
          quantity:EQty
          name:String
          product_id:(Maybe Bits32)
          --M2M
          discount:(Maybe EQty)
      %runElab derive "PrimAccountInvoiceLine.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimAccountInvoiceLine.PrimCols -> PrimAccountInvoiceLine.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)

namespace PrimAccountInvoice

      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          origin:(Maybe String)
          date_due:Date
          number:String
          account_id:(Maybe Bits32)
          partner_id:(Maybe Bits32)
          journal_id:(Maybe Bits32)
          amount_tax:Price
          state:String
          type:(Maybe String)
          date_invoice:Date
          amount_untaxed:Price
          amount_total:Price
          --O2M
      %runElab derive "PrimAccountInvoice.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

      toRecord : GetRow PrimAccountInvoice.PrimCols -> PrimAccountInvoice.RecordModel
      toRecord = to . (\x => MkSOP $ Z x)
