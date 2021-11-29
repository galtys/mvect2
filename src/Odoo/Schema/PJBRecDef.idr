module Odoo.Schema.PJBRecDef
import Category.Transaction.Types
import Data.Ratio

import Generics.Derive

import JSON

import Odoo.PG.Config
import Control.Monad.Either

%language ElabReflection

namespace PrimResPartner

      public export
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

namespace PrimM2M_OrderTax

      public export
      record RecordModel where
          constructor MkRecordModel
          order_line_id:Bits32
          tax_id:Bits32
      %runElab derive "PrimM2M_OrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace PrimOrderTax

      public export
      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          name:String
          description:(Maybe String)
          amount:EQty
          type:(Maybe String)
          price_include:(Maybe Bool)
      %runElab derive "PrimOrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace PrimOrderLine

      public export
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

namespace PrimOrder

      public export
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

namespace PrimAccountVoucher

      public export
      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          number:String
          partner_id:(Maybe Bits32)
          journal_id:(Maybe Bits32)
          amount:EQty
      %runElab derive "PrimAccountVoucher.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace PrimStockMove

      public export
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

namespace PrimStockPicking

      public export
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

namespace PrimM2M_InvoiceTax

      public export
      record RecordModel where
          constructor MkRecordModel
          invoice_line_id:Bits32
          tax_id:Bits32
      %runElab derive "PrimM2M_InvoiceTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace PrimAccountInvoiceLine

      public export
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

namespace PrimAccountInvoice

      public export
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

namespace RelResPartner
      public export
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
          child_ids:List RelResPartner.RecordModel
          email:String
          street2:(Maybe String)
      %runElab derive "RelResPartner.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelM2M_OrderTax
      public export
      record RecordModel where
          constructor MkRecordModel
          order_line_id:Bits32
          tax_id:Bits32
      %runElab derive "RelM2M_OrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelOrderTax
      public export
      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          name:String
          description:(Maybe String)
          amount:EQty
          type:(Maybe String)
          price_include:(Maybe Bool)
      %runElab derive "RelOrderTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelOrderLine
      public export
      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          price_unit:EQty
          product_uom_qty:EQty
          discount:(Maybe EQty)
          delivery_line:(Maybe Bool)
          order_id:List PrimOrder.RecordModel
          product_id:(Maybe Bits32)
          tax_ids:List PrimOrderTax.RecordModel
      %runElab derive "RelOrderLine.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelOrder
      public export
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
          order_line:List RelOrderLine.RecordModel
          requested_date:(Maybe Date)
      %runElab derive "RelOrder.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelAccountVoucher
      public export
      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          number:String
          partner_id:(Maybe Bits32)
          journal_id:(Maybe Bits32)
          amount:EQty
      %runElab derive "RelAccountVoucher.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelStockMove
      public export
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
      %runElab derive "RelStockMove.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelStockPicking
      public export
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
          move_ids:List RelStockMove.RecordModel
      %runElab derive "RelStockPicking.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelM2M_InvoiceTax
      public export
      record RecordModel where
          constructor MkRecordModel
          invoice_line_id:Bits32
          tax_id:Bits32
      %runElab derive "RelM2M_InvoiceTax.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelAccountInvoiceLine
      public export
      record RecordModel where
          constructor MkRecordModel
          pk:Bits32
          invoice_id:List PrimAccountInvoice.RecordModel
          price_unit:EQty
          quantity:EQty
          name:String
          product_id:(Maybe Bits32)
          tax_ids:List PrimOrderTax.RecordModel
          discount:(Maybe EQty)
      %runElab derive "RelAccountInvoiceLine.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

namespace RelAccountInvoice
      public export
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
          invoice_line:List RelAccountInvoiceLine.RecordModel
      %runElab derive "RelAccountInvoice.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
