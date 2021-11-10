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
OTax:String
OTax = "account_tax"
OLT:String
OLT = "sale_order_line"
OT:String
OT = "sale_order"

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
ORDER_ID_OLT:Column
ORDER_ID_OLT=notNull Bits32 "order_id" (BigInt) (Just . cast) cast OLT
PRODUCT_ID_OLT:Column
PRODUCT_ID_OLT=nullable Bits32 "product_id" (BigInt) (Just . cast) cast OLT
--M2M

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
--O2M
REQUESTED_DATE_OT:Column
REQUESTED_DATE_OT=nullable Date "requested_date" (VarChar 10) (Just . cast) cast OT
--O2M

namespace PrimOrderTax
      domain : Op
      domain = (True)
      PrimCols : List Column
      PrimCols = [PK_OTax, NAME_OTax, DESCRIPTION_OTax, AMOUNT_OTax, TYPE_OTax, PRICE_INCLUDE_OTax]

      OTax_NP : Table
      OTax_NP = MkTable "account_tax" PrimOrderTax.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PK_OTax)
          name:(idrisTpe NAME_OTax)
          description:(idrisTpe DESCRIPTION_OTax)
          amount:(idrisTpe AMOUNT_OTax)
          type:(idrisTpe TYPE_OTax)
          price_include:(idrisTpe PRICE_INCLUDE_OTax)
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
      PrimCols = [PK_OLT, PRICE_UNIT_OLT, PRODUCT_UOM_QTY_OLT, DISCOUNT_OLT, DELIVERY_LINE_OLT, ORDER_ID_OLT, PRODUCT_ID_OLT]

      OLT_NP : Table
      OLT_NP = MkTable "sale_order_line" PrimOrderLine.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PK_OLT)
          price_unit:(idrisTpe PRICE_UNIT_OLT)
          product_uom_qty:(idrisTpe PRODUCT_UOM_QTY_OLT)
          discount:(idrisTpe DISCOUNT_OLT)
          delivery_line:(idrisTpe DELIVERY_LINE_OLT)
          order_id:(idrisTpe ORDER_ID_OLT)
          product_id:(idrisTpe PRODUCT_ID_OLT)
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
      PrimCols = [PK_OT, ORIGIN_OT, ORDER_POLICY_OT, DATE_ORDER_OT, PARTNER_ID_OT, AMOUNT_TAX_OT, STATE_OT, PARTNER_INVOICE_ID_OT, AMOUNT_UNTAXED_OT, AMOUNT_TOTAL_OT, NAME_OT, PARTNER_SHIPPING_ID_OT, PICKING_POLICY_OT, CARRIER_ID_OT, REQUESTED_DATE_OT]

      OT_NP : Table
      OT_NP = MkTable "sale_order" PrimOrder.PrimCols

      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PK_OT)
          origin:(idrisTpe ORIGIN_OT)
          order_policy:(idrisTpe ORDER_POLICY_OT)
          date_order:(idrisTpe DATE_ORDER_OT)
          partner_id:(idrisTpe PARTNER_ID_OT)
          amount_tax:(idrisTpe AMOUNT_TAX_OT)
          state:(idrisTpe STATE_OT)
          partner_invoice_id:(idrisTpe PARTNER_INVOICE_ID_OT)
          amount_untaxed:(idrisTpe AMOUNT_UNTAXED_OT)
          amount_total:(idrisTpe AMOUNT_TOTAL_OT)
          name:(idrisTpe NAME_OT)
          partner_shipping_id:(idrisTpe PARTNER_SHIPPING_ID_OT)
          picking_policy:(idrisTpe PICKING_POLICY_OT)
          carrier_id:(idrisTpe CARRIER_ID_OT)
          --O2M
          requested_date:(idrisTpe REQUESTED_DATE_OT)
          --O2M
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

namespace O2MOrderTax
      domain : Op
      domain = (True)
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PK_OTax)
          name:(idrisTpe NAME_OTax)
          description:(idrisTpe DESCRIPTION_OTax)
          amount:(idrisTpe AMOUNT_OTax)
          type:(idrisTpe TYPE_OTax)
          price_include:(idrisTpe PRICE_INCLUDE_OTax)
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

namespace O2MOrderLine
      domain : Op
      domain = (True)
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PK_OLT)
          price_unit:(idrisTpe PRICE_UNIT_OLT)
          product_uom_qty:(idrisTpe PRODUCT_UOM_QTY_OLT)
          discount:(idrisTpe DISCOUNT_OLT)
          delivery_line:(idrisTpe DELIVERY_LINE_OLT)
          order_id:List PrimOrder.RecordModel
          product_id:(idrisTpe PRODUCT_ID_OLT)
          tax_ids:List PrimOrderTax.RecordModel
      %runElab derive "O2MOrderLine.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MOrderLine.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimOrderLine.RecordModel) ->io (List  O2MOrderLine.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrderLine.MkRecordModel pk price_unit product_uom_qty discount delivery_line order_id product_id)::xs) = do
            let tax_ids=[]
            order_id <- PrimOrder.read_records_c c ((PK_OT==(cast order_id))&&op)
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

namespace O2MOrder
      domain : Op
      domain = (True)
      record RecordModel where
          constructor MkRecordModel
          pk:(idrisTpe PK_OT)
          origin:(idrisTpe ORIGIN_OT)
          order_policy:(idrisTpe ORDER_POLICY_OT)
          date_order:(idrisTpe DATE_ORDER_OT)
          partner_id:(idrisTpe PARTNER_ID_OT)
          amount_tax:(idrisTpe AMOUNT_TAX_OT)
          state:(idrisTpe STATE_OT)
          partner_invoice_id:(idrisTpe PARTNER_INVOICE_ID_OT)
          amount_untaxed:(idrisTpe AMOUNT_UNTAXED_OT)
          amount_total:(idrisTpe AMOUNT_TOTAL_OT)
          name:(idrisTpe NAME_OT)
          partner_shipping_id:(idrisTpe PARTNER_SHIPPING_ID_OT)
          picking_policy:(idrisTpe PICKING_POLICY_OT)
          carrier_id:(idrisTpe CARRIER_ID_OT)
          order_line:List PrimOrderLine.RecordModel
          requested_date:(idrisTpe REQUESTED_DATE_OT)
          order_line2:List PrimOrderLine.RecordModel
      %runElab derive "O2MOrder.RecordModel" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
      export
      read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List O2MOrder.RecordModel )
      read_records_c c op = ret_x where

          add_lines : (List PrimOrder.RecordModel) ->io (List  O2MOrder.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id requested_date)::xs) = do
            order_line <- PrimOrderLine.read_records_c c ((ORDER_ID_OLT==(cast pk))&&op)
            order_line2 <- PrimOrderLine.read_records_c c ((ORDER_ID_OLT==(cast pk))&&op)
            let ret =(O2MOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id order_line requested_date order_line2)
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
