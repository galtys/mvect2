module Ledger.PG.Order


import Data.SortedMap

import Category.Transaction.Qty
--import Category.Transaction.Types
--import Category.Transaction.Hom
--import Category.Transaction.Journal
--import Category.Transaction.Demo
import Category.Transaction.Types
import Data.Ratio
--import Data.Zippable
import JSON

import Generics.Derive

import Control.Monad.Either

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

%language ElabReflection


public export
record Line where
  constructor MkLine
  sku : ProdKey
  qty : TQty
  --Unit of Measure
  --company pricelist is input  , "price_unit" modifies it, as a multiple
  currency : ProdKey   
  price_unit : TQty --together with discount,turn it into a function Qty->Qty
  discount : TQty   --idea, in amendments, fix price_unit and let the user change the discount   
  tax_code : TaxCode
  --reference to List Price  
                      --SubTotal ... calculated

%runElab derive "Line" [Generic, Meta, Show, Eq,RecordToJSON,RecordFromJSON]


public export
record LineExt where
  constructor MkLineExt
  sku : ProdKey
  qty : TQty
  currency : ProdKey   
  price_unit : TQty --together with discount,turn it into a function Qty->Qty
  discount : TQty   --idea, in amendments, fix price_unit and let the user change the discount   
  tax_code : List TaxCode

%runElab derive "LineExt" [Generic, Meta, Show, Eq]
--%runElab derive "LineExt" [Generic, Meta, Show, Eq,RecordToJSON,RecordFromJSON]

-- Order and Order Line (odoo)

OT : String
OT = "sale_order"

OLT : String
OLT = "sale_order_line"

-- Order
Id_OT : Column
Id_OT = primarySerial64 Bits32 "id" (Just . cast) OT
Origin: Column
Origin = nullable String "origin" (VarChar 64) (Just . cast) cast OT
OrderPolicy : Column
OrderPolicy = notNull String "order_policy" (VarChar 64) (Just . cast) cast OT
ShopID : Column
ShopID = notNull Bits32 "shop_id" BigInt (Just . cast) cast OT
ClientOrderRef: Column
ClientOrderRef = nullable String "client_order_ref" (VarChar 64) (Just . cast) cast OT
DateOrder : Column
DateOrder = notNull Date "date_order" (VarChar 10) (Just . cast) cast OT
PartnerID : Column
PartnerID = notNull Bits32 "partner_id" BigInt (Just . cast) cast OT
Note: Column
Note = nullable String "note" Text (Just . cast) cast OT
FiscalPosition : Column
FiscalPosition = nullable Bits32 "fiscal_position" BigInt (Just . cast) cast OT
UserID : Column
UserID = nullable Bits32 "user_id" BigInt (Just . cast) cast OT
AmountTax : Column
AmountTax = notNull Price "amount_tax" DoublePrecision (Just . toTaxA) cast OT
StateOT : Column
StateOT = notNull String "state" Text (Just . cast) cast OT
PricelistID : Column
PricelistID = notNull Bits32 "pricelist_id" BigInt (Just . cast) cast OT
PartnerInvoiceID : Column
PartnerInvoiceID = notNull Bits32 "partner_invoice_id" BigInt (Just . cast) cast OT
AmountUntaxed : Column
AmountUntaxed = notNull Price "amount_untaxed" DoublePrecision (Just . toEX20) cast OT
DateConfirm : Column
DateConfirm = nullable Date "date_confirm" (VarChar 10) (Just . cast) cast OT
AmountTotal : Column
AmountTotal = notNull Price "amount_total" DoublePrecision (Just . toINC20) cast OT
NameOT: Column
NameOT = notNull String "name" (VarChar 64) (Just . cast) cast OT
PartnerShippingID : Column
PartnerShippingID = notNull Bits32 "partner_shipping_id" BigInt (Just . cast) cast OT
PickingPolicy : Column
PickingPolicy = notNull String "picking_policy" Text (Just . cast) cast OT
CarrierID : Column
CarrierID = nullable Bits32 "carrier_id" BigInt (Just . cast) cast OT
EffectiveDate : Column
EffectiveDate = nullable Date "effective_date" (VarChar 10) (Just . cast) cast OT
RequestedDate : Column
RequestedDate = nullable Date "requested_date" (VarChar 10) (Just . cast) cast OT
CommitmentdDate : Column
CommitmentdDate = nullable Date "commitmentd_date" (VarChar 10) (Just . cast) cast OT
DeliveryNotes: Column
DeliveryNotes = nullable String "delivery_notes" Text (Just . cast) cast OT

ListSaleOrderCols : List Column
ListSaleOrderCols = [Id_OT,Origin,OrderPolicy,DateOrder,PartnerID,AmountTax,StateOT,PartnerInvoiceID,AmountUntaxed,AmountTotal, NameOT,PartnerShippingID,PickingPolicy,CarrierID,RequestedDate]

SO_NP : Table
SO_NP = MkTable "sale_order"
        ListSaleOrderCols

record RSaleOrder where
  constructor MkRSO
  pk : Bits32
  origin : Maybe String
  OrderPolicy : String
  date_order : Date
  partner_id : Bits32
  amount_tax : Price --
  state : String
  partner_invoice_id : Bits32
  amount_untaxed : Price
  amount_total : Price
  name : String
  partner_shipping_id : Bits32
  picking_policy : String
  carrier_id : Maybe Bits32
  requested_date : Maybe String

%runElab derive "RSaleOrder" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
 
toRSO : GetRow ListSaleOrderCols -> RSaleOrder
toRSO =  to . (\x => MkSOP $ Z x)

-- IO

read_sale_orders : HasIO io => MonadError SQLError io => Connection -> io (List RSaleOrder) 
read_sale_orders c  = do
  rows <- get c SO_NP (columns SO_NP) (StateOT /= "cancel")
  let so_s= [ toRSO ox | ox <- rows ]
  pure so_s

main_read_so : HasIO io => MonadError SQLError io => io (List RSaleOrder )
main_read_so  = do
  c    <- connect "postgresql://jan@localhost:5432/pjb-2021-10-27_1238"  
  x <- read_sale_orders c
  finish c
  pure x
  
export  
main_soIO : IO (List RSaleOrder)

main_soIO = do Left err <- runEitherT (main_read_so {io = EitherT SQLError IO} )
                 | Right l1 => pure l1
               printLn err
               pure []

export
main_so : HasIO io => io (List RSaleOrder )
main_so = do  
     l1 <- (liftIO main_soIO)
     pure l1

