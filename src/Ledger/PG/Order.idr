module Ledger.PG.Order

import Ledger.PG.Types


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

import Ledger.PG.Config

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

PrimListSaleOrderCols : List Column
PrimListSaleOrderCols = [Id_OT,Origin,OrderPolicy,DateOrder,PartnerID,AmountTax,StateOT,PartnerInvoiceID,AmountUntaxed,AmountTotal, NameOT,PartnerShippingID,PickingPolicy,CarrierID,RequestedDate]


----- SO Line
Id_OLT : Column
Id_OLT = primarySerial64 Bits32 "id" (Just . cast) OLT
PrimOrderID : Column
PrimOrderID = notNull Bits32 "order_id" BigInt (Just . cast) cast OLT
PriceUnit : Column
PriceUnit = notNull TQty "price_unit" DoublePrecision (Just . cast) cast OLT
ProductUomQty : Column
ProductUomQty = notNull TQty "product_uom_qty" DoublePrecision (Just . cast) cast OLT
Discount : Column
Discount = nullable TQty "discount" DoublePrecision (Just . cast) cast OLT
ProductID : Column
ProductID = nullable Bits32 "product_id" BigInt (Just . cast) cast OLT
DeliveryLine : Column
DeliveryLine = nullable Bool "delivery_line" Boolean (Just . cast) cast OLT


PrimListSaleOrderLineCols : List Column
PrimListSaleOrderLineCols = [Id_OLT,PriceUnit,ProductUomQty,Discount,DeliveryLine]

SO_NP : Table
SO_NP = MkTable "sale_order"
        PrimListSaleOrderCols
SOL_NP : Table
SOL_NP = MkTable "sale_order_line"
        PrimListSaleOrderLineCols

mutual  
  SaleOrder : Model
  SaleOrder = MkM SO_NP (Id_OT) ((toFields (columns SO_NP))++[OrderLines] ) 
  
  OrderID : Field
  OrderID = M2O SaleOrder PrimOrderID
 
  SaleOrderLine : Model
  SaleOrderLine = MkM SOL_NP (Id_OLT) ((toFields (columns SOL_NP))++[OrderID])

  OrderLines : Field
  OrderLines = O2M SaleOrderLine


namespace SO_Simple
  model : Model
  model = SaleOrder
  domain : Op
  domain = (StateOT /= "cancel")
  PrimCols : List Column
  PrimCols = PrimListSaleOrderCols
  
  record RecordCols where
    constructor MkRSO
    pk : (idrisTpe Id_OT)
    origin : (idrisTpe Origin)
    order_policy : (idrisTpe OrderPolicy)
    date_order : (idrisTpe DateOrder)
    partner_id : (idrisTpe PartnerID)
    amount_tax : (idrisTpe AmountTax)
    state : (idrisTpe StateOT)
    partner_invoice_id : (idrisTpe PartnerInvoiceID)
    amount_untaxed : (idrisTpe AmountUntaxed)
    amount_total : (idrisTpe AmountTotal)
    name : (idrisTpe NameOT)
    partner_shipping_id : (idrisTpe PartnerShippingID)
    picking_policy : (idrisTpe PickingPolicy)
    carrier_id : (idrisTpe CarrierID)
    requested_date : (idrisTpe RequestedDate)

  -----------------
  ---- Can copy ---
  -----------------
  %runElab derive "RecordCols" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
   
  toRecord : GetRow PrimCols -> RecordCols
  toRecord =  to . (\x => MkSOP $ Z x)
          
  read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List RecordCols) 
  read_records op= do
    c <- connect DB_URI
    rows <- get c (table model) (columns (table model)) (domain&&op)
    let so_s= [ toRecord ox | ox <- rows ]
    finish c    
    pure so_s
  
  main_runET : (op:Op) -> IO (List RecordCols)
  main_runET op = do Left err <- runEitherT (read_records op {io = EitherT SQLError IO} )
                       | Right l1 => pure l1
                     printLn err
                     pure []

  export
  read : HasIO io => (op:Op) -> io (List RecordCols)
  read op = do  
     l1 <- (liftIO $ main_runET op)
     pure l1

namespace SOL_Simple
  model : Model
  model = SaleOrderLine
  domain : Op
  domain = (True)
  PrimCols : List Column
  PrimCols = PrimListSaleOrderLineCols
  
  record RecordCols where
    constructor MkRSOL
    pk : (idrisTpe Id_OLT)
    price_unit : (idrisTpe PriceUnit)
    product_uom_qty : (idrisTpe ProductUomQty)
    discount : (idrisTpe Discount)
    delivery_line : (idrisTpe DeliveryLine)
    
  %runElab derive "SOL_Simple.RecordCols" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
   
  toRecord : GetRow SOL_Simple.PrimCols -> SOL_Simple.RecordCols
  toRecord =  to . (\x => MkSOP $ Z x)
          
  read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List SOL_Simple.RecordCols) 
  read_records op= do
    c <- connect DB_URI
    rows <- get c (table SOL_Simple.model) (columns (table SOL_Simple.model)) (SOL_Simple.domain&&op)
    let so_s= [ toRecord ox | ox <- rows ]
    finish c    
    pure so_s
      
  main_runET : (op:Op) -> IO (List SOL_Simple.RecordCols)
  main_runET op = do Left err <- runEitherT (SOL_Simple.read_records op {io = EitherT SQLError IO} )
                       | Right l1 => pure l1
                     printLn err
                     pure []

  export
  read : HasIO io => (op:Op) -> io (List SOL_Simple.RecordCols)
  read op = do  
     l1 <- (liftIO $ main_runET op)
     pure l1

namespace SO_O2M
  model : Model
  model = SaleOrder
  domain : Op
  domain = (StateOT /= "cancel")
  --PrimCols : List Column
  --PrimCols = PrimListSaleOrderCols
  
  record RecordCols where
    constructor MkRSO
    pk : (idrisTpe Id_OT)
    origin : (idrisTpe Origin)
    order_policy : (idrisTpe OrderPolicy)
    date_order : (idrisTpe DateOrder)
    partner_id : (idrisTpe PartnerID)
    amount_tax : (idrisTpe AmountTax)
    state : (idrisTpe StateOT)
    partner_invoice_id : (idrisTpe PartnerInvoiceID)
    amount_untaxed : (idrisTpe AmountUntaxed)
    amount_total : (idrisTpe AmountTotal)
    name : (idrisTpe NameOT)
    partner_shipping_id : (idrisTpe PartnerShippingID)
    picking_policy : (idrisTpe PickingPolicy)
    carrier_id : (idrisTpe CarrierID)
    requested_date : (idrisTpe RequestedDate)    
    lines : List SOL_Simple.RecordCols
    
  %runElab derive "SO_O2M.RecordCols" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
{-   
  toRecord : GetRow PrimCols -> RecordCols
  toRecord =  to . (\x => MkSOP $ Z x)
          
  read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List RecordCols) 
  read_records op= do
    c <- connect DB_URI
    rows <- get c (table model) (columns (table model)) (domain&&op)
    let so_s= [ toRecord ox | ox <- rows ]
    finish c    
    pure so_s
  
  main_runET : (op:Op) -> IO (List RecordCols)
  main_runET op = do Left err <- runEitherT (read_records op {io = EitherT SQLError IO} )
                       | Right l1 => pure l1
                     printLn err
                     pure []

  export
  read : HasIO io => (op:Op) -> io (List RecordCols)
  read op = do  
     l1 <- (liftIO $ main_runET op)
     pure l1
-}
