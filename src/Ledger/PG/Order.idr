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

record RSaleOrder where
  constructor MkRSO
  pk : Bits32 --(idrisTpe Id_OT)
  origin : Maybe String --(idrisTpe Origin)
  order_policy : String --(idrisTpe OrderPolicy)
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
   
toRSO : GetRow PrimListSaleOrderCols -> RSaleOrder
toRSO =  to . (\x => MkSOP $ Z x)

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

record RSaleOrderLine where
  constructor MkRSOL
  pk : Bits32
  --order_id : Bits32
  price_unit : TQty
  product_uom_qty : TQty
  discount : Maybe TQty
  product_id : Maybe Bits32
  delivery_line : Maybe Bool

%runElab derive "RSaleOrderLine" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

PrimListSaleOrderLineCols : List Column
PrimListSaleOrderLineCols = [Id_OLT,PriceUnit,ProductUomQty,Discount,ProductID,DeliveryLine]

--PrimSoFields : List Field
--PrimSoFields = toFields PrimListSaleOrderCols 

--PrimSolFields : List Field
--PrimSolFields = toFields ListSaleOrderLineCols

SO_NP : Table
SO_NP = MkTable "sale_order"
        PrimListSaleOrderCols
SOL_NP : Table
SOL_NP = MkTable "sale_order_line"
        PrimListSaleOrderCols

--(getPrimCols (fields SaleOrder))

mutual  
  SaleOrder : Model
  SaleOrder = MkM SO_NP (Id_OT) ((toFields (columns SO_NP))++[OrderLines] ) 
  
  OrderID : Field
  OrderID = M2O SaleOrder PrimOrderID
 
  SaleOrderLine : Model
  SaleOrderLine = MkM SOL_NP (Id_OLT) ((toFields (columns SOL_NP))++[OrderID])

  OrderLines : Field
  OrderLines = O2M SaleOrderLine


toRSOL : GetRow PrimListSaleOrderLineCols -> RSaleOrderLine
toRSOL =  to . (\x => MkSOP $ Z x)

-- IO

getSQLt :  (t        : Table)
       ---> (cs       : List Column)
       ---> {auto 0 _ : Elems cs (columns t)}
       -> (query    : Op)
       -> String
getSQLt t query =
  let cols = fastConcat $ intersperse ", " $ map name (columns t)
   in #"SELECT \#{cols} FROM \#{t.name} WHERE \#{opToSQL query};"#
    
export
get_t :  HasIO io
    => MonadError SQLError io
    => Connection
    -> (t        : Table)
    -> (query : Op)
    -> io (List $ GetRow (columns t))
get_t c t query = do
  res <- exec c (getSQLt t query) TUPLES_OK
  getRows (names (columns t)) (readers (columns t)) res

{-  
read_table_t : HasIO io => MonadError SQLError io => Connection -> (t:Table) -> (op:Op) -> io (List (GetRow (columns t)))   
read_table_t c t op = do
    rows <- get_t c t op--(StateOT /= "cancel")
    pure rows


read_model_v : HasIO io => MonadError SQLError io => Connection -> (m:Model) -> (opx:Op) -> Bits32 -> io (List (GetRow (columns (table m) )))   
read_model_v c (MkM t pk fields) opx val= do
    rows <- read_table_t c t ((O2M pk val)&&opx) --(pk == (cast val))
    pure rows
    

read_join_model : HasIO io => MonadError SQLError io => Connection -> (m:Model) -> List Bits32 -> io ()
read_join_model c m [] = pure ()
read_join_model c m (x::xs) = ret where
       read_fk_one : Connection -> (fk_m:Model) -> (fk_c:Column) -> Bits32 -> io (List (GetRow (columns (table fk_m) )) )
       read_fk_one c fk_m fk_col val = do
           r <- read_table_t c (table fk_m) ((JC fk_col (pk fk_m)) && (O2M (pk m) val))
           pure r
           
       read_fk : Connection -> List (Model,Column) -> io ()
       read_fk c [] = pure ()
       read_fk c ((fk_m, fk_col) :: uuu) = do
          r <- read_fk_one c fk_m fk_col x
          
          --printLn r
          
          --r <- get c (table fk_m) (columns (table fk_m)) 
          --rx <- read_table_t c (table fk_m) (fk_col == x ) --JC fk_col)
          
          
          pure ()
       
       --rows <- get c 
       ret : io ()
       ret = pure ()
-}
       
read_sale_orders : HasIO io => MonadError SQLError io => Connection -> io (List RSaleOrder) 
read_sale_orders c  = do
  rows <- get c SO_NP (columns SO_NP) (StateOT /= "cancel")
  --rows <- read_table_t c SO_NP (StateOT /= "cancel") 
  --rows <- read_prim_model c SaleOrder (StateOT /= "cancel") 
  --rows <- get c (table SaleOrder) (columns (table SaleOrder)) (StateOT /= "cancel")   
  --lines <- read_prim_model c SaleOrderLine 
  
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

