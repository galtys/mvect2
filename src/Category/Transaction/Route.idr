module Category.Transaction.Route

import Libc.Time
import Category.Transaction.Types
import Category.Transaction.RouteTypes
import Category.Transaction.Qty
import Data.SortedMap
--import Control.Monad.State
import Crypto.Hash.SHA256
import Data.Ratio
import Generics.Derive
import JSON
import Odoo.Schema.PJBRecDef

%language ElabReflection

export
routeSha : Date -> RouteSumT -> RouteRef
routeSha d r = sha256 (d++ (encode r))

export
self_company : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
self_company = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "Self Main Company", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Self Street", 
         contract = Just False, 
         city = Just "Mid London", 
         zip = Just "CE6 SS", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "self@btconnect.com", 
         street2 = Just "Mid Lane" }

export
self_bank : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
self_bank = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "Self Bank", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Bank Street", 
         contract = Just False, 
         city = Just "City of London", 
         zip = Just "CE6 BANK", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "bank@btconnect.com", 
         street2 = Just "Mid bank Lane" }

export
self_fx : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
self_fx = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "Foreign Currency Exchange", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "fx Street", 
         contract = Just False, 
         city = Just "City of London", 
         zip = Just "FX FEA", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "fx@btconnect.com", 
         street2 = Just "Mid fx Lane" }

export
self_taxman : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
self_taxman = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "Taxman (HMRC)", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Taxman Street", 
         contract = Just False, 
         city = Just "Exeter or wales", 
         zip = Just "T4 T5", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "taxman@btconnect.com", 
         street2 = Just "Mid fx Lane" }

export           
soForecastFromFx : FxData -> OrderControlRoute --SaleForecastRoute
soForecastFromFx fx = ret where
           inv : BrowseResPartner.RecordModel
           inv = (invoice fx)
           del : BrowseResPartner.RecordModel
           del = (delivery fx)                      
           
           saleDemand : MoveKey
           saleDemand = MkMK (Out del) Self Forecast --OnHand: goods allocation
           saleInvoice : MoveKey
           saleInvoice = MkMK (Control Sale inv) (Out del) Forecast  --OnHand: delivery,return,payment,refund           
           saleOrder : MoveKey
           saleOrder = MkMK (Partner Sale del) (Control Sale inv) Forecast --OnHand: goods in transit, money in transit
           
           ret : OrderControlRoute --SaleForecastRoute
           ret = MkORrec saleDemand saleInvoice saleOrder Sale
export
poForecastFromFx : FxData -> OrderControlRoute --PurchaseForecastRoute
poForecastFromFx fx = ret where
           inv : BrowseResPartner.RecordModel
           inv = (invoice fx)
           del : BrowseResPartner.RecordModel
           del = (delivery fx)                      
           {-           
           forecastIn : MoveKey
           forecastIn = MkMK (Border self_company) (In del) Forecast                     
           purchaseInvoice : MoveKey
           purchaseInvoice = MkMK (In del) (Control Purchase inv) Forecast
           purchaseOrder : MoveKey
           purchaseOrder = MkMK (Control Purchase inv) (Partner Purchase del) Forecast
           -} 
           forecastIn : MoveKey
           forecastIn = MkMK (In del) (Border self_company) Forecast                     
           purchaseInvoice : MoveKey
           purchaseInvoice = MkMK (Border self_company) (Control Purchase inv) Forecast
           purchaseOrder : MoveKey
           purchaseOrder = MkMK (Control Purchase inv) (Partner Purchase del) Forecast
           
                      
           {-
           forecastIn : MoveKey
           forecastIn = MkMK Self (Border self_company) Forecast --OnHand: allocation from supplier route to customer route            
           purchaseInvoice : MoveKey
           purchaseInvoice = MkMK (Border self_company) (Control Purchase inv) Forecast --OnHand: in delivery, in return to supplier, suppl payment, suppl refund
           -}
           ret : OrderControlRoute --PurchaseForecastRoute 
           ret = MkORrec forecastIn purchaseInvoice purchaseOrder Purchase
{-
export
custWiRoute : (c:BrowseResPartner.RecordModel) -> (i:BrowseResPartner.RecordModel) -> Route --List Location
custWiRoute c i = [Partner Sale c, Control Sale i, Out c, Self] -- Border p
export
suppWiRoute : (s:BrowseResPartner.RecordModel) -> (i:BrowseResPartner.RecordModel) -> Route --List Location
suppWiRoute s i = [Self, In s, Control Purchase i, Partner Purchase s]
export
initRoute : Route --List Location
initRoute = [Init, In self_company, Self]
-}

export
InitDate : Date
InitDate = "2021-11-01"
export
InventoryRoute : ListRoute
InventoryRoute = (MkListR i [] Purchase) where
     i : MoveKey
     i = MkMK Self (Out self_company) Forecast
export   
InitRoute : ReconciliationRoute 
InitRoute = ret where
     reconciliation : MoveKey
     reconciliation = MkMK Init (In self_company) Forecast
     allocation : MoveKey
     allocation = MkMK (In self_company) Self Forecast
     
     ret : ReconciliationRoute 
     ret = MkRR reconciliation allocation Sale
     
export
InitRouteT : RouteSumT     
InitRouteT = MkReR InitRoute
export
InitRouteRef : Ref
InitRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha InitDate InitRouteT ) Progress)


export     
InventoryRouteT : RouteSumT     
InventoryRouteT =MkAl InventoryRoute
export
InventoryRouteKey : RouteKey
InventoryRouteKey = (MkRK InitDate (routeSha InitDate InventoryRouteT) Progress)
export
InventoryRouteRef : Ref
InventoryRouteRef = MkRouteKeyRef InventoryRouteKey
     
export
TaxRoute : ReconciliationRoute
TaxRoute = ret where
     r : MoveKey
     r = MkMK (Taxman self_taxman) (In self_taxman) Forecast
     a : MoveKey
     a = MkMK (In self_taxman) Self Forecast
     ret : ReconciliationRoute
     ret = MkRR r a Sale     
export
TaxRouteT : RouteSumT
TaxRouteT = MkReR TaxRoute
export
TaxRouteRef : Ref
TaxRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha InitDate TaxRouteT) Progress)
     
export
BankRoute : ReconciliationRoute
BankRoute =  ret where
     r : MoveKey
     r = MkMK (Bank self_bank) (In self_bank) Forecast
     a : MoveKey
     a = MkMK (In self_bank) Self Forecast
     ret : ReconciliationRoute
     ret = MkRR r a Sale
export     
BankRouteT : RouteSumT
BankRouteT = MkReR BankRoute
export
BankRouteRef : Ref
BankRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha InitDate BankRouteT) Progress)

export
FxRoute : ReconciliationRoute
FxRoute = ret where
     r : MoveKey
     r = MkMK (Partner Purchase self_fx) (In self_fx) Forecast
     a : MoveKey
     a = MkMK (In self_fx) Self Forecast
     ret : ReconciliationRoute
     ret = MkRR r a Sale
export
FxRouteT : RouteSumT
FxRouteT = MkReR FxRoute
export
FxRouteRef : Ref
FxRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha InitDate FxRouteT ) Progress)

export
getDxDocumentType : MoveKey -> (DocumentType,DocumentType)
{-
getDxDocumentType (MkMK from to OnHand) = ?getDxDocumentType_rhs_1
getDxDocumentType (MkMK Self to Forecast) = ?getDxDocumentType_rhs_3
getDxDocumentType (MkMK (In x) to Forecast) = ?getDxDocumentType_rhs_4
getDxDocumentType (MkMK (Out x) to Forecast) = ?getDxDocumentType_rhs_5
getDxDocumentType (MkMK (Border x) to Forecast) = ?getDxDocumentType_rhs_6
getDxDocumentType (MkMK Init to Forecast) = ?getDxDocumentType_rhs_7
getDxDocumentType (MkMK Loss to Forecast) = ?getDxDocumentType_rhs_8
-}
{- Sale Route -}
getDxDocumentType (MkMK (Partner Sale y1) (Control Sale y2) ledger) = (SaleOrder,Delivery)
getDxDocumentType (MkMK (Control Sale y) (Out x)  ledger) = (CustomerInvoice,Dispatch)
getDxDocumentType (MkMK (Out x) Self ledger) = (SaleReservation,SaleAllocation)
{-Purchase Route -}
getDxDocumentType (MkMK (In x) (Border y) ledger) = (PurchaseReservation,PurchaseAllocation)
getDxDocumentType (MkMK (Border x) (Control Purchase y) ledger) = (SupplierInvoice,GoodsReceipt)
getDxDocumentType (MkMK (Control Purchase y) (Partner Purchase x)  ledger) = (PurchaseOrder,Shipping) --CustomerInvoice,Dispatch)

{-
getDxDocumentType (MkMK (Partner Purchase y) to Forecast) = ?getDxDocumentType_rhs_15
getDxDocumentType (MkMK (Transit x y) to Forecast) = ?getDxDocumentType_rhs_11
getDxDocumentType (MkMK (Taxman x) to Forecast) = ?getDxDocumentType_rhs_12
getDxDocumentType (MkMK (Bank x) to Forecast) = ?getDxDocumentType_rhs_13
-}
getDxDocumentType _ = (NotDefined,NotDefined)

export
swapDxCx : DocumentType -> DocumentType
swapDxCx SaleOrder = SaleOrder
swapDxCx SaleOrderAmendment = SaleOrderAmendment
swapDxCx PurchaseOrder = PurchaseOrder
swapDxCx Order = Order
swapDxCx CustomerInvoice = CustomerInvoice
swapDxCx SupplierInvoice = SupplierInvoice
swapDxCx CustomerCreditNote = CustomerCreditNote
swapDxCx SupplierCreditNote = SupplierCreditNote
swapDxCx Invoice = Invoice
swapDxCx CreditNote = CreditNote
swapDxCx Payment = Dispatch
swapDxCx Refund = Return
swapDxCx Delivery = Payment
swapDxCx Dispatch = Payment
swapDxCx Return = Refund
swapDxCx Reservation = Reservation
swapDxCx Allocation = Allocation
swapDxCx Shipping = Payment
swapDxCx NotDefined = NotDefined
swapDxCx PurchaseReservation = PurchaseReservation
swapDxCx SaleReservation = SaleReservation
swapDxCx PurchaseAllocation = PurchaseAllocation
swapDxCx SaleAllocation = SaleAllocation
swapDxCx GoodsReceipt = Payment

export
negateDocumentType : DocumentType -> DocumentType
negateDocumentType SaleOrder = SaleOrderAmendment
negateDocumentType SaleOrderAmendment = SaleOrder
negateDocumentType PurchaseOrder = PurchaseOrder --?no amendment?
negateDocumentType Order = Order
negateDocumentType CustomerInvoice = CustomerCreditNote
negateDocumentType SupplierInvoice = SupplierCreditNote
negateDocumentType CustomerCreditNote = CustomerInvoice
negateDocumentType SupplierCreditNote = SupplierInvoice
negateDocumentType Invoice = CreditNote
negateDocumentType CreditNote = Invoice
negateDocumentType Payment = Refund
negateDocumentType Refund = Payment

negateDocumentType Delivery = GoodsReceipt
negateDocumentType Dispatch = Return

negateDocumentType Return = ?negateDocumentType_rhs_14
negateDocumentType Reservation = Reservation
negateDocumentType Allocation = Allocation
negateDocumentType Shipping = Shipping
negateDocumentType NotDefined = NotDefined
negateDocumentType PurchaseReservation = PurchaseReservation
negateDocumentType SaleReservation = SaleReservation
negateDocumentType PurchaseAllocation = PurchaseAllocation
negateDocumentType SaleAllocation = SaleAllocation
negateDocumentType GoodsReceipt = Delivery




{-
export
route2ft : Route -> Ledger -> List MoveKey --(Location,Location)
route2ft [] l = []
route2ft (x::[]) l= []
route2ft (x::y::xs) l = [(MkMK x y l)]++(route2ft xs l)

export
fillRoute : Ref -> List MoveKey -> FxEvent -> WhsEvent ()
fillRoute ref [] fxe = Pure ()
fillRoute ref (mk::xs) fxe = do
     Put ref mk fxe
     fillRoute ref xs fxe
-}
