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
import Data.List

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
           {-
           saleDemand : MoveKey
           saleDemand = MkMK (Out del) Self Forecast --OnHand: goods allocation
           saleInvoice : MoveKey
           saleInvoice = MkMK (Control Sale inv) (Out del) Forecast  --OnHand: delivery,return,payment,refund           
           saleOrder : MoveKey
           saleOrder = MkMK (Partner Sale del) (Control Sale inv) Forecast --OnHand: goods in transit, money in transit
           -}
           
           saleOrder : MoveKey
           saleOrder = MkMK (Partner Sale del) (Control Sale inv) Forecast
           saleInvoice : MoveKey
           saleInvoice = MkMK (Control Sale inv) (Border del) Forecast
           
           saleDemand : MoveKey
           saleDemand = MkMK (Border del) (Out del) Forecast
           
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
           
           {-
           forecastIn : MoveKey
           forecastIn = MkMK (In del) (Border self_company) Forecast                     
           purchaseInvoice : MoveKey
           purchaseInvoice = MkMK (Border self_company) (Control Purchase inv) Forecast
           purchaseOrder : MoveKey
           purchaseOrder = MkMK (Control Purchase inv) (Partner Purchase del) Forecast
           -}
           forecastIn : MoveKey
           forecastIn = MkMK (In del) (Border del) Forecast                     
           purchaseInvoice : MoveKey
           purchaseInvoice = MkMK (Border del) (Control Purchase inv) Forecast
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
InventoryRouteT : RouteSumT     
InventoryRouteT =MkAl InventoryRoute
export
InventoryRouteKey : RouteKey
InventoryRouteKey = (MkRK InitDate (routeSha InitDate InventoryRouteT) Progress)
export
InventoryRouteRef : Ref
InventoryRouteRef = MkRouteKeyRef InventoryRouteKey


export   
InitRoute : ReconciliationRoute 
InitRoute = ret where
     reconciliation : MoveKey
     reconciliation = MkMK   Init (In self_company) Forecast

     allocation : MoveKey
     allocation = MkMK  (In self_company) Self Forecast
     
     ret : ReconciliationRoute 
     ret = MkRR allocation reconciliation Sale
     
export
InitRouteT : RouteSumT     
InitRouteT = MkReR InitRoute
export
InitRouteRef : Ref
InitRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha InitDate InitRouteT ) Progress)


     
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
     ret = MkRR a r Sale
     
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
getDxDocumentType : MoveKey -> DocumentType
{- Sale Route -}
getDxDocumentType (MkMK (Partner Sale y1) (Control Sale y2) Forecast) = SaleOrder
getDxDocumentType (MkMK (Partner Sale y1) (Control Sale y2) OnHand) = Delivery

getDxDocumentType (MkMK (Control Sale y) (Border x)  Forecast) = CustomerInvoice
getDxDocumentType (MkMK (Control Sale y) (Border x)  OnHand) = Dispatch

getDxDocumentType (MkMK (Border x) (Out y) Forecast) = SaleReservation
getDxDocumentType (MkMK (Border x) (Out y)  OnHand) = SaleAllocation

{-Purchase Route -}
getDxDocumentType (MkMK (In x) (Border y) Forecast) = PurchaseReservation
getDxDocumentType (MkMK (In x) (Border y) OnHand) = PurchaseAllocation

getDxDocumentType (MkMK (Border x) (Control Purchase y) Forecast) = SupplierInvoice
getDxDocumentType (MkMK (Border x) (Control Purchase y) OnHand) = GoodsReceipt

getDxDocumentType (MkMK (Control Purchase y) (Partner Purchase x)  Forecast) = PurchaseOrder
getDxDocumentType (MkMK (Control Purchase y) (Partner Purchase x)  OnHand) = Shipping

getDxDocumentType _ = NotDefined




export
getDocRouteType : RouteSumT -> DocumentRouteType
getDocRouteType (MkReR (MkRR allocation (MkMK Init to ledger) direction)) = InitRoute
getDocRouteType (MkReR (MkRR allocation (MkMK Loss to ledger) direction)) = StockLossRoute
getDocRouteType (MkReR (MkRR allocation (MkMK (Taxman x) to ledger) Sale)) = TaxSaleRoute
getDocRouteType (MkReR (MkRR allocation (MkMK (Taxman x) to ledger) Purchase)) = TaxPurchaseRoute
getDocRouteType (MkReR (MkRR allocation (MkMK (Bank x) to ledger) direction)) = BankRoute
getDocRouteType (MkReR (MkRR allocation (MkMK from to ledger) direction)) = DocumentRouteType.NA
{-
getDocRouteType (MkReR (MkRR allocation (MkMK Self to ledger) direction)) = DocumentRouteType.NA
getDocRouteType (MkReR (MkRR allocation (MkMK (In x) to ledger) direction)) = DocumentRouteType.NA
getDocRouteType (MkReR (MkRR allocation (MkMK (Out x) to ledger) direction)) = ?getDocRouteType_rhs_xs0_4
getDocRouteType (MkReR (MkRR allocation (MkMK (Border x) to ledger) direction)) = ?getDocRouteType_rhs_xs0_5
getDocRouteType (MkReR (MkRR allocation (MkMK (Control x y) to ledger) direction)) = ?getDocRouteType_rhs_xs0_8
getDocRouteType (MkReR (MkRR allocation (MkMK (Partner x y) to ledger) direction)) = ?getDocRouteType_rhs_xs0_9
getDocRouteType (MkReR (MkRR allocation (MkMK (Transit x y) to ledger) direction)) = ?getDocRouteType_rhs_xs0_10
-}

--getDocRouteType (MkAl (MkListR (MkMK Self (Out x) ) Purchase) [] direction) = StockRoute
getDocRouteType (MkAl (MkListR allocation [] Purchase)) = DocumentRouteType.StockRoute -- ?kkkdfasdflas_3 --DocumentRouteType.NA
getDocRouteType (MkAl x) = DocumentRouteType.NA
--getDocRouteType (MkAl (MkListR allocation (x :: xs) Purchase)) = ?kkkdfasdflas_4 --DocumentRouteType.NA

getDocRouteType (MkOR (MkORrec allocation control order Sale)) = SaleRoute
getDocRouteType (MkOR (MkORrec allocation control order Purchase)) = PurchaseRoute



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
swapDxCx AllocationDoc = AllocationDoc
swapDxCx Allocation = Allocation

swapDxCx Shipping = Payment
swapDxCx NotDefined = NotDefined
swapDxCx PurchaseReservation = PurchaseReservation
swapDxCx SaleReservation = SaleReservation
swapDxCx PurchaseAllocation = PurchaseAllocation
swapDxCx SaleAllocation = SaleAllocation
swapDxCx GoodsReceipt = Payment
--swapDxCx RouteDoc = RouteDoc
--swapDxCx RouteDocInv = RouteDocInv

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

negateDocumentType Return = Dispatch
negateDocumentType Reservation = Reservation
negateDocumentType Allocation = Allocation
negateDocumentType AllocationDoc = AllocationDoc
negateDocumentType Shipping = Shipping
negateDocumentType NotDefined = NotDefined
negateDocumentType PurchaseReservation = PurchaseReservation
negateDocumentType SaleReservation = SaleReservation
negateDocumentType PurchaseAllocation = PurchaseAllocation
negateDocumentType SaleAllocation = SaleAllocation
negateDocumentType GoodsReceipt = Delivery
--negateDocumentType RouteDoc = RouteDocInv
--negateDocumentType RouteDocInv = RouteDoc

export
isAllocationDocument : DocumentType -> Bool
isAllocationDocument SaleOrder = False
isAllocationDocument SaleOrderAmendment = False
isAllocationDocument PurchaseOrder = False
isAllocationDocument Order = False
isAllocationDocument CustomerInvoice = False
isAllocationDocument SupplierInvoice = False
isAllocationDocument CustomerCreditNote = False
isAllocationDocument SupplierCreditNote = False
isAllocationDocument Invoice = False
isAllocationDocument CreditNote = False
isAllocationDocument Payment = False
isAllocationDocument Refund = False
isAllocationDocument Delivery = False
isAllocationDocument Dispatch = False
isAllocationDocument Return = False
isAllocationDocument Reservation = False
isAllocationDocument Allocation = False
isAllocationDocument AllocationDoc = True
isAllocationDocument Shipping = False
isAllocationDocument NotDefined = False
isAllocationDocument PurchaseReservation = False
isAllocationDocument SaleReservation = False
isAllocationDocument PurchaseAllocation = True
isAllocationDocument SaleAllocation = True
isAllocationDocument GoodsReceipt = True
--isAllocationDocument RouteDoc = False
--isAllocationDocument RouteDocInv = False

export
getDocumentType : WhsEntry -> DocumentType
getDocumentType (MkWE ref fx move_key) = getDxDocumentType move_key


getRouteKey : WhsEntry -> Maybe RouteKey
getRouteKey (MkWE (MkAllocationRef x) fx move_key) = Nothing
getRouteKey (MkWE (MkRouteKeyRef rk) fx move_key) = Just rk



routeLineKeys : RouteLine -> List RouteKey
routeLineKeys (MkRL move whse_f whse_oh) = catMaybes (  (map getRouteKey whse_f)++(map getRouteKey whse_oh) )



export
listRouteKeys : RouteData -> List RouteKey
listRouteKeys (MkRD key dir lines def) = join (map routeLineKeys lines) --?listRouteKeys_rhs_0

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
