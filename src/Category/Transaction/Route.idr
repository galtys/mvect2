module Category.Transaction.Route

import Libc.Time
import Category.Transaction.Types
import Category.Transaction.Hom
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
import UserDataDemo

%language ElabReflection

export
routeSha : Date -> RouteSumT -> RouteRef
routeSha d r = sha256 (d++ (encode r))

export           
soForecastFromFx : FxData -> OrderControlRoute --SaleForecastRoute
soForecastFromFx fx = ret where
           inv : BrowseResPartner.RecordModel
           inv = (invoice fx)
           del : BrowseResPartner.RecordModel
           del = (delivery fx)                      
           
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
           forecastIn : MoveKey
           forecastIn = MkMK (In del) (Border del) Forecast                     
           purchaseInvoice : MoveKey
           purchaseInvoice = MkMK (Border del) (Control Purchase inv) Forecast
           purchaseOrder : MoveKey
           purchaseOrder = MkMK (Control Purchase inv) (Partner Purchase del) Forecast
           
           ret : OrderControlRoute --PurchaseForecastRoute 
           ret = MkORrec forecastIn purchaseInvoice purchaseOrder Purchase


export
InventoryInputRoute : ListRoute
InventoryInputRoute = (MkListR i [] Purchase) where
     i : MoveKey
     i = MkMK Self (In self_company) Forecast
export     
InventoryInputRouteT : RouteSumT     
InventoryInputRouteT =MkAl InventoryInputRoute
export
InventoryInputRouteKey : RouteKey
InventoryInputRouteKey = (MkRK InitDate (routeSha InitDate InventoryInputRouteT) Progress)
export
InventoryOutputRoute : ListRoute
InventoryOutputRoute = (MkListR i [] Sale) where
     i : MoveKey
     i = MkMK (Out self_company) Self Forecast
export     
InventoryOutputRouteT : RouteSumT     
InventoryOutputRouteT =MkAl InventoryOutputRoute
export
InventoryOutputRouteKey : RouteKey
InventoryOutputRouteKey = (MkRK InitDate (routeSha InitDate InventoryOutputRouteT) Progress)

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
InitRouteKey : RouteKey
InitRouteKey = (MkRK InitDate (routeSha InitDate InitRouteT) Progress)

export
TaxmanInputRoute : ListRoute
TaxmanInputRoute = (MkListR i [] Purchase) where
     i : MoveKey
     i = MkMK (Taxman self_company) (In self_company) Forecast     
export     
TaxmanInputRouteT : RouteSumT
TaxmanInputRouteT = MkAl TaxmanInputRoute
export
TaxmanInputRouteKey : RouteKey
TaxmanInputRouteKey = (MkRK InitDate (routeSha InitDate TaxmanInputRouteT) Progress)
export
TaxmanOutputRoute : ListRoute
TaxmanOutputRoute = (MkListR i [] Sale) where
     i : MoveKey
     i = MkMK (Out self_company) (Taxman self_company) Forecast

export     
TaxmanOutputRouteT : RouteSumT
TaxmanOutputRouteT = MkAl TaxmanOutputRoute
export
TaxmanOutputRouteKey : RouteKey
TaxmanOutputRouteKey = (MkRK InitDate (routeSha InitDate TaxmanOutputRouteT) Progress)
export
BankInputRoute : ListRoute
BankInputRoute = (MkListR i [] Purchase) where
     i : MoveKey
     i = MkMK (Bank self_company) (In self_company) Forecast
     
export     
BankInputRouteT : RouteSumT
BankInputRouteT = MkAl BankInputRoute
export
BankInputRouteKey : RouteKey
BankInputRouteKey = (MkRK InitDate (routeSha InitDate BankInputRouteT) Progress)

export
BankOutputRoute : ListRoute
BankOutputRoute = (MkListR i [] Sale) where
     i : MoveKey
     i = MkMK (Out self_company) (Bank self_company) Forecast

export     
BankOutputRouteT : RouteSumT
BankOutputRouteT = MkAl BankOutputRoute
export
BankOutputRouteKey : RouteKey
BankOutputRouteKey = (MkRK InitDate (routeSha InitDate BankOutputRouteT) Progress)
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
getDocRouteType (MkAl (MkListR (MkMK (Out y) (Taxman x) ledger) [] Sale)) = TaxSaleRoute
getDocRouteType (MkAl (MkListR (MkMK (Taxman x) (In y) ledger) [] Purchase)) = TaxPurchaseRoute

getDocRouteType (MkAl (MkListR (MkMK (Bank x) (In y) ledger) [] Purchase)) = BankRoute
getDocRouteType (MkAl (MkListR (MkMK (Out y) (Bank x) ledger) [] Sale)) = BankRoute

getDocRouteType (MkReR (MkRR allocation (MkMK from to ledger) direction)) = DocumentRouteType.Allocation

getDocRouteType (MkAl (MkListR (MkMK Self (In x) ledger) [] Purchase)) = DocumentRouteType.StockInputRoute -- ?kkkdfasdflas_3 --DocumentRouteType.NA
getDocRouteType (MkAl (MkListR (MkMK (Out x) Self ledger) [] Sale)) = DocumentRouteType.StockOutputRoute 

getDocRouteType (MkAl x) = DocumentRouteType.NA
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

export
getDocumentType : WhsEntry -> DocumentType
getDocumentType (MkWE ref (Fx121 x y) move_key) = getDxDocumentType move_key
getDocumentType (MkWE ref (Fx11 x y) move_key) = ret_f where
       dxcx : DxCx
       dxcx = toDxCx y
       
       ret : DocumentType
       ret = getDxDocumentType move_key
       
       dx_drcr : DrCr
       dx_drcr = toDrCr (dx y)
       cx_drcr : DrCr
       cx_drcr = toDrCr (cx y)
              
       muf2 : DrCr -> DocumentType -> DocumentType
       muf2 Dr doc = doc
       muf2 Cr doc = negateDocumentType doc
       
       ret_f : DocumentType
       ret_f = 
         case dxcx of
           DX => muf2 dx_drcr ret --(muf1 DX)
           CX => muf2 cx_drcr (swapDxCx ret) --(muf1 CX)

{-
export
getDxCxDocumentType : WhsEntry -> DxCx
getDxCxDocumentType (MkWE ref (Fx121 x y) move_key) = DX 
getDxCxDocumentType (MkWE ref (Fx11 x y) move_key) = (toDxCx y)
-}
getRouteKey : WhsEntry -> Maybe RouteKey
getRouteKey (MkWE (rk) fx move_key) = Just rk

routeLineKeys : RouteLine -> List RouteKey
routeLineKeys (MkRL move whse_f whse_oh) = catMaybes (  (map getRouteKey whse_f)++(map getRouteKey whse_oh) )

export
listRouteKeys : RouteData -> List RouteKey
listRouteKeys (MkRD key dir lines def) = join (map routeLineKeys lines) --?listRouteKeys_rhs_0

