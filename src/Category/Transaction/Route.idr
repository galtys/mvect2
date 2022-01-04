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
           saleOrder : MoveKey
           saleOrder = MkMK (Partner Sale del) (Control Sale inv) Forecast --OnHand: goods in transit, money in transit
           saleInvoice : MoveKey
           saleInvoice = MkMK (Control Sale inv) (Out del) Forecast  --OnHand: delivery,return,payment,refund
           saleDemand : MoveKey
           saleDemand = MkMK (Out del) Self Forecast --OnHand: goods allocation
           
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
           forecastIn = MkMK (Border self_company) (In del) Forecast --OnHand: allocation from supplier route to customer route            
           purchaseInvoice : MoveKey
           purchaseInvoice = MkMK (In del) (Control Purchase inv) Forecast --OnHand: in delivery, in return to supplier, suppl payment, suppl refund
                      
           {-
           forecastIn : MoveKey
           forecastIn = MkMK Self (Border self_company) Forecast --OnHand: allocation from supplier route to customer route            
           purchaseInvoice : MoveKey
           purchaseInvoice = MkMK (Border self_company) (Control Purchase inv) Forecast --OnHand: in delivery, in return to supplier, suppl payment, suppl refund
           -}
           purchaseOrder : MoveKey
           purchaseOrder = MkMK (Control Purchase inv) (Partner Purchase del) Forecast  -- OnHand transit
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
     i = MkMK Self (Border self_company) Forecast
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
     r = MkMK (Taxman self_taxman) (Border self_taxman) Forecast
     a : MoveKey
     a = MkMK (Border self_taxman) Self Forecast
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
     r = MkMK (Bank self_bank) (Border self_bank) Forecast
     a : MoveKey
     a = MkMK (Border self_bank) Self Forecast
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
