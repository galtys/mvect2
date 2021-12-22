module Category.Transaction.Route

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
routeSha : RouteSumT -> RouteRef
routeSha r = sha256 $ encode r

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
soForecastFromFx : FxData -> SaleForecastRoute
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
           ret : SaleForecastRoute
           ret = MkSFR saleOrder saleInvoice saleDemand
export
poForecastFromFx : FxData -> PurchaseForecastRoute
poForecastFromFx fx = ret where
           inv : BrowseResPartner.RecordModel
           inv = (invoice fx)
           del : BrowseResPartner.RecordModel
           del = (delivery fx)                      
           forecastIn : MoveKey
           forecastIn = MkMK Self (In del) Forecast --OnHand: allocation from supplier route to customer route 
           purchaseInvoice : MoveKey
           purchaseInvoice = MkMK (In del) (Control Purchase inv) Forecast --OnHand: in delivery, in return to supplier, suppl payment, suppl refund
           purchaseOrder : MoveKey
           purchaseOrder = MkMK (Control Purchase inv) (Partner Purchase del) Forecast  -- OnHand transit
           ret : PurchaseForecastRoute 
           ret = MkPFR forecastIn purchaseInvoice purchaseOrder

export
custWiRoute : (c:BrowseResPartner.RecordModel) -> (i:BrowseResPartner.RecordModel) -> Route --List Location
custWiRoute c i = [Partner Sale c, Control Sale i, Out c, Self] -- Border p
export
suppWiRoute : (s:BrowseResPartner.RecordModel) -> (i:BrowseResPartner.RecordModel) -> Route --List Location
suppWiRoute s i = [Self, In s, Control Purchase i, Partner Purchase s]

export
initRoute : Route --List Location
initRoute = [Init, In self_company, Self]
export   
InitRoute : ReconciliationRoute 
InitRoute = ret where
     reconciliation : MoveKey
     reconciliation = MkMK Init (In self_company) Forecast
     allocation : MoveKey
     allocation = MkMK (In self_company) Self Forecast
     ret : ReconciliationRoute 
     ret = MkRR reconciliation allocation
export
InitDate : Date
InitDate = "2021-11-01"
export
InitRouteRef : Ref
InitRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha (MkReR InitRoute) ) Progress)

export
InventoryRoute : AllocationRoute
InventoryRoute = MkAR i where
     i : MoveKey
     i = MkMK (Border self_company) Self Forecast
export
InventoryRouteRef : Ref
InventoryRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha (MkAl InventoryRoute) ) Progress)
     
export
TaxRoute : ReconciliationRoute
TaxRoute = ret where
     r : MoveKey
     r = MkMK (Taxman self_taxman) (Border self_taxman) Forecast
     a : MoveKey
     a = MkMK (Border self_taxman) Self Forecast
     ret : ReconciliationRoute
     ret = MkRR r a     
export
TaxRouteRef : Ref
TaxRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha (MkReR TaxRoute) ) Progress)
     
export
BankRoute : ReconciliationRoute
BankRoute = ret where
     r : MoveKey
     r = MkMK (Bank self_bank) (Border self_bank) Forecast
     a : MoveKey
     a = MkMK (Border self_bank) Self Forecast
     ret : ReconciliationRoute
     ret = MkRR r a
export
BankRouteRef : Ref
BankRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha (MkReR BankRoute) ) Progress)

export
FxRoute : ReconciliationRoute
FxRoute = ret where
     r : MoveKey
     r = MkMK (Partner Purchase self_fx) (In self_fx) Forecast
     a : MoveKey
     a = MkMK (In self_fx) Self Forecast
     ret : ReconciliationRoute
     ret = MkRR r a
export
FxRouteRef : Ref
FxRouteRef = MkRouteKeyRef (MkRK InitDate (routeSha (MkReR FxRoute) ) Progress)


