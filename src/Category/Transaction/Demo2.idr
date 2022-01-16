module Category.Transaction.Demo2

import Data.Zippable
import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import Libc.Time

--import JSON

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.RouteTypes
import Category.Transaction.Route
import Category.Transaction.Types2
import Category.Transaction.Hom
import Category.Transaction.Journal
import Crypto.Hash.SHA256
import Data.Ratio
import Odoo.Schema.PJBRecDef
import UserDataDemo
import Odoo.PG.BoM

import Category.Transaction.Owner
import Category.Transaction.Warehouse
--import Control.Monad.Either
--%language ElabReflection

export
init_self : OwnerEvent () 
init_self = do
     let -- InitDate
         h1 : Hom1
         h1 = [(pk32DX 999, 100)]
         
         
     --SetFxData (init_route_key) fx
     so_init <- new_so InitDate h1 self_company self_company --RouteKey
     
     --let bank_item : AllocationItem
     --    bank_item = MkAI InitRouteKey BankRouteKey (Fx11 InitDate to_bank )
         
     --aref <- Allocate (MkAE Forecast [bank_item])
     --Log (MkOpen fx)
     --Log (MkNewRoute InitRouteT je)       
     
     
     --Post init_route_key (reconcile InitRoute) je
     --Post init_route_key (convMovekey $ reconcile InitRoute) je
     
     
     --Show je     
     {-
     Put (MkRouteKeyRef init_route_key) (allocation InitRoute) je 
     Put (MkRouteKeyRef init_route_key) (convMovekey $ allocation InitRoute) je --je_dx
     -}
        
              
     --Log (MkNewRoute InventoryInputRouteT fx_empty)
     --tax_route <- NewRoute InitDate TaxRouteT
     --Log (MkNewRoute TaxRouteT fx_empty)          

     --Log (MkNewRoute BankRouteT fx_empty)     
     --fx_route <- NewRoute InitDate FxRouteT
     --Log (MkNewRoute FxRouteT fx_empty)       
     Pure ()


export
run_demo_so : OwnerEvent (RouteData) --(List WhsEntry)
run_demo_so = do
  let date1 : Date
      date1 = "2021-11-02"
      date2 : Date
      date2 = "2021-11-12"      
      --dx1 : Hom1 
      --dx1 = [ (pk32DX 1, 1), (pk32DX 3, 1), (pk32DX 4, 2), (pk32DX 9, 2)]
      dx2 : Hom1 
      dx2 = [ (pk32DX 1, 1), (pk32DX 4, 2), (pk32DX 9, 2)]
      dx3 : Hom1 
      dx3 = [ (pk32DX 1, 1), (pk32DX 4, 1)]
      
  so1 <- new_so date1 dx2 hilton hilton --RouteKey

  --so2 <- new_so date2 dx2 hilton hilton --RouteKey
  
  --reserve_so_full so1 "2021-11-02"
  --deliver_so_full so1 "2021-11-03"
  --invoice_so_full so1 "2021-11-04"
  
  --shipping_done_so_full so1 "2021-11-06"
  
  w <- get_hom' so1
  Pure w

export
demo_po_so : OwnerEvent () --(RouteData,UserDataMap) --(List WhsEntry) --Hom1
demo_po_so = do
   let user_data = (MkUD static_products [] static_boms [])
   UpdateUserData user_data
       --Log (MkUserUpdate user_data)
   --init_route_key <- NewRoute InitDate InitRouteT            
   inventory_input_route <- NewRoute InitDate InventoryInputRouteT
   inventory_output_route <- NewRoute InitDate InventoryOutputRouteT  
   bank_input_route <- NewRoute InitDate BankInputRouteT     
   bank_output_route <- NewRoute InitDate BankOutputRouteT        
   
   tax_input_route <- NewRoute InitDate TaxmanInputRouteT     
   tax_output_route <- NewRoute InitDate TaxmanOutputRouteT        
   
   
   --init_self
   SetRouteNumber (RouteName "InventoryInput") inventory_input_route
   SetRouteNumber (RouteName "InventoryOutput") inventory_output_route   
   SetRouteNumber (RouteName "BankInput") bank_input_route
   SetRouteNumber (RouteName "BankOutput") bank_output_route
   
   SetRouteNumber (RouteName "TaxInput") tax_input_route
   SetRouteNumber (RouteName "TaxOutput") tax_output_route
   
   --init_self
   let date1 : Date
       date1 = "2021-10-01"
       dx1 : Hom1 
       dx1 = [ (pk32DX 1, 10), (pk32DX 3, 15), (pk32DX 4, 5), (pk32DX 5, 1), (pk32DX 6,2)]
       date2 : Date
       date2 = "2021-10-15"
       dx2 : Hom1 
       dx2 = (map (mult_p 2) dx1) ++ [ (pk32DX 7,3) ]

       date3 : Date
       date3 = "2021-11-05"

   po1 <- new_po date1 dx1 factory1 factory1 
   wx <- run_demo_so
   --transit_po_full po1 "2021-10-17"
   --receive_po_full po1 "2021-10-25"


   po2 <- new_po date2 dx2 factory1 factory2 
   --reserve_po_full so1 "2021-11-02"
   --transit_po_full po2 "2021-10-18"
   po3 <- new_po date3 dx1 factory1 factory1   
   {- 

   -}

   --w <- get_hom' po1
   Pure () --w

export
read_ref_data : RouteKey -> OwnerEvent (Maybe RouteData,UserDataMap)
{-
read_ref_data (MkAllocationRef x) = do
       u <- GetUserData
       Pure (Nothing,u) --?open_ref_rhs_0
-}       
read_ref_data x = do
       user_data_map <- GetUserData
       rd <- get_hom' x
       Pure (Just rd, user_data_map)
{-
export
read_allocation : AllocationRef -> OwnerEvent (Maybe AllocationEntry,UserDataMap)
read_allocation ref = do
       u <- GetUserData
       ma <- GetAE (MkAllocationRef ref)
       Pure (ma,u)
-}       
       
export
read_route1 : RouteKey -> OwnerEvent ( List1 (RouteData,UserDataMap) )
read_route1 x = do
       user_data_map <- GetUserData
       rd <- get_hom' x
       let related:List RouteKey
           related = listRouteKeys rd                      
           get_related: List RouteKey -> OwnerEvent ( List (RouteData,UserDataMap) )
           get_related [] = Pure []
           get_related (y :: xs) = do
                           this <- get_hom' y
                           there <- get_related xs
                           Pure ( (this,user_data_map)::there )                      
       rel <- get_related related
       Pure ( (rd,user_data_map):::rel)


export
list_refs : OwnerEvent (List RouteKey,UserDataMap)
list_refs = do
   user_data_map <- GetUserData
   refs <- ListRefs
   Pure (refs, user_data_map)



export
demo_po_so_whs : WhsEvent () --(RouteData,UserDataMap) --(List WhsEntry)
demo_po_so_whs = (toWhs   demo_po_so)

