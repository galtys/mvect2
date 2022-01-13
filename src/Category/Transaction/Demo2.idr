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
run_demo_so : OwnerEvent (RouteData,UserDataMap) --(List WhsEntry)
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
demo_po_so : OwnerEvent (RouteData,UserDataMap) --(List WhsEntry) --Hom1
demo_po_so = do
 Init 
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
 
 --transit_po_full po1 "2021-10-17"
 --receive_po_full po1 "2021-10-25"
 

 --po2 <- new_po date2 dx2 factory1 factory2 
 --reserve_po_full so1 "2021-11-02"
 --transit_po_full po2 "2021-10-18"
 --po3 <- new_po date3 dx1 factory1 factory1   
 {- 
 
 -}
 wx <- run_demo_so
 w <- get_hom' po1
 Pure wx

export
read_ref_data : Ref -> OwnerEvent (Maybe RouteData,UserDataMap)
read_ref_data (MkAllocationRef x) = do
       u <- GetUserData
       Pure (Nothing,u) --?open_ref_rhs_0
read_ref_data (MkRouteKeyRef x) = do
       (rd,u) <- get_hom' x
       Pure (Just rd, u)
export
read_allocation : AllocationRef -> OwnerEvent (Maybe AllocationEntry,UserDataMap)
read_allocation ref = do
       u <- GetUserData
       ma <- GetAE (MkAllocationRef ref)
       Pure (ma,u)
{-
export
read_route : RouteKey -> OwnerEvent (RouteData,UserDataMap)
       r <- get_hom x
       Pure r
-}

export
list_refs : OwnerEvent (List Ref,UserDataMap)
list_refs = do
   user_data_map <- GetUserData
   refs <- ListRefs
   Pure (refs, user_data_map)



export
demo_po_so_whs : WhsEvent (RouteData,UserDataMap) --(List WhsEntry)
demo_po_so_whs = (toWhs   demo_po_so)

