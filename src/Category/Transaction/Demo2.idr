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

--%language ElabReflection

export
run_demo_so : OwnerEvent ()
run_demo_so = do
  let date1 : Date
      date1 = "2021-11-01"
      dx1 : Hom1 
      dx1 = [ (PK32 DX 1, 1), (PK32 DX 3, 1), (PK32 DX 4, 2)]
  so1 <- new_so date1 dx1 hilton hilton
  reserve_so_full so1 "2021-11-02"
  deliver_so_full so1 "2021-11-03"
  invoice_so_full so1 "2021-11-04"
  shipping_done_so_full so1 "2021-11-06"
  Pure ()

export
demo_po_so : OwnerEvent ()
demo_po_so = do
 Init 
 let date1 : Date
     date1 = "2021-10-01"
     dx1 : Hom1 
     dx1 = [ (PK32 DX 1, 10), (PK32 DX 3, 15), (PK32 DX 4, 5), (PK32 DX 5, 1), (PK32 DX 6,2)]
     date2 : Date
     date2 = "2021-10-15"
     dx2 : Hom1 
     dx2 = (map (mult_p 2) dx1) ++ [ (PK32 DX 7,3) ]
          
     date3 : Date
     date3 = "2021-11-05"

 po1 <- new_po date1 dx1 factory1 factory1 
 transit_po_full po1 "2021-10-17"
 receive_po_full po1 "2021-10-25"
 
 po2 <- new_po date2 dx2 factory2 factory2 
 transit_po_full po2 "2021-10-18"
 
 po3 <- new_po date3 dx1 factory1 factory1   
 run_demo_so
 Pure ()


