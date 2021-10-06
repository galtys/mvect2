module Category.Transaction.Demo

import Data.Zippable
import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import JSON

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Hom
import Category.Transaction.Journal
import Crypto.Hash.SHA256
import Data.Ratio

%language ElabReflection


prices: List TQty
prices = [10,7,5,2,11,9,50,1,33,100]

skus : List ProdKey
skus = [("p"++show i) | i <- [1..(length prices)]]

public export
pricelist : List (ProdKey,TQty)
pricelist = (zip skus prices)

public export
Pricelist : Hom2
Pricelist xs = map (pricelist_f1 pricelist) xs

public export
pricelist_journal : Journal
pricelist_journal = jref (JDate 0 (JDoc "plist") PriceList)

--public export 
--pricelist_term : LineTerm
--pricelist_term = LPList pricelist_1' (LRef pricelist_journal)

public export
pjb : Account
pjb = C (MkC "P.J.Bridgman")

public export
hilton : Account
hilton = C (MkC "Hilton")

public export
pjb_loc : Account
pjb_loc = L (MkL "Enfield")

public export
pjb_r : Account
pjb_r = L (MkL "Reservation")

public export
hilton_loc : Account
hilton_loc = L (MkL "Bristol")

public export
so1_j : Journal
so1_j = jref (JDate 0 (JOrder pjb_loc pjb_r pjb_loc hilton_loc) SaleOrder) 

public export
so1_l1 : Line
so1_l1 = MkLine "p1" 5 "£" INC20 31 (percent 0)

public export
l1_e : LineExpr
l1_e = snd $ get_line so1_l1

public export
l1_e_2 : LineExpr
l1_e_2 = (addLineExpr l1_e l1_e) 

public export
so1 : OrderTerm
so1 = ChO so1_j [get_line so1_l1] 

public export
test_demo : IO ()
test_demo = do
  --printLn (get_len "č")
  
--  putStrLn (show eval_qtyratio (r1*r2) )
  --printLn $ show $ eval_qtyratio (r1*r2)
  printLn so1_l1
  
  printLn l1_e
  printLn (get_hom1 l1_e, get_hom2 l1_e)
  printLn (get_hom1 l1_e_2, get_hom2 l1_e_2)


