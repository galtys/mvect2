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

import Ledger.PG.Order

%language ElabReflection

%runElab derive "UserName" [Generic, Meta,Eq, ToJSON,FromJSON]
%runElab derive "Namespace" [Generic, Meta,Eq, ToJSON,FromJSON]
%runElab derive "Name" [Generic, Meta,Eq, ToJSON,FromJSON]




public export
record Msg where
   constructor MkMsg
   msg_type : TypeInfo
   

export
listInfo : TypeInfo
listInfo = getInfo "List"


export
aInfo : TypeInfo
aInfo = getInfo "Address"

export
lineInfo : TypeInfo
lineInfo = getInfo "Ledger.PG.Order.Line"


public export
record TestInfo where
  constructor MkTI
  a1 : Int
  a2 : Integer
  a3 : String
  a4 : Bool
  
%runElab derive "TestInfo" [Generic, Meta, Eq, Ord,Show]  

export
tInfo : TypeInfo
tInfo = getInfo "TestInfo"

export
bInfo : TypeInfo
bInfo = getInfo "Country"













prices: List TQty
prices = [10,7,5,2,11,9,50,1,33,100]

skus : List ProdKey
skus = [ PKUser ("p"++show i) | i <- [1..(length prices)]]

public export
pricelist : List (ProdKey,TQty)
pricelist = (zip skus prices)

{-
public export
Pricelist : Hom2
Pricelist xs = map (pricelist_f1 pricelist) xs

public export
pricelist_journal : Journal
pricelist_journal = jref (JDate 0 (JDoc "plist") PriceList)
-}


--public export 
--pricelist_term : LineTerm
--pricelist_term = LPList pricelist_1' (LRef pricelist_journal)

public export
th11 : Hom1
th11 = [ ("p1",4), ("p2",3), ("p1", 9) ]

public export
th11L : List Product
th11L = th11


public export
th11' : Hom1
th11' = [ ("p1",3), ("p1", 6) ]


public export
th12 : Hom1
th12 = [ ("GBP",38) ]


public export
th3 : Hom1
th3 = diffHom1 th11 th12


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

{-
public export
so1_j : Journal
so1_j = jref (JDate 0 (JOrder pjb_loc pjb_r pjb_loc hilton_loc) SaleOrder) 
-}

public export
so1_j : Journal
so1_j = 
    let j_hilton = (MkAcc hilton hilton)
        j_pjb = (MkAcc pjb pjb)
        
        j_hilton_loc = (MkAcc hilton_loc hilton_loc)
        j_pjb_loc = (MkAcc pjb_loc pjb_r)
        
        j_w = (JProAcc WOrder "" j_hilton j_pjb Jstart )     
        j_l = (JProAcc LRes  "" j_hilton_loc j_pjb_loc j_w) in jref j_l

public export
so1_l1 : Line
so1_l1 = MkLine "p1" 5 "£" 120 (percent 0) INC20

public export
l1_t : LineTerm
l1_t = snd $ get_line so1_l1

public export
l1_tax : LineTerm
l1_tax = ev_tax l1_t

public export
l1_t_2 : LineTerm
l1_t_2 = (addLineTerm l1_t l1_t) 

public export
so1_l1_prod2 : Product2
so1_l1_prod2 = get_line so1_l1

public export
so1_l1_ext : LineExt
so1_l1_ext = fromProduct2 so1_l1_prod2


public export
so1_std : STD
so1_std = MkSTD [so1_l1_prod2] [] []

public export
so1 : OrderEvent
so1 = (WHom2 "" so1_std)

--public export
--so1_jt : JournalOrderState
--so1_jt = [ (so1_j, MkOrderState [so1] [] [] [] []  ) ]

public export
test_demo : IO ()
test_demo = do
  --printLn (get_len "č")
  
--  putStrLn (show eval_qtyratio (r1*r2) )
  --printLn $ show $ eval_qtyratio (r1*r2)
  printLn PC20
  printLn one5
  printLn inc20_const
  
  printLn so1_l1
  printLn so1_l1_ext
  
  {-
  printLn l1_t
  printLn l1_tax
  
  printLn (get_hom1_TQty l1_t, get_hom2_TQty l1_t)
  printLn (get_hom1_TQty l1_t_2, get_hom2_TQty l1_t_2)
  printLn "tax line:"
  printLn (get_hom1_TQty l1_tax, get_hom2_TQty l1_tax)
  -}
  
