module Category.Transaction.Journal

import Data.Zippable
import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import JSON

import Category.Transaction.Types

%language ElabReflection




prices_1: List Qty
prices_1 = [10,7,5,2,11,9,50,1,33,100]

sku_1 : List ProdKey
sku_1 = [("p"++show i) | i <- [1..(length prices_1)]]

pricelist_1' : List (ProdKey,Qty)
pricelist_1' = zip sku_1 prices_1

public export
pricelist_1'_map : SortedMap ProdKey Qty
pricelist_1'_map = fromList pricelist_1'



public export
pricelist_f1 : Hom2_f
pricelist_f1 (Debit (px,qty)) =  case (lookup px pricelist_1'_map) of 
                                     Just price_unit => Debit ("£",qty*price_unit) 
                                     Nothing => Debit ("£", 0)
pricelist_f1 (Credit (px,qty)) = case (lookup px pricelist_1'_map) of 
                                     Just price_unit => Credit ("£",qty*price_unit) 
                                     Nothing => Credit ("£", 0)


{-
public export
pricelist_f : Hom2_f
pricelist_f (Debit ("p1",qty)) =  Debit ("£",qty*10)
pricelist_f (Credit ("p1",qty)) =  Credit ("£",qty*10)
pricelist_f (Debit ("p2",qty)) =  Debit ("£",qty*7)
pricelist_f (Credit ("p2",qty)) =  Credit ("£",qty*7)
pricelist_f (Debit ("p3",qty)) =  Debit ("£",qty*5)
pricelist_f (Credit ("p3",qty)) =  Credit ("£",qty*5)
pricelist_f (Debit ("p4",qty)) =  Debit ("£",qty*2)
pricelist_f (Credit ("p4",qty)) =  Credit ("£",qty*2)
pricelist_f (Debit ("p5",qty)) =  Debit ("£",qty*11)
pricelist_f (Credit ("p5",qty)) =  Credit ("£",qty*11)
pricelist_f (Debit ("p6",qty)) =  Debit ("£",qty*9)
pricelist_f (Credit ("p6",qty)) =  Credit ("£",qty*9)
pricelist_f (Debit ("p7",qty)) =  Debit ("£",qty*50)
pricelist_f (Credit ("p7",qty)) =  Credit ("£",qty*50)
pricelist_f (Debit ("p8",qty)) =  Debit ("£",qty*1)
pricelist_f (Credit ("p8",qty)) =  Credit ("£",qty*1)
pricelist_f (Debit ("p9",qty)) =  Debit ("£",qty*33)
pricelist_f (Credit ("p9",qty)) =  Credit ("£",qty*33)
pricelist_f (Debit ("p10",qty)) =  Debit ("£",qty*100)
pricelist_f (Credit ("p10",qty)) =  Credit ("£",qty*100)
pricelist_f _ = Debit ("",0)
-}

public export
Pricelist : Hom2
Pricelist xs = map pricelist_f1 xs


public export
pricelist_journal : Journal
pricelist_journal = MkDate 0 (MkDoc "plist") PriceList

public export 
pricelist_term : LineTerm
pricelist_term = PList pricelist_1' (LRef pricelist_journal)



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
so1 : Journal
so1 = MkDate 0 (Order pjb_loc pjb_r pjb_loc hilton_loc) SaleOrder

--public export
--t1_r : Term
--t1_r = Ch (Ref so1) th11 --reservation
--public export
--t1_d : Term
--t1_d = Ch (Ref so1) th11' --delivery

--public export
--t1 : Term
--t1 = Co so1 t1_r t1_d 

--public export
--encode_x : String
--encode_x = encode t1

--public export
--term_x : Either JSONErr Term
--term_x = decode encode_x
