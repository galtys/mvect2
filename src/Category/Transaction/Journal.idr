module Category.Transaction.Journal

import Data.Zippable
import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import JSON

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Hom
import Crypto.Hash.SHA256
import Data.Ratio

%language ElabReflection

public export
jref : Journal -> Journal
jref x = JRef $ sha256 $ encode x

prices_1: List TQty
prices_1 = [10,7,5,2,11,9,50,1,33,100]

sku_1 : List ProdKey
sku_1 = [("p"++show i) | i <- [1..(length prices_1)]]

public export
pricelist_1' : Hom2_f' 
pricelist_1' = zip sku_1 prices_1

public export
pricelist_1'_map : Hom2_f' -> SortedMap ProdKey TQty
pricelist_1'_map xs= fromList xs --pricelist_1'

public export
pricelist_f1 : Hom2_f' -> Hom2_f
pricelist_f1 pl (px,qty) =  case (lookup px (pricelist_1'_map pl) ) of 
                                     Just price => ("£",qty*price) 
                                     Nothing => ("£", 0) 

public export
Pricelist : Hom2
Pricelist xs = map (pricelist_f1 pricelist_1') xs

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
fromLine : Line -> LineTerm
fromLine l = (LDiscount (discount l) (LHom2 ((currency l, price_unit l))  (LHom1 (((sku l),(qty l))) ) ) )

public export
so1_l1 : Line
so1_l1 = MkLine "p1" 5 "£" INC20 31 (percent 0)

public export 
so1_lt1 : LineTerm
so1_lt1 = fromLine so1_l1

public export
so1 : OrderTerm
so1 = ChO so1_j [so1_lt1] 

public export
get_hom1 : LineTerm -> Product
get_hom1 (LHom1 qty) = qty
--get_hom1 (LPList pricelist x) = get_hom1 x
get_hom1 (LHom2 price_unit x) = get_hom1 x
get_hom1 (LDiscount discount x) = get_hom1 x

public export
get_hom2 : LineTerm -> Hom2_f   --(TProduct->TProduct)
get_hom2 (LHom1 (px,qty) ) = id
--get_hom2 (LPList pricelist l) = (pricelist_f1 pricelist) . (get_hom2 l)
get_hom2 (LHom2  (cy,p_u)  l) = ( (\(px,qt) => (cy, qt*p_u)  )  . (get_hom2 l))
                                               
get_hom2 (LDiscount d l) = (\(px,qty) => (px,qty*d) ) . (get_hom2 l)

public export
fromLineTerm : LineTerm -> LineTQty
fromLineTerm (LHom1 qty) = (LTQHom1 (snd qty))
fromLineTerm (LHom2 price_unit x) = (LTQHom2 (snd price_unit) (fromLineTerm x))
fromLineTerm (LDiscount discount x) = (LTQDiscount discount (fromLineTerm x))

public export
mufum : Hom2_f
mufum = get_hom2 so1_lt1

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
