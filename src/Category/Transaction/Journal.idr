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
pricelist_f1 pl (Debit (px,qty)) =  case (lookup px (pricelist_1'_map pl) ) of 
                                     Just price_unit => Debit ("£",qty*price_unit) 
                                     Nothing => Debit ("£", 0) 
                                     
pricelist_f1 pl (Credit (px,qty)) = case (lookup px (pricelist_1'_map pl)) of 
                                     Just price_unit => Credit ("£",qty*price_unit) 
                                     Nothing => Credit ("£", 0)

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
fromLine l = (LDiscount (discount l) (LHom2 (Debit (currency l, price_unit l))  (LHom1 (Debit ((sku l),(qty l))) ) ) )

public export
so1_l1 : Line
so1_l1 = MkLine "p1" 5 "£" INC20 31 (percent 3)

public export 
so1_lt1 : LineTerm
so1_lt1 = fromLine so1_l1

public export
so1 : OrderTerm
so1 = ChO so1_j [so1_lt1] 

public export
get_hom1 : LineTerm -> TProduct
get_hom1 (LHom1 qty) = qty
--get_hom1 (LPList pricelist x) = get_hom1 x
get_hom1 (LHom2 price_unit x) = get_hom1 x
get_hom1 (LDiscount discount x) = get_hom1 x


public export
get_hom2 : LineTerm -> Hom2_f   --(TProduct->TProduct)
get_hom2 (LHom1 (Debit  (px,qty)) ) = id
get_hom2 (LHom1 (Credit (px,qty)) ) = id
--get_hom2 (LPList pricelist l) = (pricelist_f1 pricelist) . (get_hom2 l)
get_hom2 (LHom2 (Debit  (cy,p_u))  l) = ((\x => case x of
                                               (Debit  (px,qt)) => Debit  (cy, qt*p_u )  
                                               (Credit  (px,qt)) => (Credit  (cy, qt*p_u))    )  . (get_hom2 l))
                                               
get_hom2 (LHom2 (Credit  (cy,price_unit))  l) = (\x => case x of
                                               (Debit  (px,qty)) => (Debit  (cy,qty*price_unit))
                                               (Credit  (px,qty)) => (Credit  (cy,qty*price_unit))    ) . (get_hom2 l)

get_hom2 (LDiscount d l) = (\x => case x of
                                               (Debit  (px,qty)) => (Debit  (px,qty*d))
                                               (Credit  (px,qty)) => (Credit  (px,qty*d))  ) . (get_hom2 l)


addLineTerms : LineTerm -> LineTerm -> LineTerm
addLineTerms (LHom1 (Debit y)) (LHom1 (Debit x)) = ?addLineTerms_rhs_9
addLineTerms (LHom1 (Debit y)) (LHom1 (Credit x)) = ?addLineTerms_rhs_10
addLineTerms (LHom1 (Credit y)) (LHom1 (Debit x)) = ?addLineTerms_rhs_11
addLineTerms (LHom1 (Credit y)) (LHom1 (Credit x)) = ?addLineTerms_rhs_12

addLineTerms (LHom1 qty) (LHom2 price_unit x) = ?addLineTerms_rhs_5
addLineTerms (LHom1 qty) (LDiscount discount x) = ?addLineTerms_rhs_6
addLineTerms (LHom2 price_unit x) y = ?addLineTerms_rhs_2
addLineTerms (LDiscount discount x) y = ?addLineTerms_rhs_3

public export
fromLineTerm : LineTerm -> LineTQty
fromLineTerm (LHom1 qty) = (LTQHom1 (getVal qty))
fromLineTerm (LHom2 price_unit x) = (LTQHom2 (getVal price_unit) (fromLineTerm x))
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
