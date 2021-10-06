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

public export
pricelist_1'_map : Hom2_f' -> SortedMap ProdKey TQty
pricelist_1'_map xs= fromList xs --pricelist_1'

public export
pricelist_f1 : Hom2_f' -> Hom2_f
pricelist_f1 pl (px,qty) =  case (lookup px (pricelist_1'_map pl) ) of 
                                     Just price => ("£",qty*price) 
                                     Nothing => ("£", 0) 

public export
get_line : Line -> Product2
get_line l =
   let h1=LEHom1 (qty l)
       pk2 = MkProdK2 (sku l) (currency l)
       p = LEMul (price_unit l) UnitPrice h1
       d = LEMul (discount l) Discount p 
       t = LETaxCode (tax_code l) d in (pk2,t)

public export
get_hom1 : LineExpr -> TQty
get_hom1 (LEHom1 qty) = qty
get_hom1 (LETaxCode tc l) = (get_hom1 l)
get_hom1 (LEAdd l1 l2) = (get_hom1 l1) + (get_hom1 l2)
get_hom1 (LEMul u mu l) = get_hom1 l

public export
get_hom2 : LineExpr -> TQty 
get_hom2 (LEHom1 qty) = 1
get_hom2 (LETaxCode tc l) = (get_hom2 l)
get_hom2 (LEAdd l1 l2) = (get_hom2 l1) + (get_hom2 l2)
get_hom2 (LEMul u mu l) = (get_hom2 l) * u

public export
addLineExpr : LineExpr -> LineExpr -> LineExpr
addLineExpr x y = 
       let q1 = get_hom1 x
           q2 = get_hom1 y
           q = q1+q2
           l1 = LEMul (q1/q) MultQty x
           l2 = LEMul (q2/q) MultQty y
           l = LEAdd l1 l2 in l


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
