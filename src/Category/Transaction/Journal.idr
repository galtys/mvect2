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
PC20 : TQty
PC20 = (percent 20)

public export
one5 : TQty
one5 = Debit (MkQr 1 5)   --1 - PC20

public export
inc20_const : TQty
inc20_const = Debit (MkQr 1 6) --(  1/(1+one5) ) * one5

public export
taxRatio : TaxCode -> TQty
taxRatio ZeroVAT = 0
taxRatio INC20 = inc20_const
taxRatio EX20 = one5

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

get_tax_codes : LineTerm -> List TaxCode
get_tax_codes (LEHom1 qty) = []
get_tax_codes (LETaxCode taxcode x) = [taxcode]++(get_tax_codes x)
get_tax_codes (LEAdd l1 l2) = (get_tax_codes l1)++(get_tax_codes l2)
get_tax_codes (LEMul u mu l) = (get_tax_codes l)

get_tc_prodkey : List TaxCode -> ProdKey
get_tc_prodkey xs = (concat [(show x) | x <- xs] )

{-
    let xs_map = fromList [(x,0) | x<- xs]
        ret = [ k | (k,v) <- xs_map ] in concat ret       

-}
public export
ev_tax : LineTerm -> LineTerm
ev_tax (LEHom1 qty) = (LEHom1 qty) --terminating
ev_tax (LETaxCode taxcode x) = LEMul (taxRatio taxcode) TaxMul x
ev_tax x = ev_tax x

public export
tax_line : Product2 -> Product2
tax_line ((MkProdK2 keyfrom keyto), y) = --?tax_line_rhs_2
   let t_c = get_tax_codes y
       pk2 = MkProdK2 (get_tc_prodkey t_c) keyto
       t_l = (ev_tax y) in (pk2,t_l)


public export
get_hom1 : LineTerm -> TQty
get_hom1 (LEHom1 qty) = qty
get_hom1 (LETaxCode tc l) = (get_hom1 l)
get_hom1 (LEAdd l1 l2) = (get_hom1 l1) + (get_hom1 l2)
get_hom1 (LEMul u mu l) = get_hom1 l

public export
get_hom2 : LineTerm -> TQty 
get_hom2 (LEHom1 qty) = 1
get_hom2 (LETaxCode tc l) = (get_hom2 l)
get_hom2 (LEAdd l1 l2) = (get_hom2 l1) + (get_hom2 l2)
get_hom2 (LEMul u mu l) = (get_hom2 l) * u

public export
addLineTerm : LineTerm -> LineTerm -> LineTerm
addLineTerm x y = 
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
