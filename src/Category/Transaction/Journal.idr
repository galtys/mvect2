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
getWHom2 : OrderTerm -> Hom2
getWHom2 (WHom2 h2) = h2
getWHom2 (WDeliveryLine delivery subtotal) = getWHom2 subtotal

public export
getDeliveryLine : OrderTerm -> LineTerm 
getDeliveryLine (WHom2 h2) = LEHom1 0
getDeliveryLine (WDeliveryLine delivery subtotal) = delivery --? recursive?

public export
addOrderTerm : OrderTerm -> OrderTerm -> OrderTerm
addOrderTerm x y = 
      let h2 = evalProduct2List ( (getWHom2 x) ++ (getWHom2 y) )
          d = (getDeliveryLine x) `addLineTerm` (getDeliveryLine y)
          o = WDeliveryLine d (WHom2 h2) in o


{- Keep
public export
pricelist_1'_map : Hom2_f' -> SortedMap ProdKey TQty
pricelist_1'_map xs= fromList xs --pricelist_1'

public export
pricelist_f1 : Hom2_f' -> Hom2_f
pricelist_f1 pl (px,qty) =  case (lookup px (pricelist_1'_map pl) ) of 
                                     Just price => ("£",qty*price) 
                                     Nothing => ("£", 0) 
-}






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
