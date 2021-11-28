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
jref : Journal -> Journal
jref x = JRef $ sha256 $ encode x



public export
getHomW : OrderEvent ->  List (Date,Hom2)
getHomW (WHom2 date h2) = [(date,h2)]
--getHomW (WSub sub) = getHomW sub
getHomW (LHom1 date mv h1) = []
--getHomW (LCo x y) = []
--getHomW (LPro x y) = []
--getHomW (Add x y ) = (getHomW x)++(getHomW y)

getHomL : OrderEvent ->  MoveType -> List (Date,Hom1)
getHomL (WHom2 date h2) m = [] 
--getHomL (WSub sub) m = []
getHomL (LHom1 date mv h1) m = if (mv==m) then [(date,h1)] else [] 
--getHomL (LCo x y) m = []
--getHomL (LPro x y) m = [] 
--getHomL (Add x y) m = (getHomL x m)++(getHomL y m)
-}

--public export
--addOrderEvent : OrderEvent -> OrderEvent -> OrderEvent
--addOrderEvent x y = Add x y


{-
public export
getWSub : OrderEvent -> List (Date,Hom2)
getWSub (WHom2 d h2) = [(d,h2)]
getWSub sub = getWSub sub

--getWSub (WDeliveryLine delivery subtotal) = getWSub subtotal


public export
getDeliveryLine : OrderEvent -> LineTerm 
getDeliveryLine (WSub h2) = LEHom1 0
getDeliveryLine (WDeliveryLine delivery subtotal) = delivery --? recursive?



public export
addOrderEvent : OrderEvent -> OrderEvent -> OrderEvent
addOrderEvent x y = 
      let h2 = evalProduct2List ( (getWSub x) ++ (getWSub y) )
          d = (getDeliveryLine x) `addLineTerm` (getDeliveryLine y)
          o = WDeliveryLine d (WSub h2) in o


merge_item_into3 : SortedMap Journal OrderEvent -> Term -> (SortedMap Journal OrderEvent)
merge_item_into3 acc x = mergeWith (addOrderEvent) acc (fromList [x])


fromJournalTermList : JournalTerm -> SortedMap Journal OrderEvent
fromJournalTermList xs = foldl merge_item_into3 empty xs

public export
evalJournalTermList : JournalTerm -> JournalTerm
evalJournalTermList xs = toList $ fromJournalTermList xs 

-}

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
