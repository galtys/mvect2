module Category.Transaction.Hom

import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import JSON
import Data.List

import Category.Transaction.Qty
import Category.Transaction.Types
import Data.Ratio

%language ElabReflection

{-
public export
get_hom1 : Term -> List Hom1
get_hom1 (Ref j1) = []
get_hom1 (Ch j1  h1) = [h1]
get_hom1 (Pro j1 t1 t2) = (get_hom1 t1) ++ (get_hom1 t2)
get_hom1 (Co j1 t1 t2) = (get_hom1 t1) ++ (get_hom1 t2)
-}

||| Hom1


unTQty : TQty -> TQty -> TQty 
unTQty (Debit a) (Debit b) = Debit (min a b)
unTQty (Credit a) (Credit b) = Credit (min a b)
unTQty (Debit a) (Credit b) = Debit 0
unTQty (Credit a) (Debit b) = Debit 0

partial
public export
merge_as_union : (SortedMap ProdKey TQty) -> Product -> (SortedMap ProdKey TQty)
merge_as_union acc x = case (lookup (fst x) acc) of
                             Nothing => acc
                             Just v => case (unTQty v (snd x) ) of 
                                           0 => (delete (fst x) acc)
                                           nv => (insert (fst x) nv acc)                                           

merge_item_into : (SortedMap ProdKey TQty) -> (ProdKey, TQty) -> (SortedMap ProdKey TQty)
merge_item_into acc x = mergeWith (+) acc (fromList [x])

fromProductList : Hom1 -> SortedMap ProdKey TQty
fromProductList xs = foldl merge_item_into empty xs

public export
evalProductList : Hom1 -> Hom1
evalProductList xs = toList $ fromProductList xs 

public export 
unionHom1' : Hom1 -> Hom1 -> SortedMap ProdKey TQty
unionHom1' a b = foldl merge_as_union (fromProductList a) b

public export
unionHom1 : Hom1 -> Hom1 -> Hom1
unionHom1 a b = toList $ unionHom1' a b

public export
id_hom1 : Hom1
id_hom1 = []

public export
invHom1 : Hom1 -> Hom1
invHom1 [] = []
invHom1 ((k,v)::xs) = [(k, negate v)] ++ (invHom1 xs)


public export
addHom1 : Hom1 -> Hom1 -> Hom1
addHom1 x y = x ++ y

public export
evalHom1 : Hom1 -> Hom1
evalHom1 = evalProductList

public export
diffHom1 : Hom1 -> Hom1 -> Hom1
diffHom1 x y = addHom1 x (invHom1 y)


public export
evDiffHom1 : Hom1 -> Hom1 -> Hom1
evDiffHom1 x y = (evalHom1 (diffHom1 x y))

public export
eqHom1 : Hom1 -> Hom1 -> Bool
eqHom1 x y = (evDiffHom1 x y) == id_hom1

||| Tax
public export
PC20 : TQty
PC20 = (percent 20)

public export
one5 : TQty
one5 = 1 - PC20

public export
inc20_const : TQty
inc20_const = (  1/(1+one5) ) * one5

public export
taxRatio : TaxCode -> TQty
taxRatio ZeroVAT = 0
taxRatio INC20 = inc20_const
taxRatio EX20 = one5


||| Hom2
public export
get_line : Line -> Product2
get_line l =
   let h1=LEHom1 (qty l)
       pk2 = MkProdK2 (sku l) (currency l)
       p = LEMul (price_unit l) UnitPrice h1
       d = LEMul (discount l) Discount p 
       t = LETaxCode (tax_code l) d in (pk2,t)


public export
get_hom1_TQty : LineTerm -> TQty
get_hom1_TQty (LEHom1 qty) = qty
get_hom1_TQty (LETaxCode tc l) = (get_hom1_TQty l)
get_hom1_TQty (LEAdd l1 l2) = (get_hom1_TQty l1) + (get_hom1_TQty l2)
get_hom1_TQty (LEMul u mu l) = get_hom1_TQty l

{-
public export
get_hom2_TQty : LineTerm -> TQty 
get_hom2_TQty (LEHom1 qty) = 1
get_hom2_TQty (LETaxCode tc l) = (get_hom2_TQty l)
get_hom2_TQty (LEAdd l1 l2) = (get_hom2_TQty l1) + (get_hom2_TQty l2)
get_hom2_TQty (LEMul u mu l) = (get_hom2_TQty l) * u
-}

public export
get_hom2_Mul : LineTerm -> List (LineTermMultType,TQty)
get_hom2_Mul (LEHom1 qty) = []
get_hom2_Mul (LETaxCode taxcode x) = get_hom2_Mul x
get_hom2_Mul (LEAdd l1 l2) = (get_hom2_Mul l1) ++ (get_hom2_Mul l2)
get_hom2_Mul (LEMul u mu l)= [(mu,u)] ++ (get_hom2_Mul l)

public export
get_hom2_TQty : LineTerm -> TQty 
get_hom2_TQty l = foldl (*) 1 [ v | (k,v) <- get_hom2_Mul l]

--public export
--get_hom2
get_tax_codes : LineTerm -> List TaxCode
get_tax_codes (LEHom1 qty) = []
get_tax_codes (LETaxCode taxcode x) = [taxcode]++(get_tax_codes x)
get_tax_codes (LEAdd l1 l2) = (get_tax_codes l1)++(get_tax_codes l2)
get_tax_codes (LEMul u mu l) = (get_tax_codes l)

get_tc_prodkey : List TaxCode -> ProdKey
get_tc_prodkey xs = (concat [(show x) | x <- xs] )

eqLineTerm_TaxMult : LineTerm -> LineTerm -> Bool
eqLineTerm_TaxMult l1 l2 = 
          let l1_tc = sort $ get_tax_codes l1
              l2_tc = sort $ get_tax_codes l2
              l1_h2 = sort $ get_hom2_Mul l1
              l2_h2 = sort $ get_hom2_Mul l2 in ( (l1_tc==l2_tc) && (l1_h2==l2_h2) )

eqLineTerm_TaxTQty : LineTerm -> LineTerm -> Bool
eqLineTerm_TaxTQty l1 l2 = 
          let l1_tc = sort $ get_tax_codes l1
              l2_tc = sort $ get_tax_codes l2
              l1_h2 = get_hom2_TQty l1
              l2_h2 = get_hom2_TQty l2 in ( (l1_tc==l2_tc) && (l1_h2==l2_h2) )

fromMaybeTQty : Maybe TQty -> TQty
fromMaybeTQty Nothing = 0
fromMaybeTQty (Just x) = x

fromProduct2 : Product2 -> LineExt
fromProduct2 (p2,lt) =
          let lt_tc = get_tax_codes lt
              lt_h2 = get_hom2_Mul lt
              lt_map = Data.SortedMap.fromList lt_h2 --TBD: Merge TQty by LineTermMultType
              p_u = fromMaybeTQty $ Data.SortedMap.lookup UnitPrice lt_map
              disc = fromMaybeTQty $ Data.SortedMap.lookup Discount lt_map
              qty = get_hom1_TQty lt
              l = MkLineExt (keyfrom p2) qty (keyto p2) p_u disc lt_tc in l

replaceHom1 : LineTerm -> TQty -> LineTerm
replaceHom1 (LEHom1 qty) y = (LEHom1 y)
replaceHom1 (LETaxCode taxcode x) y = (LETaxCode taxcode (replaceHom1 x y) )
replaceHom1 (LEAdd l1 l2) y = (LEAdd (replaceHom1 l2 y) (replaceHom1 l2 y))
replaceHom1 (LEMul u mu l) y = (LEMul u mu (replaceHom1 l y) )



public export
addLineTerm : LineTerm -> LineTerm -> LineTerm
addLineTerm x y = 
       let q1 = get_hom1_TQty x
           q2 = get_hom1_TQty y
           q = q1+q2
           l1 = LEMul (q1/q) MultQty x
           l2 = LEMul (q2/q) MultQty y
           l = if (eqLineTerm_TaxTQty x y) then replaceHom1 x q else LEAdd l1 l2 in l


merge_item_into2 : (SortedMap ProdKey2 LineTerm) -> (ProdKey2, LineTerm) -> (SortedMap ProdKey2 LineTerm)
merge_item_into2 acc x = mergeWith (addLineTerm) acc (fromList [x])

fromProduct2List : Hom2 -> SortedMap ProdKey2 LineTerm
fromProduct2List xs = foldl merge_item_into2 empty xs

public export
evalProduct2List : Hom2 -> Hom2
evalProduct2List xs = toList $ fromProduct2List xs 




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
