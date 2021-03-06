module Category.Transaction.Hom

import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import JSON
import Data.List

import Category.Transaction.Qty
import Category.Transaction.Types
--import Category.Transaction.Types2
import Data.Ratio
import Odoo.Schema.PJBRecDef

--import Category.PG.Order

%language ElabReflection
%ambiguity_depth 10
{-
public export
get_hom1 : Term -> List Hom1
get_hom1 (Ref j1) = []
get_hom1 (Ch j1  h1) = [h1]
get_hom1 (Pro j1 t1 t2) = (get_hom1 t1) ++ (get_hom1 t2)
get_hom1 (Co j1 t1 t2) = (get_hom1 t1) ++ (get_hom1 t2)
-}

||| Hom1

{-
unEQty : EQty -> EQty -> EQty 
unEQty (Debit a) (Debit b) = Debit (min a b)
unEQty (Credit a) (Credit b) = Credit (min a b)
unEQty (Debit a) (Credit b) = Debit 0
unEQty (Credit a) (Debit b) = Debit 0
-}
unEQty : EQty -> EQty -> EQty
unEQty x y = (min x y)
partial
public export
merge_as_union : (SortedMap ProdKey EQty) -> Product -> (SortedMap ProdKey EQty)
merge_as_union acc x = case (lookup (fst x) acc) of
                             Nothing => acc
                             Just v => case (unEQty v (snd x) ) of 
                                           0 => (delete (fst x) acc)
                                           nv => (insert (fst x) nv acc)                                           


merge_item_into : (SortedMap ProdKey EQty) -> (ProdKey, EQty) -> (SortedMap ProdKey EQty)
merge_item_into acc x = mergeWith (+) acc (fromList [x])

fromProductList : Hom1 -> SortedMap ProdKey EQty
fromProductList xs = foldl merge_item_into empty xs

public export
evalProductList : Hom1 -> Hom1
evalProductList xs = toList $ fromProductList xs 

public export 
unionHom1' : Hom1 -> Hom1 -> SortedMap ProdKey EQty
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
multHom1 : Hom1 -> Hom1 -> Hom1
multHom1 x y = [] -- will be apply

public export
evDiffHom1 : Hom1 -> Hom1 -> Hom1
evDiffHom1 x y = (evalHom1 (diffHom1 x y))

public export
eqHom1 : Hom1 -> Hom1 -> Bool
eqHom1 x y = (evDiffHom1 x y) == id_hom1


public export
Num Hom1 where
   (+) x y = evalProductList $ addHom1 x y
   (*) = multHom1 -- tbd?
   fromInteger x = ProdKey.pkFromInteger x

public export
Neg Hom1 where
   (-) = evDiffHom1
   negate = invHom1

||| Tax
public export
PC20 : EQty
PC20 = (percent 20)

public export
one5 : EQty
one5 = 1 - PC20

public export
inc20_const : EQty
inc20_const = (  1/(1+one5) ) * one5

public export
taxRatio : TaxCode -> EQty
--taxRatio ZeroVAT = 0
taxRatio INC20 = inc20_const
taxRatio EX20 = one5
taxRatio TAXAMOUNT = 0
taxRatio ERROR = 0

public export  
applyHom2 : Hom2 -> Hom1 -> Hom1
applyHom2 h2 p = ret where
  {-
  kp : List ProdKey
  kp = map fst p
  -}
  ret1 : List (Maybe Product,EQty)
  ret1 = [ (lookup (fst x) h2, snd x) | x <- p ]
  
  ev_ret1 : List (Maybe Product,EQty) -> Hom1
  ev_ret1 [] = []
  ev_ret1 ((Nothing,q)::xs) = (ev_ret1 xs)
  ev_ret1 ((Just x,q)::xs) = [ (fst x, (snd x)*q) ]++(ev_ret1 xs)
  
  ret : Hom1
  ret = (ev_ret1 ret1)

--export
--toHom11 (MkH121 dx bom appl cx h11)

export
toHom12 : Hom121 -> Hom12
toHom12 (MkH121 dx bom appl cx h11) = (MkHom12 dx appl bom) 
{-
export
toHom1 : Hom11 -> Hom1
toHom1 (MkH11 dx cx) = (dx+cx)
-}
public export
toQLine : Hom12 -> HomQLine
toQLine (MkHom12 dx appl bom) = ret where
  test_bom : Maybe BoM32 -> Maybe BoM32
  test_bom Nothing = Nothing
  test_bom (Just (Node32 qty sku [])) = Nothing
  test_bom bom@(Just (Node32 qty sku (xs))) = bom

  bom_m : List BoM32 -> List (ProdKey, BoM32) 
  bom_m  [] = []
  bom_m ((Node32 qty sku components) :: xs) = [(sku, (Node32 qty sku components))]++(bom_m xs)

  ret1 : List (ProdKey, EQty,Maybe Product, Maybe BoM32)
  ret1 = [ (fst x, snd x, lookup (fst x) appl, lookup (fst x) (bom_m bom) )| x <- dx ]
     
  retA : List (ProdKey, EQty,Maybe Product,Maybe BoM32) -> HomQLine
  retA [] = []
  retA ((dx, (q, (Nothing, b))) :: xs) = retA xs
  retA ((dx, (q, ((Just cx), b))) :: xs) = [MkQL dx (test_bom b) q (fst cx) (snd cx)]++(retA xs)
  {-
  retA [] = []
  retA ((x, (y, Nothing)) :: xs) = retA xs
  retA ((dx, (q, (Just z))) :: xs) = [MkQL dx Nothing q (fst z) (snd z)]++(retA xs)
  -}
  ret : HomQLine
  ret = retA ret1

public export
colimQLine : HomQLine -> HomQLine --List (List QLine)
colimQLine xs_in = concat ret6 where
   eqx : QLine -> QLine -> Bool
   eqx a b = ((dxpk a)==(dxpk b)) && ((cxpk a)==(cxpk b))
   eqp : QLine -> QLine -> Bool
   eqp a b = ((price a)==(price b))   
   
   ret : List (List1 QLine)
   ret = groupBy eqx (sortBy (\a,b=> compare (dxpk a,cxpk a,price a) (dxpk b,cxpk a,price b) ) xs_in)
   
   ret2 : List (List QLine)
   ret2 = map toList ret
   {-   
   ret3 : List (List (List QLine))
   ret3 = map (  (map init) . (groupBy eqp) ) ret2
   -}
   muf1 : List1 QLine -> QLine
   muf1 xs = addq (head xs) c where
      c : EQty
      c = sum (map q (tail xs))
      addq : QLine -> EQty -> QLine
      addq (MkQL dxpk bom q cxpk price) y = (MkQL dxpk bom (q+y) cxpk price)
        
   ret4 : List QLine -> List (Bits8, QLine)
   ret4 xs = [ (cast x,y) | (x,y) <- (zip [1..(length xs)] xs)]
   
   add8 : (Bits8,QLine) -> QLine
   add8 (y, (MkQL dxpk bom q cxpk price)) = (MkQL (ProdKey.addBits8 dxpk y) bom q (ProdKey.addBits8 cxpk y) price)   
   
   ret5 : List (Bits8, QLine) -> List QLine
   ret5 [] = []
   ret5 lx@(x::xs) = case (length lx) of 
         1 => [snd x]
         _ => map add8 lx
         
   ret3 : List (List QLine)
   ret3 = map (  (map muf1) . (groupBy eqp) ) ret2
   
   ret6 : List (List QLine)
   ret6 = map (  ret5 . ret4  ) ret3
      
public export  
applyHom2Tax : Hom2 -> Hom1 -> Hom1
applyHom2Tax h2 p = ret where
  {-
  kp : List ProdKey
  kp = map fst p
  -}
  ret1 : List (Maybe Product,EQty)
  ret1 = [ (lookup (fst x) h2, snd x) | x <- p ]
  
  
  
  ev_ret1 : List (Maybe Product,EQty) -> Hom1
  ev_ret1 [] = []
  ev_ret1 ((Nothing,q)::xs) = (ev_ret1 xs)
  ev_ret1 ((Just x,q)::xs) = [ (toTaxAmountKey $ fst x,(taxRatio $ taxCodeFromKey $ fst x)*(snd x)*q) ]++(ev_ret1 xs)
  
  ret : Hom1
  ret = (ev_ret1 ret1)


export
addHom11 : Hom11 -> Hom11 -> Hom11
addHom11 (MkH11 dx cx) (MkH11 xs ys) = MkH11 (dx+xs) (cx+ys)

export
diffHom11 : Hom11 -> Hom11 -> Hom11
diffHom11 (MkH11 dx cx) (MkH11 xs ys) = MkH11 (dx-xs) (cx-ys)

export
fromIntegerHom11 : Integer -> Hom11
fromIntegerHom11 x = MkH11 (fromInteger x) []

negateHom11 : Hom11 -> Hom11
negateHom11 (MkH11 dx cx) = MkH11 (invHom1 dx) (invHom1 cx)

multHom11 : Hom11 -> Hom11 -> Hom11
multHom11 x y = MkH11 [] []

export
toHom1 : Hom11 -> Hom1
toHom1 (MkH11 dx cx) = dx++cx

export
dropDX : Hom11 -> Hom11
dropDX (MkH11 dx cx) = (MkH11 [] cx)

export
dropCX : Hom11 -> Hom11
dropCX (MkH11 dx cx) = (MkH11 dx [])

export
justDX : Hom11 -> Hom11
justDX (MkH11 dx cx) = (MkH11 dx [])

export
justCX : Hom11 -> Hom11
justCX (MkH11 dx cx) = (MkH11 [] cx)

export
isDx : Hom11 -> Bool
isDx (MkH11 dx []) = True
isDx (MkH11 dx (x :: xs)) = False

export
toDxCx : Hom11 -> DxCx
toDxCx (MkH11 dx []) = DX
toDxCx (MkH11 [] cx) = CX
toDxCx (MkH11 dx cx) = DX

export
isCx : Hom11 -> Bool
isCx (MkH11 [] cx) = True
isCx (MkH11 (x :: xs) cx) = False

export
filterZero : Hom1 -> Hom1
filterZero [] = []
filterZero ((x, y) :: xs) = if (y==0) then (filterZero xs) else [(x,y)]++(filterZero xs)

export
evalHom11 : Hom11 -> Hom11
evalHom11 (MkH11 dx cx) = MkH11 (filterZero $ evalHom1 dx) (filterZero $evalHom1 cx)

public export
Num Hom11 where
   (+) = addHom11
   (*) = multHom11
   fromInteger = fromIntegerHom11

export
sumHom11 : List Hom11 -> Hom11
sumHom11 xs = evalHom11 ret where   
   ret : Hom11
   ret = foldl (\x,y => (x+y) ) emptyHom11 xs

public export
Neg Hom11 where
   (-) = diffHom11
   negate = negateHom11
   
export
fromMaybeEQty : Maybe EQty -> EQty
fromMaybeEQty Nothing = 0
fromMaybeEQty (Just x) = x
export
fromOrderTax2 : PrimOrderTax.RecordModel -> TaxCode
fromOrderTax2 (MkRecordModel pk name Nothing amount type price_include) = ERROR
fromOrderTax2 (MkRecordModel pk name (Just x) amount type price_include) = case x of
       "STI20" => INC20
       "STE20" => EX20
       _       => ERROR
export
getTax : List PrimOrderTax.RecordModel -> TaxCode
getTax [] = ERROR
getTax (x::xs) = (fromOrderTax2 x)


export              
priceFromOrderLine : List BrowseOrderLine.RecordModel -> Hom2 --List (ProdKey, Currency)
priceFromOrderLine [] = []
priceFromOrderLine ((MkRecordModel pk price_unit product_uom_qty discount delivery_line order_id product_id tax_ids) :: xs) = ret where
           val : EQty
           val = (fromMaybeEQty discount)*price_unit
           ret : Hom2
           ret =case product_id of 
                  Nothing => (priceFromOrderLine xs)
                  Just p_id => [ (PK32 DX pk One, (PKPrice CX GBP (getTax tax_ids) One,val) ) ] ++ (priceFromOrderLine xs)
        
export  
priceFromStockMove : TaxCode -> List BrowseStockMove.RecordModel -> Hom2
priceFromStockMove tc [] = []
priceFromStockMove tax_code ((MkRecordModel pk origin price_unit product_qty product_id location_id location_dest_id picking_id purchase_line_id sale_line_id state) :: xs) = [ (PK32 DX product_id ProdKey.One, (PKPrice CX GBP tax_code ProdKey.One,fromMaybeEQty price_unit ) ) ] ++ (priceFromStockMove tax_code xs)

export
fromAccountVoucher : List BrowseAccountVoucher.RecordModel -> Hom1
fromAccountVoucher [] = []
fromAccountVoucher ((MkRecordModel pk number partner_id journal_id amount) :: xs) = [("GBP",amount)]++(fromAccountVoucher xs)

export
qtyFromOrderLine : List BrowseOrderLine.RecordModel -> Hom1 --List (ProdKey,EQty)
qtyFromOrderLine [] = []
qtyFromOrderLine ((MkRecordModel pk price_unit product_uom_qty discount delivery_line order_id product_id tax_ids) :: xs) = 
           case product_id of 
              Nothing => [ (PKUser DX "missing" One,product_uom_qty) ] ++ (qtyFromOrderLine xs)
              Just p_id => [ (PK32 DX p_id One, product_uom_qty) ] ++ (qtyFromOrderLine xs)
              --Just p_id => [ (PK32 DX pk, product_uom_qty) ] ++ (qtyFromOrderLine xs)
export
fromStockMove : List BrowseStockMove.RecordModel -> Hom1 --List (ProdKey,EQty)
fromStockMove [] = []
fromStockMove ((MkRecordModel pk origin price_unit product_qty product_id location_id location_dest_id picking_id purchase_line_id sale_line_id state) :: xs) = [ (PK32 DX product_id ProdKey.One,product_qty) ] ++ (fromStockMove xs)

export
getCxINC20 : Hom1 -> Hom1
getCxINC20 [] = []
getCxINC20 (((PKCy x z v), y) :: xs) = getCxINC20 xs
getCxINC20 (((PKUser x z v), y) :: xs) = getCxINC20 xs
getCxINC20 (((PK32 x z v), y) :: xs) = getCxINC20 xs
getCxINC20 (((PKPrice DX z w v), y) :: xs) = getCxINC20 xs
getCxINC20 (((PKPrice CX z INC20 v), y) :: xs) = [(PKPrice CX z INC20 v,y)] ++ getCxINC20 xs
getCxINC20 (((PKPrice CX z EX20 v), y) :: xs) = getCxINC20 xs
getCxINC20 (((PKPrice CX z TAXAMOUNT v), y) :: xs) = getCxINC20 xs
getCxINC20 (((PKPrice CX z ERROR v), y) :: xs) = getCxINC20 xs
getCxINC20 (((FromInteger x v), y) :: xs) = getCxINC20 xs

export
getCxEX20 : Hom1 -> Hom1
getCxEX20 [] = []
getCxEX20 (((PKCy x z v), y) :: xs) = getCxEX20 xs
getCxEX20 (((PKUser x z v), y) :: xs) = getCxEX20 xs
getCxEX20 (((PK32 x z v), y) :: xs) = getCxEX20 xs
getCxEX20 (((PKPrice DX z w v), y) :: xs) = getCxEX20 xs
getCxEX20 (((PKPrice CX z INC20 v), y) :: xs) = getCxEX20 xs
getCxEX20 (((PKPrice CX z EX20 v), y) :: xs) = [(PKPrice CX z EX20 v,y)] ++getCxEX20 xs
getCxEX20 (((PKPrice CX z TAXAMOUNT v), y) :: xs) = getCxEX20 xs
getCxEX20 (((PKPrice CX z ERROR v), y) :: xs) = getCxEX20 xs
getCxEX20 (((FromInteger x v), y) :: xs) = getCxEX20 xs


{-
public export
get_line : Line -> Product2
get_line l =
   let h1=LEHom1 (qty l)
       pk2 = MkProdK2 (sku l) (currency l)
       p = LEMul (price_unit l) UnitPrice h1
       d = LEMul (discount l) Discount p 
       t = LETaxCode (tax_code l) d in (pk2,t)
-}
{-
public export
get_hom1_EQty : LineTerm -> EQty
get_hom1_EQty (LEHom1 qty) = qty
get_hom1_EQty (LETaxCode tc l) = (get_hom1_EQty l)
get_hom1_EQty (LEAdd l1 l2) = (get_hom1_EQty l1) + (get_hom1_EQty l2)
get_hom1_EQty (LEMul u mu l) = get_hom1_EQty l


public export
get_hom2_EQty : LineTerm -> EQty 
get_hom2_EQty (LEHom1 qty) = 1
get_hom2_EQty (LETaxCode tc l) = (get_hom2_EQty l)
get_hom2_EQty (LEAdd l1 l2) = (get_hom2_EQty l1) + (get_hom2_EQty l2)
get_hom2_EQty (LEMul u mu l) = (get_hom2_EQty l) * u


public export
get_hom2_Mul : LineTerm -> List (LineTermMultType,EQty)
get_hom2_Mul (LEHom1 qty) = []
get_hom2_Mul (LETaxCode taxcode x) = get_hom2_Mul x
get_hom2_Mul (LEAdd l1 l2) = (get_hom2_Mul l1) ++ (get_hom2_Mul l2)
get_hom2_Mul (LEMul u mu l)= [(mu,u)] ++ (get_hom2_Mul l)

public export
get_hom2_EQty : LineTerm -> EQty 
get_hom2_EQty l = foldl (*) 1 [ v | (k,v) <- get_hom2_Mul l]

--public export
--get_hom2
get_tax_codes : LineTerm -> List TaxCode
get_tax_codes (LEHom1 qty) = []
get_tax_codes (LETaxCode taxcode x) = [taxcode]++(get_tax_codes x)
get_tax_codes (LEAdd l1 l2) = (get_tax_codes l1)++(get_tax_codes l2)
get_tax_codes (LEMul u mu l) = (get_tax_codes l)
-}
{-
eqLineTerm_TaxMult : LineTerm -> LineTerm -> Bool
eqLineTerm_TaxMult l1 l2 = 
          let l1_tc = sort $ get_tax_codes l1
              l2_tc = sort $ get_tax_codes l2
              l1_h2 = sort $ get_hom2_Mul l1
              l2_h2 = sort $ get_hom2_Mul l2 in ( (l1_tc==l2_tc) && (l1_h2==l2_h2) )

eqLineTerm_TaxEQty : LineTerm -> LineTerm -> Bool
eqLineTerm_TaxEQty l1 l2 = 
          let l1_tc = sort $ get_tax_codes l1
              l2_tc = sort $ get_tax_codes l2
              l1_h2 = get_hom2_EQty l1
              l2_h2 = get_hom2_EQty l2 in ( (l1_tc==l2_tc) && (l1_h2==l2_h2) )
-}
{-
public export
fromProduct2 : Product2 -> LineExt
fromProduct2 (p2,lt) =
          let lt_tc = get_tax_codes lt
              lt_h2 = get_hom2_Mul lt
              lt_map = Data.SortedMap.fromList lt_h2 --TBD: Merge EQty by LineTermMultType
              p_u = fromMaybeEQty $ Data.SortedMap.lookup UnitPrice lt_map
              disc = fromMaybeEQty $ Data.SortedMap.lookup Discount lt_map
              qty = get_hom1_EQty lt
              l = MkLineExt (keyfrom p2) qty (keyto p2) p_u disc lt_tc in l

replaceHom1 : LineTerm -> EQty -> LineTerm
replaceHom1 (LEHom1 qty) y = (LEHom1 y)
replaceHom1 (LETaxCode taxcode x) y = (LETaxCode taxcode (replaceHom1 x y) )
replaceHom1 (LEAdd l1 l2) y = (LEAdd (replaceHom1 l2 y) (replaceHom1 l2 y))
replaceHom1 (LEMul u mu l) y = (LEMul u mu (replaceHom1 l y) )



public export
addLineTerm : LineTerm -> LineTerm -> LineTerm
addLineTerm x y = 
       let q1 = get_hom1_EQty x
           q2 = get_hom1_EQty y
           q = q1+q2
           l1 = LEMul (q1/q) MultQty x
           l2 = LEMul (q2/q) MultQty y
           l = if (eqLineTerm_TaxEQty x y) then replaceHom1 x q else LEAdd l1 l2 in l


merge_item_into2 : (SortedMap ProdKey2 LineTerm) -> (ProdKey2, LineTerm) -> (SortedMap ProdKey2 LineTerm)
merge_item_into2 acc x = mergeWith (addLineTerm) acc (fromList [x])

fromProduct2List : Hom2 -> SortedMap ProdKey2 LineTerm
fromProduct2List xs = foldl merge_item_into2 empty xs

public export
evalProduct2List : Hom2 -> Hom2
evalProduct2List xs = toList $ fromProduct2List xs 

-}


{-
    let xs_map = fromList [(x,0) | x<- xs]
        ret = [ k | (k,v) <- xs_map ] in concat ret       
-}
{-
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

-}

{-
export
confirm_so : OrderEvent ()
confirm_so = do
 let date = "2021-11-01"
     h1 = [p1,p2,p3]
     h2 = [ (fst p1, ("GBP",31.73)), 
            (fst p2, ("GBP",15.03)),
            (fst p3, ("GBP",25.00))]
            
     fx = (MkFx date hilton (MkH121 h1 h2 (apply2' h2 h1) ))
 Confirm (MkO Sale fx)
 Pure ()
-}



