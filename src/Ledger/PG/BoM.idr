module Ledger.PG.BoM

import Data.SortedMap

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Hom
import Category.Transaction.Journal
import Category.Transaction.Demo
import Category.Transaction.Types
import Data.Ratio
--import Data.Zippable
import JSON

import Generics.Derive
import JSON

import Control.Monad.Either

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types

%language ElabReflection

record RBoM where
  constructor MkRBoM
  product_id : Bits32
  product_qty : TQty
  bom_id : (Maybe Bits32)
  pk : Bits32
        
%runElab derive "RBoM" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]

record RProduct where
  constructor MkRProduct
  pk : Bits32
  sku : String  
  name : String
  list_price : Maybe Price
  trade : Maybe Price
  retail : Maybe Price
  contract : Maybe Price

%runElab derive "RProduct" [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]
--- Join


--------------------------------------------------------------------------------
--          Product and Bom
--------------------------------------------------------------------------------
PT : String
PT = "product_template"

PP : String
PP = "product_product"

BM : String
BM = "mrp_bom"

-- Product Template Table
Id_PT : Column
Id_PT = primarySerial64 Bits32 "id" (Just . cast) PT

Name : Column
Name = notNull String "name" Text Just id PT

ListPrice : Column
ListPrice = nullable Price "list_price" DoublePrecision (Just . toINC20) cast PT

ProductTemplate_NP : Table
ProductTemplate_NP = MkTable PT
         [Id_PT, Name, ListPrice]

-- Product table
Id_PP : Column
Id_PP = primarySerial64 Bits32 "id" (Just . cast) PP

ProductTmplID : Column
ProductTmplID = notNull Bits32 "product_tmpl_id" BigInt (Just . cast) cast PP

TradePrice : Column
TradePrice = nullable Price "trade" DoublePrecision (Just . toEX20) cast PP

RetailPrice : Column
RetailPrice = nullable Price "retail" DoublePrecision (Just . toINC20) cast PP

ContractPrice : Column
ContractPrice = nullable Price "contract" DoublePrecision (Just . toEX20) cast PP

SKU  : Column
SKU = notNull String "default_code" Text Just id PP

Product_NP : Table
Product_NP = MkTable PP
         [Id_PP,ProductTmplID, SKU, TradePrice,RetailPrice,ContractPrice]

-- BoM table
Id_BM : Column
Id_BM = primarySerial64 Bits32 "id" (Just . cast) BM

ProdQty : Column
ProdQty = notNull TQty "product_qty" DoublePrecision (Just . cast) cast BM

ProductID : Column
ProductID = notNull Bits32 "product_id" BigInt (Just . cast) cast BM

BomID : Column
BomID = nullable Bits32 "bom_id" BigInt (Just . cast) cast BM

BoM_NP : Table
BoM_NP = MkTable "mrp_bom"
      [ProductID,ProdQty,BomID,Id_BM]
      
ListProdCols : List Column
ListProdCols = [Id_PP, SKU,Name,ListPrice, TradePrice, RetailPrice, ContractPrice]

Prod_NP_R : Table
Prod_NP_R = MkTable "x"
            ListProdCols

public export
0 GetRSOP : List Column -> Type
GetRSOP cs = SOP I [(GetTypes cs)]

toRBoM : GetRow (columns BoM_NP) -> RBoM
toRBoM =  toRBoMx . tosop5 where
  tosop5 : GetRow (columns BoM_NP) -> GetRSOP (columns BoM_NP)
  tosop5 x = MkSOP $ Z x

  toRBoMx : GetRSOP (columns BoM_NP) -> RBoM
  toRBoMx = to


toRProduct : GetRow ListProdCols -> RProduct
toRProduct x = 
   let lp = (get (Maybe Price) (tl (tl (tl x))))
       tp = (get (Maybe Price) (tl (tl (tl (tl x)))))
       rp = (get (Maybe Price) (tl (tl (tl (tl (tl x))))))
       cp = (get (Maybe Price) (tl (tl (tl (tl (tl (tl x)))))))
       xn =(get String (tl (tl x)))
       prod =MkRProduct (get Bits32 x) (get String (tl x)) xn lp tp rp cp in prod

rbom2bom32  : RBoM -> (List BoM32) -> BoM32
rbom2bom32 (MkRBoM product_id product_qty bom_id pk) xs = let 
   qty = (cast product_qty) in Node32 ( qty) product_id xs

chmap2bom32 : (List (RBoM, List RBoM) ) -> List BoM32 -> List BoM32
chmap2bom32 [] chld= []
chmap2bom32 ((x, y) :: xs) chld = 
   let ch=[ (rbom2bom32 ox chld) | ox <- y]
       b=rbom2bom32 x ch in [b]++chmap2bom32 xs chld


child_map_RBoM : (List (RBoM, List RBoM) ) ->  SortedMap Bits32 (List RBoM)
child_map_RBoM [] = empty
child_map_RBoM (( (MkRBoM product_id product_qty bom_id pk), y) :: xs) = insert product_id y (child_map_RBoM xs)

safeHead : List x -> Maybe x
safeHead [] = Nothing
safeHead (y :: xs) = Just y

rbom_to_list : Maybe (List RBoM) -> List (TQty,Bits32)
rbom_to_list Nothing = []
rbom_to_list (Just x) = [ (product_qty u,product_id u) | u<-x]

ret_spaces : Bits32 -> String
ret_spaces x = if x==0 then "" else concat [ "  " | u<- [0..x]]

print_ch : HasIO io =>  Bits32 -> Bits32 -> SortedMap Bits32 (List RBoM) -> io()
print_ch i p_id m = do
  printLn ( (ret_spaces i) ++(show p_id)++":"++(show $ rbom_to_list $ lookup p_id m))

print_ch_r : HasIO io =>  Bits32 -> List (TQty,Bits32) -> SortedMap Bits32 (List RBoM) -> io ()
print_ch_r i [] m = pure ()
print_ch_r i (muf@(qty,p_id)::xs) m = do
  let ch = rbom_to_list $ lookup p_id m
  
  printLn ( (ret_spaces i) ++(show p_id)++":"++(show ch ))
  print_ch_r (i+1) ch m
  
  print_ch_r (i) xs m

ch_map_to_BoM32 : List (TQty,Bits32) -> SortedMap Bits32 (List RBoM) -> List BoM32
ch_map_to_BoM32 [] m = []
ch_map_to_BoM32 (muf@(qty,p_id)::xs) m = 
  let ch = rbom_to_list $ lookup p_id m
      bom32_ch = ch_map_to_BoM32 ch m
      q = qty
      node = Node32 ( q) p_id bom32_ch in [node]++(ch_map_to_BoM32 xs m)

mult_BoM32 : TQty -> List BoM32 -> List BoM32
mult_BoM32 x [] = []
mult_BoM32 x ((Node32 qty sku components) :: xs) = 
    let ch = mult_BoM32 (x*qty) components
        n = Node32 (x*qty) sku ch in [n] ++ (mult_BoM32 x xs)

variants_BoM32 : List BoM32 -> List (TQty,Bits32)
variants_BoM32 [] = []
variants_BoM32 ((Node32 qty sku []) :: xs) = [(qty,sku)] ++ (variants_BoM32 xs)
variants_BoM32 ((Node32 qty sku c) :: xs) = (variants_BoM32 c)++(variants_BoM32 xs)


print_BoM32 : Bits32 -> List BoM32 -> List String
print_BoM32 i [] = []
print_BoM32 i (b32@(Node32 qty sku components) :: xs) = 
      
      let ua = ( (ret_spaces i) ++ (show qty) ++ "," ++ (show sku) )
          uc = print_BoM32 (i+1) components
          uxs = print_BoM32 i xs in [ua]++uc++uxs
      
print_list : HasIO io => List String -> io ()
print_list [] = pure ()
print_list (x::xs) = do
  --printLn x
  putStrLn x
  print_list xs
  
read_root_boms : HasIO io => MonadError SQLError io => Connection -> io (List RBoM) 
read_root_boms c  = do
  child_rows <- get c BoM_NP (columns BoM_NP) (IsNull BomID)
  let child_rbom = [ toRBoM ox | ox <- child_rows ]
  pure child_rbom
                          
read_boms_ : HasIO io => MonadError SQLError io => Connection -> (List RBoM) ->  io (List (RBoM, List RBoM) )
read_boms_ c [] = pure []
read_boms_ c (x@(MkRBoM product_id product_qty b_id pk) :: xs) = do

  child_rows <- get c BoM_NP (columns BoM_NP) (BomID == Just (cast pk ) )  
  let child_rbom = [ toRBoM ox | ox <- child_rows ]
  let ret = ((x,child_rbom))
  xs <- read_boms_ c xs
  pure ([ret]++xs) 

read_product_templates : HasIO io => MonadError SQLError io => Connection -> io ()
read_product_templates c = do
  rows <- get c ProductTemplate_NP (columns ProductTemplate_NP) (Name == "test product") --(True)  
  printLn ( rows)
  printLn (length rows)
  rows2 <- getJoin c ProductTemplate_NP Product_NP ListProdCols (JC ProductTmplID Id_PT)
  
  let rprod = [toRProduct ox | ox <- rows2 ]
  traverse_ printLn rprod
  pure ()
  
main_read_bom : HasIO io => MonadError SQLError io => io (List (RBoM, List RBoM) )
main_read_bom  = do
  c    <- connect "postgresql://jan@localhost:5432/pjb-2021-10-27_1238"  
  
  read_product_templates c
  
  
  boms <- read_root_boms c
  let root_p_ids = [ (product_qty u,product_id u) | u <- boms]
  --printLn (length boms)
  l1 <- read_boms_ c boms
  --let l2 =  chmap2bom32 l1 []
  let m1 = child_map_RBoM l1
  

  --rows <- get c BoM_NP [Id] (IsNull BomID)  
  --b_ids <- prods_to_bom_ids c [p_id]
  --boms <- read_bom_p_id2 c b_ids
  
  --print_ch_r 0  m1
  let qp = [(1,3303)]
  let m32x = ch_map_to_BoM32 qp m1
  let m32 = ch_map_to_BoM32 root_p_ids m1
  print_list $ print_BoM32 0 m32x
  let qp_mult = mult_BoM32 1 m32x 
  print_list $ print_BoM32 0 qp_mult
  let vr =  variants_BoM32 qp_mult
  let vr_qty = [ (fst x) | x<-vr ]
  let sum_vr = sum vr_qty
  printLn vr_qty
  --printLn sum_vr
  let r_sum_vr = (recip sum_vr)
  let m32_r = mult_BoM32 r_sum_vr m32x
  
  print_list $ print_BoM32 0 m32_r
  
  let r_vr =  variants_BoM32 m32_r
  let r_vr_qty = [ (fst x) | x<-r_vr ]
  let vr2_qty = [ x*r_sum_vr | x <- vr_qty]
  printLn vr_qty  
  printLn vr2_qty
  printLn $ sum vr2_qty
  finish c
  pure l1
  
  
export  
main_3 : IO (List (RBoM, List RBoM) )
main_3 = do Left err <- runEitherT (main_read_bom {io = EitherT SQLError IO} )
              | Right l1 => pure l1
            printLn err
            pure []

export
muf_3 : HasIO io => io (List (RBoM, List RBoM) )
muf_3 = do  
     l1 <- (liftIO main_3)
     pure l1
