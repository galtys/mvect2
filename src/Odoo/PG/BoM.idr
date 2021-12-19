module Odoo.PG.BoM

import Data.SortedMap

import Category.Transaction.Qty
import Category.Transaction.Types
--import Category.Transaction.Hom
--import Category.Transaction.Journal
--import Category.Transaction.Demo
--import Category.Transaction.Types
--import Category.Transaction.Types2
import Data.Ratio
--import Odoo.PG.BoM
import Odoo.Schema.PJBRecDef
--import Generics.Derive

--%language ElabReflection

ret_spaces : Bits32 -> String
ret_spaces x = if x==0 then "" else concat [ "  " | u<- [0..x]]

export
mult_BoM32 : EQty -> List BoM32 -> List BoM32
mult_BoM32 x [] = []
mult_BoM32 x ((Node32 qty sku components) :: xs) = 
    let ch = mult_BoM32 (x*qty) components
        n = Node32 (x*qty) sku ch in [n] ++ (mult_BoM32 x xs)
export
variants_BoM32 : List BoM32 -> Hom1 --List (ProdKey, EQty)
variants_BoM32 [] = []
variants_BoM32 ((Node32 qty sku []) :: xs) = [(sku,qty)] ++ (variants_BoM32 xs)
variants_BoM32 ((Node32 qty sku c) :: xs) = (variants_BoM32 c)++(variants_BoM32 xs)

export
print_BoM32 : Bits32 -> List BoM32 -> List String
print_BoM32 i [] = []
print_BoM32 i (b32@(Node32 qty sku components) :: xs) = 
      
      let ua = ( (ret_spaces i) ++ (show qty) ++ "," ++ (show sku) )
          uc = print_BoM32 (i+1) components
          uxs = print_BoM32 i xs in [ua]++uc++uxs
export      
print_list : HasIO io => List String -> io ()
print_list [] = pure ()
print_list (x::xs) = do
  --printLn x
  putStrLn x
  print_list xs
export
toBoM_map : List BrowseBoM.RecordModel -> SortedMap ProdKey (List BrowseBoM.RecordModel)
toBoM_map [] = empty
toBoM_map ((MkRecordModel pk product_qty bom_id bom_lines product_id) :: xs) = insert (PK32 DX product_id) bom_lines (toBoM_map xs)
export
toProduct_map : List BrowseProduct.RecordModel -> SortedMap ProdKey BrowseProduct.RecordModel
toProduct_map [] = empty
toProduct_map (p@(MkRecordModel pk product_tmpl_id trade retail contract default_code) :: xs) = insert (PK32 DX pk) p (toProduct_map xs)
--toProduct_map [] = empty
export
rbom_to_list : Maybe (List BrowseBoM.RecordModel) -> Hom1 --List (ProdKey,EQty)
rbom_to_list Nothing = []
rbom_to_list (Just x) = [ (PK32 DX $product_id u,product_qty u) | u<-x]
export    
map_to_BoM32 : Hom1 -> SortedMap ProdKey (List BrowseBoM.RecordModel) -> List BoM32 --List (ProdKey,EQty)
map_to_BoM32 [] m = []
map_to_BoM32 (muf@(p_id,qty)::xs) m = 
  let ch = rbom_to_list $ lookup p_id m  
      bom32_ch = map_to_BoM32 ch m
      q = qty
      node = Node32 ( q) p_id bom32_ch in [node]++(map_to_BoM32 xs m)
