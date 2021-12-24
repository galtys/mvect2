import Libc.Time
import Data.Ratio
import Category.Transaction.Types
import Odoo.Schema.PJBRecDef



make_prod : Bits32->String->EQty->String -> BrowseProduct.RecordModel
make_prod pk_ name_ price sku = MkRecordModel 
          {pk=pk_,
           product_tmpl_id=[MkRecordModel {pk=pk_,name=name_,list_price = Just (PKPrice CX GBP INC20, price)}], 
           trade = Just (PKPrice CX GBP EX20, price*0.7), 
           retail = Just (PKPrice CX GBP INC20, price*1.1), 
           contract = Just (PKPrice CX GBP EX20, price*0.9), 
           default_code =sku }

static_products : List BrowseProduct.RecordModel
static_products = 
  [MkRecordModel { pk = 735, product_tmpl_id = [MkRecordModel { pk = 735, name = "Delivery", list_price = Just (PKPrice CX GBP INC20, 1) }], trade = Just (PKPrice CX GBP EX20, 0), retail = Just (PKPrice CX GBP INC20, 0), contract = Just (PKPrice CX GBP EX20, 0), default_code = "DELIVERY" }, 
   MkRecordModel { pk = 1042, product_tmpl_id = [MkRecordModel { pk = 1042, name = "230cm Mayfair Oval Dining Table with 8 Dining Armchairs", list_price = Just (PKPrice CX GBP INC20, 3199) }], trade = Just (PKPrice CX GBP EX20, 0), retail = Just (PKPrice CX GBP INC20, 3903), contract = Just (PKPrice CX GBP EX20, 0), default_code = "1YWORMOT7_8YWORMDA" }, 
   MkRecordModel { pk = 1064, product_tmpl_id = [MkRecordModel { pk = 1064, name = "2 Mayfair Sun Loungers with Square Side Table", list_price = Just (PKPrice CX GBP INC20, 1799) }], trade = Just (PKPrice CX GBP EX20, 0), retail = Just (PKPrice CX GBP INC20, 1900), contract = Just (PKPrice CX GBP EX20, 0), default_code = "2YWORMSL_1YWORMST" }, 
   MkRecordModel { pk = 4085, product_tmpl_id = [MkRecordModel { pk = 4085, name = "300cm Rectangular Premium Garden Furniture Set cover", list_price = Just (PKPrice CX GBP INC20, 139) }], trade = Just (PKPrice CX GBP EX20, 72), retail = Just (PKPrice CX GBP INC20, 139), contract = Just (PKPrice CX GBP EX20, 86), default_code = "YCOVN1" }, 
   MkRecordModel { pk = 4089, product_tmpl_id = [MkRecordModel { pk = 4089, name = "215cm Single Sunlounger Premium cover", list_price = Just (PKPrice CX GBP INC20, 69) }], trade = Just (PKPrice CX GBP EX20, 36), retail = Just (PKPrice CX GBP INC20, 69), contract = Just (PKPrice CX GBP EX20, 43), default_code = "YCOVN5" }, 
   MkRecordModel { pk = 4095, product_tmpl_id = [MkRecordModel { pk = 4095, name = "65cm Side Table Premium cover ", list_price = Just (PKPrice CX GBP INC20, 49) }], trade = Just (PKPrice CX GBP EX20, 26), retail = Just (PKPrice CX GBP INC20, 49), contract = Just (PKPrice CX GBP EX20, 31), default_code = "YCOVN11" }, 
   MkRecordModel { pk = 735, product_tmpl_id = [MkRecordModel { pk = 735, name = "Delivery", list_price = Just (PKPrice CX GBP INC20, 1) }], trade = Just (PKPrice CX GBP EX20, 0), retail = Just (PKPrice CX GBP INC20, 0), contract = Just (PKPrice CX GBP EX20, 0), default_code = "DELIVERY" }, 
   MkRecordModel { pk = 2932, product_tmpl_id = [MkRecordModel { pk = 2932, name = "Mayfair Dining Armchair (No Cushions)", list_price = Just (PKPrice CX GBP INC20, 211) }], trade = Just (PKPrice CX GBP EX20, 134), retail = Just (PKPrice CX GBP INC20, 258), contract = Just (PKPrice CX GBP EX20, 162), default_code = "YWORMDA-nc" }, 
   MkRecordModel { pk = 2852, product_tmpl_id = [MkRecordModel { pk = 2852, name = "standard Cushion only for Mayfair Dining Armchair", list_price = Just (PKPrice CX GBP INC20, 88) }], trade = Just (PKPrice CX GBP EX20, 46), retail = Just (PKPrice CX GBP INC20, 88), contract = Just (PKPrice CX GBP EX20, 54), default_code = "CYWORMDA" }, 
   MkRecordModel { pk = 726, product_tmpl_id = [MkRecordModel { pk = 726, name = "230cm Mayfair Oval Dining Table", list_price = Just (PKPrice CX GBP INC20, 999) }], trade = Just (PKPrice CX GBP EX20, 591), retail = Just (PKPrice CX GBP INC20, 1135), contract = Just (PKPrice CX GBP EX20, 709), default_code = "YWORMOT7" }, 
   MkRecordModel { pk = 2942, product_tmpl_id = [MkRecordModel { pk = 2942, name = "Mayfair Sun Lounger (No Cushion)", list_price = Just (PKPrice CX GBP INC20, 734) }], trade = Just (PKPrice CX GBP EX20, 450), retail = Just (PKPrice CX GBP INC20, 864), contract = Just (PKPrice CX GBP EX20, 593), default_code = "YWORMSL-nc" }, 
   MkRecordModel { pk = 3531, product_tmpl_id = [MkRecordModel { pk = 3531, name = "standard Cushion only for Mayfair Sun Lounger", list_price = Just (PKPrice CX GBP INC20, 95) }], trade = Just (PKPrice CX GBP EX20, 45), retail = Just (PKPrice CX GBP INC20, 86), contract = Just (PKPrice CX GBP EX20, 55), default_code = "CYWORMSL" }, 
   MkRecordModel { pk = 733, product_tmpl_id = [MkRecordModel { pk = 733, name = "50cm Mayfair Square Side Table", list_price = Just (PKPrice CX GBP INC20, 199) }], trade = Just (PKPrice CX GBP EX20, 119), retail = Just (PKPrice CX GBP INC20, 228), contract = Just (PKPrice CX GBP EX20, 143), default_code = "YWORMST" }, 
   MkRecordModel { pk = 4085, product_tmpl_id = [MkRecordModel { pk = 4085, name = "300cm Rectangular Premium Garden Furniture Set cover", list_price = Just (PKPrice CX GBP INC20, 139) }], trade = Just (PKPrice CX GBP EX20, 72), retail = Just (PKPrice CX GBP INC20, 139), contract = Just (PKPrice CX GBP EX20, 86), default_code = "YCOVN1" }, 
   MkRecordModel { pk = 4089, product_tmpl_id = [MkRecordModel { pk = 4089, name = "215cm Single Sunlounger Premium cover", list_price = Just (PKPrice CX GBP INC20, 69) }], trade = Just (PKPrice CX GBP EX20, 36), retail = Just (PKPrice CX GBP INC20, 69), contract = Just (PKPrice CX GBP EX20, 43), default_code = "YCOVN5" }, 
   MkRecordModel { pk = 4095, product_tmpl_id = [MkRecordModel { pk = 4095, name = "65cm Side Table Premium cover ", list_price = Just (PKPrice CX GBP INC20, 49) }], trade = Just (PKPrice CX GBP EX20, 26), retail = Just (PKPrice CX GBP INC20, 49), contract = Just (PKPrice CX GBP EX20, 31), default_code = "YCOVN11" }]

static_boms : List BrowseBoM.RecordModel
static_boms = [
  MkRecordModel { pk = 525, product_qty = 1, bom_id = Nothing, bom_lines = [MkRecordModel { pk = 5885, product_qty = 8, bom_id = Just 525, bom_lines = [], product_id = 2932 },MkRecordModel { pk = 5886, product_qty = 8, bom_id = Just 525, bom_lines = [], product_id = 2852 }, MkRecordModel { pk = 527, product_qty = 1, bom_id = Just 525, bom_lines = [], product_id = 726 }], product_id = 1042 }, 
  MkRecordModel { pk = 1269, product_qty = 1, bom_id = Nothing, bom_lines = [MkRecordModel { pk = 5925, product_qty = 2, bom_id = Just 1269, bom_lines = [], product_id = 2942 }, MkRecordModel { pk = 5926, product_qty = 2, bom_id = Just 1269, bom_lines = [], product_id = 3531 }, MkRecordModel { pk = 1270, product_qty = 1, bom_id = Just 1269, bom_lines = [], product_id = 733 }], product_id = 1064 }]
