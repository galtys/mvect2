module UserDataDemo

import Libc.Time
import Data.Ratio
import Category.Transaction.Types
import Odoo.Schema.PJBRecDef

make_bom : Bits32 -> EQty -> List BrowseBoM.RecordModel  -> Bits32 -> BrowseBoM.RecordModel
make_bom pk_ qty_ boms p_id= MkRecordModel {pk=pk_,product_qty=qty_,bom_id=Nothing,bom_lines=boms, product_id=p_id }

make_prod : Bits32->String->EQty->String -> BrowseProduct.RecordModel
make_prod pk_ name_ price sku = MkRecordModel 
          {pk=pk_,
           product_tmpl_id=[MkRecordModel {pk=pk_,name=name_,list_price = Just (PKPrice CX GBP INC20, price)}], 
           trade = Just (PKPrice CX GBP EX20, price*0.7), 
           retail = Just (PKPrice CX GBP INC20, price*1.1), 
           contract = Just (PKPrice CX GBP EX20, price*0.9), 
           default_code =sku }
export
static_products : List BrowseProduct.RecordModel
static_products = [make_prod 1 "Na pravdě záleží?" (239/30) "9788090392380",
            make_prod 2 "Konflikt" (200/29) "9788090631601",
            make_prod 3 "Category Theory for Programmers" (34) "9780464243878",
            make_prod 4 "Type-Driven Development with Idris" (49.99) "9781617293023",
            make_prod 5 "Zapomenuté příběhy" (250/30) "9788088088998",
            make_prod 6 "Reality in Advertising" (14.95/1.8) "9780982694145",
            make_prod 7 "Global Warming Disaster" (44) "9781441110527",
            make_prod 8 "AD Bundle" (239/30 + 200/29 - 5) "bundle1",
            make_prod 9 "Programming Bundle" (34 + 49.99 - 4) "bundle2"]



bundle1 : BrowseBoM.RecordModel
bundle1 = make_bom 1 1 [make_bom 2 2 [] 1, make_bom 3 1 [] 2] 8

bundle2 : BrowseBoM.RecordModel
bundle2 = make_bom 4 1 [make_bom 5 1 [] 3, make_bom 6 1 [] 4] 9
export
static_boms : List BrowseBoM.RecordModel
static_boms = [bundle1, bundle2]


export
sti20 : PrimOrderTax.RecordModel
sti20 = PrimOrderTax.MkRecordModel 
      { pk = 14, 
        name = "Standard rate sales IncVAT (20%)", 
        description = Just "STI20", 
        amount = 1/5, 
        type = Just "percent", 
        price_include = Just True }

make_order : Bits32->Date->List (Bits32,EQty,EQty)->Date -> BrowseOrder.RecordModel
make_order pk_ d_ lines de_ = ret where
     ol : List BrowseOrderLine.RecordModel
     ol = [MkRecordModel { pk = 1, 
                              price_unit = price, 
                              product_uom_qty = qty, 
                              discount = Just (EQVal EPercent 0), 
                              delivery_line = Nothing, 
                              order_id = pk_, 
                              product_id = Just p_id, 
                              tax_ids = [sti20] }  | (p_id,price,qty) <- lines]
     ret : BrowseOrder.RecordModel 
     ret = BrowseOrder.MkRecordModel 
       { pk = pk_, 
         origin = Nothing, 
         order_policy = "manual", 
         date_order = d_,--"2021-11-18", 
         partner_id = 1, 
         amount_tax = (PKPrice CX GBP TAXAMOUNT,833), --MkPrice { tax = TAXAMOUNT, price = 833 }, 
         state = "manual", 
         partner_invoice_id = 1, 
         amount_untaxed = (PKPrice CX GBP EX20, 4165), --MkPrice { tax = EX20, price = 4165 }, 
         amount_total = (PKPrice CX GBP INC20, 4998), --MkPrice { tax = INC20, price = 4998 }, 
         name = #"XO000\#{show pk_}"#, 
         partner_shipping_id = 1, 
         picking_policy = "direct", 
         carrier_id = Just 1, 
         order_line = ol,
         requested_date = Just de_}

export
so_44970 : BrowseOrder.RecordModel
so_44970 = BrowseOrder.MkRecordModel 
       { pk = 44970, 
         origin = Nothing, 
         order_policy = "manual", 
         date_order = "2021-11-18", 
         partner_id = 31587, 
         amount_tax = (PKPrice CX GBP TAXAMOUNT,833), --MkPrice { tax = TAXAMOUNT, price = 833 }, 
         state = "manual", 
         partner_invoice_id = 31587, 
         amount_untaxed = (PKPrice CX GBP EX20, 4165), --MkPrice { tax = EX20, price = 4165 }, 
         amount_total = (PKPrice CX GBP INC20, 4998), --MkPrice { tax = INC20, price = 4998 }, 
         name = "SO44907", 
         partner_shipping_id = 31587, 
         picking_policy = "direct", 
         carrier_id = Just 7, 
         order_line = 
             [MkRecordModel { pk = 176369, 
                              price_unit = 0, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 0), 
                              delivery_line = Just True, 
                              order_id = 44970, 
                              product_id = Just 735, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176363, 
                              price_unit = 3199, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 0), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 1042, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176364, 
                              price_unit = 1799, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 0), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 1064, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176366,
                              price_unit = 139, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 100), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 4085, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176367, 
                              price_unit = 69, 
                              product_uom_qty = 2, 
                              discount = Just (EQVal EPercent 100), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 4089, 
                              tax_ids = [sti20] }, 
              MkRecordModel { pk = 176368, 
                              price_unit = 49, 
                              product_uom_qty = 1, 
                              discount = Just (EQVal EPercent 100), 
                              delivery_line = Just False, 
                              order_id = 44970, 
                              product_id = Just 4095, 
                              tax_ids = [sti20]} ] , 
         requested_date = Nothing }
         
export
sp_43747 : BrowseStockPicking.RecordModel
sp_43747 = MkRecordModel 
      { pk = 43747, 
        origin = Just "SO44907", 
        backorder_id = Nothing, 
        date_done = "", 
        partner_id = Just 31587, 
        min_date = "2021-11-19 12:00:00", 
        name = "OUT40884", 
        state = "assigned", 
        move_ids = 
          [MkRecordModel 
             { pk = 163195, 
               origin = Just "SO44907", 
               price_unit = Just 999, 
               product_qty = 1, 
               product_id =  726, 
               location_id =  12, 
               location_dest_id =  9, 
               picking_id = Just 43747, 
               purchase_line_id = 1,
               sale_line_id = 1,
               state = "confirmed" }, 
           MkRecordModel 
             { pk = 163196, 
               origin = Just "SO44907", 
               price_unit = Just 211, 
               product_qty = 8, 
               product_id =  2932, 
               location_id =  12, 
               location_dest_id =  9, 
               picking_id = Just 43747, 
               purchase_line_id = 1,
               sale_line_id = 1,
               state = "confirmed" }, 
           MkRecordModel 
             { pk = 163197, 
               origin = Just "SO44907", 
               price_unit = Just 88, 
               product_qty = 8, 
               product_id =  2852, 
               location_id =  12, 
               location_dest_id =  9, 
               picking_id = Just 43747, 
               purchase_line_id = 1,
               sale_line_id = 1,
               state = "confirmed" }, 
           MkRecordModel 
             { pk = 163198, 
               origin = Just "SO44907", 
               price_unit = Just 199, 
               product_qty = 1, 
               product_id =  733, 
               location_id =  12, 
               location_dest_id =  9, 
               picking_id = Just 43747, 
               purchase_line_id = 1,
               sale_line_id = 1,
               state = "confirmed" }, 
           MkRecordModel 
               { pk = 163199, 
                 origin = Just "SO44907", 
                 price_unit = Just 734, 
                 product_qty = 2, 
                 product_id =  2942, 
                 location_id =  12, 
                 location_dest_id =  9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,
                 state = "confirmed" }, 
           MkRecordModel { 
                 pk = 163200, 
                 origin = Just "SO44907", 
                 price_unit = Just 95, 
                 product_qty = 2, 
                 product_id =  3531, 
                 location_id =  12, 
                 location_dest_id =  9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,
                 state = "confirmed" }, 
           MkRecordModel { 
                 pk = 163192, 
                 origin = Just "SO44907", 
                 price_unit = Just 139, 
                 product_qty = 1, 
                 product_id =  4085, 
                 location_id =  12, 
                 location_dest_id = 9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,
                 state = "assigned" }, 
           MkRecordModel { 
                 pk = 163193, 
                 origin = Just "SO44907", 
                 price_unit = Just 69, 
                 product_qty = 2, 
                 product_id =  4089, 
                 location_id =  12, 
                 location_dest_id =  9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,                 
                 state = "assigned" }, 
           MkRecordModel { 
                 pk = 163194, 
                 origin = Just "SO44907", 
                 price_unit = Just 49, 
                 product_qty = 1, 
                 product_id =  4095, 
                 location_id =  12, 
                 location_dest_id =  9, 
                 picking_id = Just 43747, 
                 purchase_line_id = 1,
                 sale_line_id = 1,                 
                 state = "assigned" }] }
export
va_43244 : BrowseAccountVoucher.RecordModel 
va_43244 = BrowseAccountVoucher.MkRecordModel 
         { pk = 43244, 
           number = "WALT1248", 
           partner_id = Just 31587, 
           journal_id = Just 23, 
           amount = 4998 }
export
retail_cust_31587 : BrowseResPartner.RecordModel
retail_cust_31587 = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "John Retail1", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Sharing House", 
         contract = Just False, 
         city = Just "Mid London", 
         zip = Just "CE5 CE6", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "retail_cust1@btconnect.com", 
         street2 = Just "Mid Lane" }

export
hilton : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
hilton = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "John Hilton", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Sharing Hilton House", 
         contract = Just False, 
         city = Just "Mid London", 
         zip = Just "CE6", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "hilton@btconnect.com", 
         street2 = Just "Mid Lane" }
         --retail_cust_31587 --MkA "Street" "" "London" "SU 4X" UK (MkC "Hilton")


export
factory1 : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
factory1 = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "factory 1", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Sharing House", 
         contract = Just False, 
         city = Just "Mid London", 
         zip = Just "CE5 CE6", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "retail_cust1@btconnect.com", 
         street2 = Just "Mid Lane" }
         --retail_cust_31587 --MkA "Factory street" "" "Asia" "44AX" UK (MkC "Factory1")

export
factory2 : BrowseResPartner.RecordModel --BrowseResPartner.RecordModel
factory2 = BrowseResPartner.MkRecordModel 
       { pk = 31587, 
         name = "factory 2", 
         use_parent_address = Just False, 
         active = Just True, 
         street = Just "Sharing House", 
         contract = Just False, 
         city = Just "Mid London", 
         zip = Just "CE5 CE6", 
         country_id = Just 284, 
         parent_id = Nothing, 
         child_ids = [], 
         email = "retail_cust1@btconnect.com", 
         street2 = Just "Mid Lane" }
         --retail_cust_31587  --MkA "Factory street2" "" "Asia2" "X" UK (MkC "Factory2")
{-
export
p1:Product
p1 = ("p1",2)
export
p2:Product
p2 = ("p2",7)
export
p3:Product
p3 = ("p1",10)

export
p4:Product
p4 = ("p4",10)

-}
