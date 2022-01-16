module Category.Transaction.Types

import Category.Transaction.Qty
import Data.SortedMap
--import Control.Monad.State
import Crypto.Hash.SHA256
import Data.Ratio
--import Libc.Time

import Generics.Derive
import JSON

%language ElabReflection

public export
Date : Type
Date = String --DateTime --String

namespace DocumentRouteType
  public export
  data DocumentRouteType = SaleRoute | PurchaseRoute | InitRoute | FxRoute | BankRoute | StockInputRoute | StockOutputRoute | ListSale | ListPurchase |NA |StockLossRoute|TaxSaleRoute |TaxPurchaseRoute | Allocation
  %runElab derive "DocumentRouteType" [Generic, Meta, Eq,Show,Ord,EnumToJSON,EnumFromJSON]

public export
data DocumentType = SaleOrder 
                  | SaleOrderAmendment
                  | PurchaseOrder
                  | Order 
                  | CustomerInvoice
                  | SupplierInvoice
                  | CustomerCreditNote
                  | SupplierCreditNote
                  | Invoice 
                  | CreditNote
                  | Payment 
                  | Refund 
                  | Delivery 
                  | Dispatch
                  | Return 
                  | Reservation 
                  | AllocationDoc
                  | Allocation
                  | Shipping 
                  | NotDefined 
                  | PurchaseReservation 
                  | SaleReservation 
                  | PurchaseAllocation 
                  | SaleAllocation 
                  | GoodsReceipt 

--| RouteDoc |RouteDocInv
%runElab derive "DocumentType" [Generic, Meta, Eq,Show,Ord,EnumToJSON,EnumFromJSON]

export
docPrefix : DocumentType -> String
docPrefix SaleOrder = "SO"
docPrefix SaleOrderAmendment = "SOA"
docPrefix PurchaseOrder = "PO"
docPrefix Order = "O"
docPrefix CustomerInvoice = "INV"
docPrefix SupplierInvoice = "SINV"
docPrefix CustomerCreditNote = "CRINV"
docPrefix SupplierCreditNote = "SCRINV"
docPrefix Invoice = "INV"
docPrefix CreditNote = "CRN"
docPrefix Payment = ""
docPrefix Refund = ""
docPrefix Delivery = "D"
docPrefix Dispatch = "OUT"
docPrefix Return = "RET"
docPrefix Reservation = "RES"
docPrefix Allocation = "AL"
docPrefix AllocationDoc = "ALD"
docPrefix Shipping = "SH"
docPrefix NotDefined = "ND"
docPrefix PurchaseReservation = "PORES"
docPrefix SaleReservation = "SORES"
docPrefix PurchaseAllocation = "POAL"
docPrefix SaleAllocation = "SOAL"
docPrefix GoodsReceipt = "GRC"
--docPrefix RouteDoc = "RD"
--docPrefix RouteDocInv = "RDI"

export
docDigits : DocumentType -> Int
docDigits d = 3

public export
record DocumentNumberItem where
    constructor MkDNI
    dt:DocumentType
    code:Maybe String
    number:Int
%runElab derive "DocumentNumberItem" [Generic, Meta, Eq, Show,Ord,RecordToJSON,RecordFromJSON]

public export
data DocumentNumber : Type where
     DocNr : DocumentNumberItem -> DocumentNumber
     RouteNr : DocumentNumberItem -> DocumentNumber
     RouteName : String -> DocumentNumber
     DocName : String -> DocumentNumber     
%runElab derive "DocumentNumber" [Generic, Meta, Eq, Ord,ToJSON,FromJSON]


export
toRouteDoc : DocumentNumber -> DocumentNumber
toRouteDoc (DocNr (MkDNI dt code number)) = (RouteNr (MkDNI dt code number))
toRouteDoc (DocName x) = (RouteName x)
toRouteDoc (RouteNr (MkDNI dt code number)) = (RouteNr (MkDNI dt code number))
toRouteDoc (RouteName x) = (RouteName x)


repeat_zeros : Int -> String
repeat_zeros x = if x<=0 then "" else concat [ "0" | u<- [0..x]]
with_zeros : DocumentType -> Int -> String
with_zeros dt a = ret where
     str : String
     str = show a
     ret : String
     ret = ( repeat_zeros ( (docDigits dt) - (cast $length str)) ) ++ str

export 
show_document_number : DocumentNumber -> String
show_document_number (DocNr (MkDNI dt Nothing number)) = (docPrefix dt)++(with_zeros dt number) --?show_document_number_rhs_0
show_document_number (DocNr (MkDNI dt (Just x) number)) = (docPrefix dt)++x++(with_zeros dt number) --?show_document_number_rhs_1
show_document_number (RouteNr (MkDNI dt Nothing number)) = "Allocation "++(docPrefix dt)++(with_zeros dt number)
show_document_number (RouteNr (MkDNI dt (Just x) number)) = "Allocation "++(docPrefix dt)++x++(with_zeros dt number)
show_document_number (RouteName x) = x
show_document_number (DocName x) = x

export
Show DocumentNumber where
   show = show_document_number     

public export
data TreeB = Leaf String | Node TreeB String TreeB
%runElab derive "TreeB" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]


public export
data DirectionTag = Sale | Purchase
%runElab derive "DirectionTag" [Generic, Meta, Eq,Ord, Show,EnumToJSON,EnumFromJSON]     


public export
data Ledger = OnHand | Forecast
%runElab derive "Ledger" [Generic, Meta, Eq, Ord, Show,EnumToJSON,EnumFromJSON]

public export
data TaxCode =  INC20 | EX20 | TAXAMOUNT |ERROR
%runElab derive "TaxCode" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

public export
data Currency = GBP | EUR | CZK | USD
%runElab derive "Currency" [Generic, Meta, Eq, Ord,Show, EnumToJSON,EnumFromJSON]

public export
data DxCx = DX | CX
%runElab derive "DxCx" [Generic, Meta, Eq, Ord,Show, EnumToJSON,EnumFromJSON]
public export
data DrCr = Dr | Cr
%runElab derive "DrCr" [Generic, Meta, Eq, Ord,Show, EnumToJSON,EnumFromJSON]

export
currencyAll : List Currency
currencyAll = [GBP, EUR, CZK, USD]

export
toCurrency : String -> Maybe Currency
toCurrency s = lookup s [ (show x,x) | x <- currencyAll ] 


namespace ProdKey
  export
  One : Maybe Bits8
  One = Nothing
  
  public export
  data ProdKey : Type where
     PKCy:   (dcx:DxCx) ->  (cy:Currency) -> (v:Maybe Bits8) -> ProdKey
     PKUser: (dcx:DxCx) ->  (u:String)   ->  (v:Maybe Bits8) -> ProdKey
     PK32:   (dcx:DxCx) ->  (pk:Bits32)   -> (v:Maybe Bits8) -> ProdKey
     PKPrice: (dcx:DxCx) -> (cy:Currency) -> (tax:TaxCode) -> (v:Maybe Bits8)->ProdKey
     FromInteger: (dcx:DxCx) -> (v:Maybe Bits8)->ProdKey
  %runElab derive "ProdKey" [Generic, Meta, Eq, Ord,ToJSON,FromJSON]
  
  show_v : Maybe Bits8 -> String
  show_v Nothing = ""
  show_v (Just x) = "^\{show x}"
  
  
  show_pk : ProdKey -> String
  show_pk (PKCy dcx cy v) = "\{show cy}\{show_v v}"
  show_pk (PKUser dcx u v) ="\{u}\{show_v v}"
  show_pk (PK32 dcx pk v) = "pk\{show pk}\{show_v v}"
  show_pk (PKPrice dcx cy tax v) = "\{show cy}\{show_v v} \{show tax}"
  show_pk (FromInteger dcx v) = "\{show dcx}\{show_v v}"
  
  export
  Show ProdKey where
    show = show_pk
  
  addM8 : Maybe Bits8 -> Bits8 -> Maybe Bits8
  addM8 Nothing y = Just y
  addM8 (Just x) y = Just (x+y)
  
  export
  addBits8 : ProdKey -> Bits8 -> ProdKey
  addBits8 (PKCy dcx cy v) y = (PKCy dcx cy (addM8 v y) )
  addBits8 (PKUser dcx u v) y = (PKUser dcx u (addM8 v y))
  addBits8 (PK32 dcx pk v) y = (PK32 dcx pk (addM8 v y))
  addBits8 (PKPrice dcx cy tax v) y = (PKPrice dcx cy tax (addM8 v y))
  addBits8 (FromInteger dcx v) y = (FromInteger dcx (addM8 v y))
  
  
  export
  PKIntOne : ProdKey
  PKIntOne = FromInteger DX One
  
   --|PKAppl ProdKey Nat --ProdKey
  export
  pkFromInteger : Integer -> List (ProdKey,EQty) --Product
  pkFromInteger x = [(PKIntOne, (fromInteger x))]

  export
  pk32DX : Bits32 -> ProdKey
  pk32DX x = PK32 DX x ProdKey.One
  
  export
  pkPriceEX20 : Currency -> ProdKey
  pkPriceEX20 cy = ProdKey.PKPrice CX cy EX20 ProdKey.One
  export
  pkPriceINC20 : Currency -> ProdKey
  pkPriceINC20 cy = ProdKey.PKPrice CX cy INC20 ProdKey.One
  export
  pkPriceTA: Currency -> ProdKey
  pkPriceTA cy = ProdKey.PKPrice CX cy TAXAMOUNT ProdKey.One
  

  public export
  FromString ProdKey where
     fromString s = case toCurrency s of
           Nothing => PKUser DX s One
           Just cy => PKCy CX cy One



export
taxCodeFromKey : ProdKey -> TaxCode
taxCodeFromKey (PKCy x y v) = ERROR
taxCodeFromKey (PKUser x y v) = ERROR
taxCodeFromKey (PK32 x y v) = ERROR
taxCodeFromKey (PKPrice x y z v) = z
taxCodeFromKey (FromInteger x v) = ERROR
{-
export
currencyFromKey : ProdKey -> Maybe Currency
currencyFromKey (PKCy x y) = Just y
currencyFromKey (PKUser x y) = Nothing
currencyFromKey (PK32 x y) = Nothing
currencyFromKey (PKPrice x y z) = Just y
currencyFromKey (FromInteger x) = Nothing
-}
export
toTaxAmountKey : ProdKey -> ProdKey
toTaxAmountKey (PKCy x y v) = PKCy x y v
toTaxAmountKey (PKUser x y v) = PKUser x y v
toTaxAmountKey (PK32 x y v) = PK32 x y v
toTaxAmountKey (PKPrice x y z v) = PKPrice x y TAXAMOUNT v
toTaxAmountKey (FromInteger x v) = FromInteger x v


public export
data BoM32 : Type where  
  --Node32 : (qty:TQty) -> (sku:Bits32) -> (bid:Bits32)->(bom_id:Maybe Bits32)->(components:List BoM32) -> BoM32   
   Node32 : (qty:EQty) -> (sku:ProdKey) ->(components:List BoM32) -> BoM32   
%runElab derive "BoM32" [Generic, Meta, Show, Eq,Ord,ToJSON,FromJSON]

public export
Product : Type
Product = (ProdKey, EQty)
public export
Product2 : Type
Product2 = (ProdKey, Product) --CurrencyProd)

public export
record QLine where
  constructor MkQL
  dxpk : ProdKey
  --dxname : String
  bom : Maybe BoM32
  q  : EQty
  cxpk : ProdKey
  price : EQty
%runElab derive "QLine" [Generic, Meta, Show, Eq,Ord,RecordToJSON,RecordFromJSON]


public export
HomQLine : Type
HomQLine = List QLine

export
demoQL : HomQLine
demoQL = ret where
   muf : ProdKey -> EQty -> EQty -> QLine
   muf dxpk q p = (MkQL dxpk  Nothing q (pkPriceEX20 GBP) p)
   
   ret : HomQLine
   ret = [muf "sku1" 2 10, 
          muf "sku1" 3 10, 
          muf "sku1" 7 11, 
          muf "sku2" 4 9, 
          muf "sku3" 9 13,
          muf "sku1" 5 10, 
          muf "sku1" 2 12] 


{-
public export
ProductLine : Type
ProductLine = (QLine,EQty)
-}

export  
toINC20 : Double -> Product
toINC20 x = (PKPrice CX GBP INC20 One, (cast x))     --MkPrice INC20 (cast x) 
export
fromPrice : Product -> Double
fromPrice (x,y) = cast y   --(MkPrice tax x) = (cast x)
export
toEX20 : Double -> Product
toEX20 x = (PKPrice CX GBP EX20 One, (cast x))  --MkPrice EX20 (cast x) 
export
toTaxA : Double -> Product
toTaxA x = (PKPrice CX GBP TAXAMOUNT One, (cast x))  --MkPrice TAXAMOUNT (cast x) 

public export
Cast Product Double where
  cast = fromPrice
public export
Cast Product EQty where
  cast (x,y) = y
  
           
public export
Hom1 : Type
Hom1 = List Product
{-
public export
THom : Type
THom = List TProduct
-}


public export
Hom2 : Type
Hom2 = List Product2 --was (Hom1->Hom1)

public export
record Hom11 where
   constructor MkH11
   dx:Hom1
   cx:Hom1
%runElab derive "Hom11" [Generic, Meta, Show,Ord,Eq,RecordToJSON,RecordFromJSON]

public export
emptyHom11 : Hom11
emptyHom11 = MkH11 [] []   

public export
record Hom12 where
   constructor MkHom12
   dx:Hom1
   appl:Hom2
   bom : List BoM32
   
public export
record Hom121 where
   constructor MkH121
   dx:Hom1
   bom : List BoM32   
   appl:Hom2

   cx : Hom1
   h11 : Hom11
   
%runElab derive "Hom121" [Generic, Meta, Show,Ord,Eq,RecordToJSON,RecordFromJSON]

export
fromH121 : Hom121 -> Hom11
fromH121 h121 = h11 h121 --(MkH11 (dx h121) (cx h121))


------------- THIS WILL BE MOVED
{-
public export
apply3' : Hom2 -> Hom1 -> Hom1
apply3' h2 p = ret where
  h2map : SortedMap ProdKey CurrencyProd --Product
  h2map = fromList h2
  
  ret1 : List (Maybe CurrencyProd,EQty)
  ret1 = [ (lookup (fst x) h2map,(snd x)) | x<-p ]
  
  ret2 : List (Maybe CurrencyProd,EQty) -> Hom1
  ret2 [] = []
  ret2 ((Nothing,q) ::xs) = (ret2 xs)
  ret2 ((Just x,q) ::xs) = [(fst x, (snd (snd x))*q)]++ (ret2 xs)
  
  
  ret : Hom1
  ret = (ret2 ret1)
-}  
