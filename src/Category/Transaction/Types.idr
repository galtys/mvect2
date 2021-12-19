module Category.Transaction.Types

import Category.Transaction.Qty
import Data.SortedMap
--import Control.Monad.State
import Crypto.Hash.SHA256
import Data.Ratio
import Generics.Derive
import JSON

%language ElabReflection

public export
Date : Type
Date = String

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

export
currencyAll : List Currency
currencyAll = [GBP, EUR, CZK, USD]

export
toCurrency : String -> Maybe Currency
toCurrency s = lookup s [ (show x,x) | x <- currencyAll ] 

{-
export
taxCodeAll : List TaxCode
taxCodeAll = [INC20, EX20, TAXAMOUNT]
export
toTaxCode : String -> Maybe TaxCode
toTaxCode tc = lookup tc [ (show x,x) | x <- taxCodeAll ]
-}


--get_tc_prodkey : List TaxCode -> Currency -> ProdKey
--get_tc_prodkey xs cy = PKTax cy (concat [(show x) | x <- xs] )


public export
data ProdKey = PKCy Currency | PKUser String | PK32 Bits32 | PKTax Currency TaxCode | FromInteger --|PKUserDate Date String | PK32Date Date Bits32
%runElab derive "ProdKey" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]

public export
TProdKey : Type
TProdKey = T ProdKey
--%runElab derive "TProdKey" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]

--public export
--ProdKey : Type
--ProdKey = TProdKey

public export
data BoM32 : Type where  
  --Node32 : (qty:TQty) -> (sku:Bits32) -> (bid:Bits32)->(bom_id:Maybe Bits32)->(components:List BoM32) -> BoM32   
   Node32 : (qty:EQty) -> (sku:ProdKey) ->(components:List BoM32) -> BoM32   
%runElab derive "BoM32" [Generic, Meta, Show, Eq,ToJSON,FromJSON]

public export
Product : Type
Product = (ProdKey, EQty)
public export
TProduct : Type
TProduct = (TProdKey, EQty)


public export
record Price where
  constructor MkPrice
  tax : TaxCode
  price : EQty
%runElab derive "Price" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
export  
toINC20 : Double -> Price
toINC20 x = MkPrice INC20 (cast x) 
export
fromPrice : Price -> Double
fromPrice (MkPrice tax x) = (cast x)
export
toEX20 : Double -> Price
toEX20 x = MkPrice EX20 (cast x) 
export
toTaxA : Double -> Price
toTaxA x = MkPrice TAXAMOUNT (cast x) 



public export
Cast Price Double where
  cast = fromPrice
public export
Cast Price EQty where
  cast (MkPrice tax x) = x
  
public export
FromString ProdKey where
   fromString s = case toCurrency s of
           Nothing => PKUser s
           Just cy => PKCy cy
           

public export
CurrencyProd : Type
CurrencyProd = (ProdKey, Price)

public export
Hom1 : Type
Hom1 = List Product

public export
THom : Type
THom = List TProduct

public export
Product2 : Type
Product2 = (ProdKey, CurrencyProd)


public export
Hom2 : Type
Hom2 = List Product2 --was (Hom1->Hom1)

public export
record Hom11 where
   constructor MkH11
   dx:Hom1
   cx:Hom1
%runElab derive "Hom11" [Generic, Meta, RecordToJSON,RecordFromJSON]



public export
record Hom121 where
   constructor MkH121
   dx:Hom1
   bom : List BoM32   
   appl:Hom2
   cx : Hom1
   
%runElab derive "Hom121" [Generic, Meta, RecordToJSON,RecordFromJSON]

export
fromH121 : Hom121 -> Hom11
fromH121 h121 = (MkH11 (dx h121) (cx h121))



------------- THIS WILL BE MOVED

public export
apply2' : Hom2 -> Hom1 -> Hom1
apply2' h2 p = ret where
  h2map : SortedMap ProdKey CurrencyProd --Product
  h2map = fromList h2
  
  ret1 : List (Maybe CurrencyProd,EQty)
  ret1 = [ (lookup (fst x) h2map,(snd x)) | x<-p ]
  
  ret2 : List (Maybe CurrencyProd,EQty) -> Hom1
  ret2 [] = []
  ret2 ((Nothing,q) ::xs) = (ret2 xs)
  ret2 ((Just x,q) ::xs) = [(fst x, (price (snd x))*q)]++ (ret2 xs)
  
  
  ret : Hom1
  ret = (ret2 ret1)
  
  

