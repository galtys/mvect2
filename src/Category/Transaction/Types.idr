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

public export
data DxCx = DX | CX
%runElab derive "DxCx" [Generic, Meta, Eq, Ord,Show, EnumToJSON,EnumFromJSON]

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
data ProdKey = PKCy DxCx Currency | PKUser DxCx String | PK32 DxCx Bits32 | PKPrice DxCx Currency TaxCode | FromInteger DxCx --|PKAppl ProdKey ProdKey
%runElab derive "ProdKey" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]
export
taxCodeFromKey : ProdKey -> TaxCode
taxCodeFromKey (PKCy x y) = ERROR
taxCodeFromKey (PKUser x y) = ERROR
taxCodeFromKey (PK32 x y) = ERROR
taxCodeFromKey (PKPrice x y z) = z
taxCodeFromKey (FromInteger x) = ERROR
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
toTaxAmountKey (PKCy x y) = PKCy x y
toTaxAmountKey (PKUser x y) = PKUser x y
toTaxAmountKey (PK32 x y) = PK32 x y
toTaxAmountKey (PKPrice x y z) = PKPrice x y TAXAMOUNT
toTaxAmountKey (FromInteger x) = FromInteger x


public export
data BoM32 : Type where  
  --Node32 : (qty:TQty) -> (sku:Bits32) -> (bid:Bits32)->(bom_id:Maybe Bits32)->(components:List BoM32) -> BoM32   
   Node32 : (qty:EQty) -> (sku:ProdKey) ->(components:List BoM32) -> BoM32   
%runElab derive "BoM32" [Generic, Meta, Show, Eq,Ord,ToJSON,FromJSON]

public export
Product : Type
Product = (ProdKey, EQty)

export  
toINC20 : Double -> Product
toINC20 x = (PKPrice CX GBP INC20, (cast x))     --MkPrice INC20 (cast x) 
export
fromPrice : Product -> Double
fromPrice (x,y) = cast y   --(MkPrice tax x) = (cast x)
export
toEX20 : Double -> Product
toEX20 x = (PKPrice CX GBP EX20, (cast x))  --MkPrice EX20 (cast x) 
export
toTaxA : Double -> Product
toTaxA x = (PKPrice CX GBP TAXAMOUNT, (cast x))  --MkPrice TAXAMOUNT (cast x) 

public export
Cast Product Double where
  cast = fromPrice
public export
Cast Product EQty where
  cast (x,y) = y
  
public export
FromString ProdKey where
   fromString s = case toCurrency s of
           Nothing => PKUser DX s
           Just cy => PKCy CX cy
           
public export
Hom1 : Type
Hom1 = List Product
{-
public export
THom : Type
THom = List TProduct
-}
public export
Product2 : Type
Product2 = (ProdKey, Product) --CurrencyProd)


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
