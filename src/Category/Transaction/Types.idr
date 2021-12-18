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
{-
public export
data LineTermMultType = UnitPrice | Discount | MultQty | TaxMul
%runElab derive "LineTermMultType" [Generic, Meta, Eq, Ord,Show,EnumToJSON,EnumFromJSON]


public export
data LineTerm : Type where
     LEHom1 : (qty:EQty) -> LineTerm
     LETaxCode : (taxcode:TaxCode) -> LineTerm -> LineTerm
     LEAdd : (l1:LineTerm) -> (l2:LineTerm) -> LineTerm
     LEMul : (u:EQty) -> (mu:LineTermMultType) -> (l:LineTerm) -> LineTerm
%runElab derive "LineTerm" [Generic, Meta, Eq, Show, ToJSON,FromJSON]     
-}

public export
data ProdKey = PKUser String | PK32 Bits32 | PKTax String
%runElab derive "ProdKey" [Generic, Meta, Eq, Ord,Show, ToJSON,FromJSON]

public export
data BoM32 : Type where  
  --Node32 : (qty:TQty) -> (sku:Bits32) -> (bid:Bits32)->(bom_id:Maybe Bits32)->(components:List BoM32) -> BoM32   
   Node32 : (qty:EQty) -> (sku:ProdKey) ->(components:List BoM32) -> BoM32   
%runElab derive "BoM32" [Generic, Meta, Show, Eq,ToJSON,FromJSON]



public export
record Price where
  constructor MkPrice
  tax : TaxCode
  price : EQty
%runElab derive "Price" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]

price_mult : Price -> Price -> Price



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
   fromString s = PKUser s

public export
Product : Type
Product = (ProdKey, EQty)

public export
Currency : Type
Currency = (ProdKey, Price)

public export
Hom1 : Type
Hom1 = List Product

public export
Product2 : Type
Product2 = (ProdKey, Currency)

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
