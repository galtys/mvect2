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
data Country = UK | CZ | US | DE | FR
%runElab derive "Country" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

public export
record Contact where
  constructor MkC
  name : String
%runElab derive "Contact" [Generic, Meta, Eq, Ord,Show, RecordToJSON,RecordFromJSON]

public export
record Address where
  constructor MkA
  street : String
  street2 : String
  city : String
  zip : String
  country_id : Country
  contact: Contact
%runElab derive "Address" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]

public export
data DirectionTag = Sale | Purchase
%runElab derive "DirectionTag" [Generic, Meta, Eq,Ord, Show,EnumToJSON,EnumFromJSON]     

public export
data Location =  Self | Control DirectionTag Address |Partner DirectionTag Address | Init
%runElab derive "Location" [Generic, Meta, Eq, Ord,Show,ToJSON,FromJSON]


public export
data Ledger = OnHand | Forecast
%runElab derive "Ledger" [Generic, Meta, Eq, Ord, Show,EnumToJSON,EnumFromJSON]

public export
data TaxCode =  INC20 | EX20 | TAXAMOUNT |ERROR
%runElab derive "TaxCode" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

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
  
