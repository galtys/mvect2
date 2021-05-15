module Test

import Demo.DemoCompany
import Data.Nat

namespace AppBaseData
  public export
  data Asset = MkAsset
  data Location = MkLocation
  data Owner = MkOwner

  -- vector states and messages
  data VectorUsage = State | Message
    
  Order : Type
  Invoice : Type
  OrderLessInvoice : Type

  LocationTarget : Type
  Picking : Type
  Backorders : Type

  CurrencyTarget : Type
  Payment : Type
  Debt : Type

namespace NsModelVar
  export
  interface ModelVar a where
       name : a -> String
       all_items : a -> List String  

  export
  implementation ModelVar AssetVars where
       name Cy = "cy"
       name Sku = "sku"
       all_items Cy = (map show [GBP,EUR,USD,CZK])
       all_items Sku =   (map show [A1,A2,A3,A4])

namespace NsModel
  export                              
  interface Model a where
       name : a -> String
       vars : a -> List String
       all_items : a -> List String
       
  export
  implementation Model Asset where
       name MkAsset= "asset"
       vars MkAsset= (map name [Cy,Sku])
       all_items MkAsset = (all_items Cy) ++ (all_items Sku)

test:String
test="hello idris2"

%foreign "javascript:lambda: x => console.log(x)"
prim__consoleLog : String -> PrimIO ()

consoleLog : HasIO io => String -> io ()
consoleLog x = primIO $ prim__consoleLog x

x:Nat
x=1099956644355

y:Nat
y=pred x

main : IO ()
main = do
     consoleLog test
     consoleLog $ show y

