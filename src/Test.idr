module Test



namespace AppBaseData
  Asset : Type
  Location : Type
  Owner : Type
  TaxDepartment : Type
 
  -- vector messages
  Order : Type
  Invoice : Type
  OrderLessInvoice : Type

  LocationTarget : Type
  Picking : Type
  Backorders : Type

  CurrencyTarget : Type
  Payment : Type
  Debt : Type



  data AssetCurrency = GBP | EUR | USD | CZK
  data AssetStock = A1 | A2 | A3 | A4 

  data VectorUsage = State | Message

  data TaxCodes = IVAT20 | EXVAT20 | ZVAT
  data OwnerDemo = Company 
  data Customers = Cust1 | Cust2 | Cust3
  data Suppliers = Sup1  | Sup2

  Show AssetCurrency where
       show GBP = "GBP"
       show EUR = "EUR"
       show USD = "USD"
       show CZK = "CZK"

  
  


test:String
test="hello idris2"

%foreign "javascript:lambda: x => console.log(x)"
prim__consoleLog : String -> PrimIO ()

consoleLog : HasIO io => String -> io ()
consoleLog x = primIO $ prim__consoleLog x

main : IO ()
main = consoleLog test


