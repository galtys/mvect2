module Demo.DemoCompany


public export
data NameSpace : Type where
  NS : String -> NameSpace

public export
data NSVar : Type where
  VarN : String -> NameSpace-> NSVar

public export
data IsVar : NSVar -> Nat -> List NSVar -> Type where
     First : IsVar n Z (n :: ns)
     Later : IsVar n i ns -> IsVar n (S i) (m :: ns)


public export
a : NSVar 
a = VarN "a" (NS "Asset")

public export
cy_ty : NSVar 
cy_ty = VarN "cy_ty" (NS "CurrencyType")

public export
cy : NSVar
cy = VarN "cy" (NS "Asset")

public export
sku : NSVar 
sku = VarN "sku" (NS "Asset")

public export
data Sequence : Type -> Type where
  Sq : Sequence ty
  Next : (ty:Type) -> Sequence ty
  Prev : (ty:Type) -> Sequence ty
  --Fst
  --Last
public export
data Relation : Type -> Type where
  M2O : (ty1:type) -> (ty2:type) -> Relation type
  M2M : (ty1:type) -> (ty2:type) -> Relation type
  X : (ty1:type) -> (ty2:type) -> Relation type
  ISO : (ty1:type) -> (ty2:type) -> Relation type

public export
data SimpleT :  Type where
  IntT :  SimpleT 
  BooleanT :  SimpleT 
  DateTimeT :  SimpleT 

public export
data VectorName : Type where
  VN : String -> VectorName

--public export
--data VOp = Create|Delete --operation

public export
data Vuse = Vstate | Vmessage

data Ring : Type -> Type where
  Plus : (ty1:type) -> (ty2:type) -> Ring type
  Mult : (ty1:type) -> (ty2:type) -> Ring type
  Inv :  (ty1:type) -> Ring type

data Carrier = IntCarrier

data Vector : Type where
  Vcarrier : (name:VectorName) ->  (x:NSVar) -> Carrier -> Vector 
  VRing : (name:VectorName) -> (x:NSVar) -> Ring (Vector ) -> Vector

public export
data Schema :  Type where
  Var : (x:NSVar) -> Schema  -- add   Vuse ->  idea is to declare a message and then turn it into msg or state in the exec env
  Seq : (x:NSVar) -> (seq:Sequence Schema ) ->  Schema 
  Rel : (x:NSVar) -> (rel:Relation Schema )  -> Schema   
  Si  : (x:NSVar) -> (si:SimpleT )   -> Schema 
  VOp : Schema
  OP  : Schema  --Create Or Delete

public export
s : Schema
s = Rel cy (M2O (Var a) (Var cy_ty)) 

{-
public export
data Env : Schema -> Type
  Nil : Env tm
  (::)  :  (s:Schema) -> Env 
-}

--public export
--data Env : (tm : Schema) -> List NSVar -> Type where
--     Nil : Env tm []
--     (::) : (s:Schema) -> Env tm vars -> Env tm (x :: vars)



  
namespace DemoData
  public export
  data AssetCurrency = GBP | EUR | USD | CZK
  public export  
  data AssetStock = A1 | A2 | A3 | A4 
  public export
  data AssetVars = Cy | Sku   -- model variables

  export
  Show AssetCurrency where
       show GBP = "GBP"
       show EUR = "EUR"
       show USD = "USD"
       show CZK = "CZK"
  export       
  Show AssetStock where
       show A1 = "a1"
       show A2 = "a2"
       show A3 = "a3"
       show A4 = "a4"
  
  public export  
  data TaxDepartmentCodes = IVAT20 | EXVAT20 | ZVAT
  public export  
  data OwnerDemo = DemoCompany
  public export   
  data CustomersDemo = Cust1 | Cust2 | Cust3
  public export  
  data SuppliersDemo = Sup1  | Sup2
  public export 
  data OwnerVars = TaxOffice | Company | Customers | Suppliers


  public export
  data TaxOfficeLocationDemo = MkTaxOfficeLocation
  public export 
  data CompanyLocationsDemo = Stock
  public export
  data CustomerLocationsDemo = Loc1 | Loc2 | Loc3
  public export
  data SupplierLocationsDemo = Loc4 | Loc5
  public export
  data LocationVars = TaxOfficeLocation | CompanyStock | CustomerLocation |SupplierLocation
  
  

