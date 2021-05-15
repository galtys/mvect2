module Demo.DemoCompany

public export
NameSpace : Type
NameSpace = String

data NameType : Type where
  Msg : NameType  -- message 
  St  : NameType  -- state
  Vn  : NameType  -- vector name
  
public export
data NSVar :  Type where
  VarMsg : String -> NameSpace-> NSVar
  VarSt  :String -> NameSpace-> NSVar
  V : String -> NSVar

public export
qty : NSVar 
qty = V "qty"

public export
unit : NSVar 
unit = V "unit"

public export
price : NSVar 
price = V "price"

public export
tot : NSVar 
tot = V "total"


public export
sku : NSVar 
sku = VarMsg "sku" "Asset"

public export
cy : NSVar 
cy = VarMsg "cy" "Asset"

public export
a : NSVar 
a = VarMsg "a" "Asset"

public export
cy_ty : NSVar  
cy_ty = VarMsg "cy_ty" "CurrencyType"

public export
sku_ty : NSVar 
sku_ty = VarMsg "sku_ty" "StockType"

public export
items : NSVar 
items = VarMsg "items" "Items"

public export
data Sequence : Type -> Type where
  Sq : Sequence ty
  Next : (ty:Type) -> Sequence ty
  Prev : (ty:Type) -> Sequence ty
  --Fst
  --Last

infixr 5 .|.  
  
public export
data Relation : Type -> Type where
  M2O : (ty1:type) -> (ty2:type) -> Relation type
  M2M : (ty1:type) -> (ty2:type) -> Relation type
  (.|.) : (ty1:type) -> (ty2:type) -> Relation type
  ISO : (ty1:type) -> (ty2:type) -> Relation type
--  OP  : Relation type
  
public export
data SimpleT :  Type where
  IntT :  SimpleT 
  BooleanT :  SimpleT 
  DateTimeT :  SimpleT 

data Ring : Type -> Type where
  Plus : (ty1:type) -> (ty2:type) -> Ring type
  Mult : (ty1:type) -> (ty2:type) -> Ring type
  InvP :  (ty1:type) -> Ring type
  NTran : (ty1:type) -> Ring type
  IntCarrier : Ring type

{-    
public export
data Schema :  List NSVar  -> Type where
  Ref :  (x:NSVar ) -> Schema  vars
  RefSt : (x:NSVar ) -> 
          (y:NSVar ) -> 
          (scope: Schema (x :: vars))  ->
          Schema vars
  Seq : (x:NSVar ) -> 
        (seq:Sequence (Schema vars) ) ->  
        (scope: Schema (x :: vars)) -> 
        Schema vars 
  Rel : (x:NSVar ) -> 
        (rel:Relation (Schema vars) ) -> 
        (scope: Schema (x :: vars)) -> 
        Schema vars   
  Si  : (xx:NSVar ) -> 
        (si:SimpleT )   ->
        (scope:Schema (xx :: vars)) -> 
        Schema vars
  VRing : (q:NSVar ) -> 
          (x:NSVar ) -> 
          Ring (Schema vars) -> 
          (scope: Schema (q :: vars))  -> 
          Schema vars
  OP : Schema vars
-}

public export
data Schema :  Type where
  Ref :  (x:NSVar ) -> Schema 
  RefSt : (x:NSVar ) -> 
          (y:NSVar ) -> 
          Schema 
  Seq : (x:NSVar ) -> 
        (seq:Sequence Schema  ) ->  
        Schema 
  Rel : (x:NSVar ) -> 
        (rel:Relation Schema ) -> 
        Schema 
  Si  : (xx:NSVar ) -> 
        (si:SimpleT )   ->
        Schema 
  VRing : (q:NSVar ) -> 
          (x:NSVar ) -> 
          Ring Schema -> 
          Schema 
  OP : Schema 



public export
s : List Schema
s = [Rel cy (M2O (Ref a) (Ref cy_ty)),
     Rel sku (M2O (Ref a) (Ref sku_ty)),
     Rel items ( (Ref cy) .|. (Ref sku) ),

     VRing qty items IntCarrier,
     VRing unit items IntCarrier,
     VRing price items (Mult (Ref qty) (Ref unit) ),
     VRing tot cy (NTran (Ref price))
     ]

  
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
  
  

