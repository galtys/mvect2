module Demo.DemoCompany

public export
NameSpace : Type
NameSpace = String

data NameType : Type where
  Msg : NameType  -- message 
  St  : NameType  -- state
  Vn  : NameType  -- vector name
  
public export
data NSVar : NameType -> Type where
  VarMsg : String -> NameSpace-> NSVar Msg
  VarSt  :String -> NameSpace-> NSVar St
  V : String -> NSVar Vn 

public export
qty : NSVar Vn
qty = V "qty"

public export
unit : NSVar Vn
unit = V "unit"

public export
price : NSVar Vn
price = V "price"

public export
tot : NSVar Vn
tot = V "total"


public export
sku : NSVar Msg
sku = VarMsg "sku" "Asset"

public export
cy : NSVar Msg
cy = VarMsg "cy" "Asset"

public export
a : NSVar Msg
a = VarMsg "a" "Asset"

public export
cy_ty : NSVar Msg 
cy_ty = VarMsg "cy_ty" "CurrencyType"

public export
sku_ty : NSVar Msg
sku_ty = VarMsg "sku_ty" "StockType"

public export
items : NSVar Msg
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
    
public export
data Schema :  Type where
  Ref :  (x:NSVar nt) -> Schema  -- add   Vuse ->  idea is to declare a message and then turn it into msg or state in the exec env
  RefSt : (x:NSVar St) -> (x:NSVar Msg) -> Schema --convert msg to state
  Seq : (x:NSVar Msg) -> (seq:Sequence Schema ) ->  Schema 
  Rel : (x:NSVar Msg) -> (rel:Relation Schema )  -> Schema   
  Si  : (x:NSVar Msg) -> (si:SimpleT )   -> Schema   
  VRing : (q:NSVar Vn) -> (x:NSVar nt) -> Ring (Schema ) -> Schema
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
  
  

