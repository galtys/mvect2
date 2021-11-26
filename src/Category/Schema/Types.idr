module Category.Schema.Types

import Generics.Derive
import Data.SortedMap
import JSON

%language ElabReflection

public export
data SDoc : Type where
   Line : (i:Bits32) -> (t:String) -> SDoc
   Def : (lines:List SDoc) -> SDoc
   Sep : SDoc
   
%runElab derive "SDoc" [Generic, Meta, Eq, Ord, Show,ToJSON,FromJSON]
   
namespace OE
   public export
   data PrimTypes = I_Bits32|I_Price|I_Date|I_String|I_EQty|I_Bool
   %runElab derive "PrimTypes" [Generic, Meta, Eq, Ord, EnumToJSON,EnumFromJSON]
   
   export
   pt_show : PrimTypes -> String
   pt_show I_Bits32 = "Bits32"
   pt_show I_Price = "Price"
   pt_show I_Date = "Date"
   pt_show I_String = "String"
   pt_show I_EQty = "EQty"
   pt_show I_Bool = "Bool"
   
   export
   Show PrimTypes where
     show = pt_show
   
   public export
   data PgTypes = BigInt | Text | DoublePrecision | VarChar Int|Boolean
   %runElab derive "PgTypes" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]

   public export
   record TableName where
     constructor MkTN
     ref : String     
     dbtable : String
     m : String
     isM2M : Bool
   %runElab derive "TableName" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
   
   public export
   data IsNull = Nullable | NotNull 

   isNull_show : IsNull -> String
   isNull_show Nullable = "nullable"
   isNull_show NotNull = "notNull"   
   
   public export
   showJust : IsNull -> String
   showJust Nullable = "Just"
   showJust NotNull = ""
   
   
   public export
   Show IsNull where
     show = isNull_show

   %runElab derive "IsNull" [Generic, Meta, Eq, Ord, EnumToJSON,EnumFromJSON]
   
   
   public export
   record Field where
     constructor MkF
     isNull : IsNull
     primType : PrimTypes          
     name : String
--     db_field : String
     pg_type : PgTypes
     castTo : String --ToPG
     castFrom : String --FromPG
     table : TableName
   %runElab derive "Field" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]  
   
   public export
   data RelationType = Tm2o | To2m | Tm2m
   %runElab derive "RelationType" [Generic, Meta, Eq, Ord, Show,EnumToJSON,EnumFromJSON]            
      
   public export
   data Schema : Type where
     Pk : (name:String) -> (db_field:String) -> (table:TableName) -> Schema
     Prim : (prim:OE.Field) -> Schema
     M2O : (rel: TableName) -> (db_field:Field) ->(table:TableName) -> Schema
     O2M : (rec_field:String) -> (rel_f:Field) -> (tn: TableName) -> Schema
     M2M : (rec_field:String) -> (f1:OE.Field) -> (f2:OE.Field) -> (m2m_table:TableName) -> (tn: TableName) -> Schema
     Model : (table:TableName) -> (fields:List Schema) -> Schema
     Sch : (name:String) -> (models: List Schema) -> Schema
     
   %runElab derive "Schema" [Generic, Meta, Eq, Ord, Show,ToJSON,FromJSON]            
   public export
   data View : Type where
     ListView : View -> View --Should be one Group of fields
     FormView :  List View -> View
     
     Notebook : List View -> View -- for form view
     Page : View -> View     -- for Notebook (or paginated document)
     FieldGroup : List Schema -> View -- List of fields in a group
     Separator : View -- separator
          
   %runElab derive "View" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]              
   public export
   data Menu : Type where
      MenuItem : (view:View) -> Menu
      MenuSub : (items:List Menu) -> Menu
     
   
   public export
   data SchemaTree : Type where
     MkST : (tn:Maybe TableName) -> (links:List SchemaTree) -> SchemaTree
  
   %runElab derive "SchemaTree" [Generic, Meta, Show, Eq,ToJSON,FromJSON]


