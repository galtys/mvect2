module Ledger.Schema.Types

import Generics.Derive
import Data.SortedMap
import JSON

%language ElabReflection

namespace OE
   public export
   data PrimTypes = I_Bits32|I_Price|I_Date|I_String|I_TQty|I_Bool
   %runElab derive "PrimTypes" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON] 
   public export
   data PgTypes = BigInt | Text | DoublePrecision | VarChar Int|Boolean
   %runElab derive "PgTypes" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]
   public export
   data ToPG = Nto String -- will be Name of the function
   %runElab derive "ToPG" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]
   public export   
   data FromPG = Nfrom String 
   %runElab derive "FromPG" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]   

   public export
   record TableName where
     constructor MkTN
     ref : String     
     dbtable : String
   %runElab derive "TableName" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]       
   public export
   data IsNull = Nullable | NotNull
   %runElab derive "IsNull" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]
   
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
   data Schema : Type where
     --Ref : (ref:String) -> Schema
     Pk : (name:String) -> (db_field:String) -> (table:TableName) -> Schema
     Prim : (prim:OE.Field) -> Schema --prim field
     M2O : (tn: TableName) -> (col : OE.Field) -> Schema
     O2M : (db_field:String) -> (tn: TableName) -> Schema
     M2M : (tn: TableName) -> Schema
     --(table:TableName)->(pk:Schema)->
     Model : (fields:List Schema) -> Schema
     Sch : (models: List Schema) -> Schema
   %runElab derive "Schema" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]             
   
   public export
   data View : Type where
     FieldGroup : List Schema -> View -- List of fields in a group
     ListView : View -> View --Should be one Group of fields
     Separator : View -> View -- separator
     FormView :  List View -> View
     Notebook : View -> View -- for form view
     Page : View -> View     -- for Notebook (or paginated document)
     
   %runElab derive "View" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]             
   
   public export
   data Menu : Type where
      MenuItem : (view:View) -> Menu
      Parent : (items:List Menu) -> Menu
      

   public export
   validateSchema : Schema -> Bool
   --validateSchema (Ref x) = ?mufds
   validateSchema (Pk pk dbf t) = True
   validateSchema (Prim pk) = True   
   validateSchema (M2O model col) = ?validateSchema_rhs_2
   validateSchema (O2M dbf model) = ?validateSchema_rhs_3
   validateSchema (M2M model) = ?validateSchema_rhs_4
   validateSchema (Model fields) = ?validateSchema_rhs_5
   validateSchema (Sch models) = ?validateSchema_rhs_6

