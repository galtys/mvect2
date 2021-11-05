module Ledger.Schema.Types

import Generics.Derive
import Data.SortedMap
import JSON

%language ElabReflection

data SDoc : Type where
   Line : (i:Int) -> (t:String) -> SDoc
   Def : (lines:List SDoc) -> SDoc
   Sep : SDoc
   
%runElab derive "SDoc" [Generic, Meta, Eq, Ord, ToJSON,FromJSON]
   
namespace OE
   public export
   data PrimTypes = I_Bits32|I_Price|I_Date|I_String|I_TQty|I_Bool
   %runElab derive "PrimTypes" [Generic, Meta, Eq, Ord, EnumToJSON,EnumFromJSON]
   
   export
   pt_show : PrimTypes -> String
   pt_show I_Bits32 = "Bits32"
   pt_show I_Price = "Price"
   pt_show I_Date = "Date"
   pt_show I_String = "String"
   pt_show I_TQty = "TQty"
   pt_show I_Bool = "Bool"
   
   Show PrimTypes where
     show = pt_show
   
   public export
   data PgTypes = BigInt | Text | DoublePrecision | VarChar Int|Boolean
   %runElab derive "PgTypes" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]

   {-
   public export   
   data ToPG = Nto String -- will be Name of the function
   %runElab derive "ToPG" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]
   public export   
   data FromPG = Nfrom String 
   %runElab derive "FromPG" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]   
   -}
   
   public export
   record TableName where
     constructor MkTN
     ref : String     
     dbtable : String
   %runElab derive "TableName" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
   
   tn_show : TableName -> SDoc
   tn_show (MkTN ref dbtable) = Def [(Line 0 #"\#{ref}:String"#),
                                     (Line 0 #"\#{ref} = \"\#{dbtable}\" "#)]   
   
   public export
   data IsNull = Nullable | NotNull --| PrimarySerial64
   %runElab derive "IsNull" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]
   
   isNull_show : IsNull -> String
   isNull_show Nullable = "nullable"
   isNull_show NotNull = "notNull"   
   --isNull_show PrimarySerial64 = "primarySerial64"
   
   Show IsNull where
     show = isNull_show
   
   db_field2Ref : String -> String
   db_field2Ref x = pack (map toUpper (unpack x))

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
   
   export
   id2pk : String -> String
   id2pk x = if (x=="id") then "pk" else x
   
   export
   field2Ref : Field -> String
   field2Ref (MkF isNull primType name pg_type castTo castFrom (MkTN ref dbtable)) = (db_field2Ref (id2pk name))++"_"++ref
   
   
   
   export   
   filed_show : Field -> SDoc         
   filed_show xf@(MkF isNull primType name pg_type castTo castFrom (MkTN ref dbtable)) = Def [(Line 0 #"\#{field2Ref xf}:Column"#),
                                                                              (Line 0 df)] where
         df:String
         df=(show isNull)++" "++(show primType)++" "++"("++(show pg_type)++")"++castTo++castFrom++ref
   
   public export
   data Schema : Type where
     Pk : (name:String) -> (db_field:String) -> (table:TableName) -> Schema
     Prim : (prim:OE.Field) -> Schema --prim field
     M2O : (rel: TableName) -> (db_field:String) ->(table:TableName) -> Schema -- ->(col : OE.Field)
     O2M : (db_field:String) -> (tn: TableName) -> Schema
     M2M : (f1:String) -> (f2:String) -> (tn: TableName) -> Schema
     --(table:TableName)->(pk:Schema)->
     Model : (fields:List Schema) -> Schema
     Sch : (models: List Schema) -> Schema
     
     
     
     
   %runElab derive "Schema" [Generic, Meta, Eq, Ord, Show,ToJSON,FromJSON]            
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
   validateSchema (Pk pk dbf t) = True
   validateSchema (Prim pk) = True   
   validateSchema (M2O rel f t) = True
   validateSchema (O2M dbf model) = True
   validateSchema (M2M f1 f2 model) = True
   validateSchema (Model fields) = ?validateSchema_rhs_5
   validateSchema (Sch models) = ?validateSchema_rhs_6

