module Ledger.Schema.Types

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
     m : String
   %runElab derive "TableName" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
   
   add_quote : String -> String
   add_quote x = "\"" ++ x ++ "\""
   
   {-
   export
   table_ref : TableName -> String
   table_ref (MkTN ref dbtable m) = ref
   -}
   export
   getPrimModelNs : TableName -> String
   getPrimModelNs (MkTN ref dbtable m) = ("Prim"++m)
   
   export
   tn_show : TableName -> SDoc
   tn_show (MkTN ref dbtable m) = Def [(Line 0 #"\#{ref}:String"#),
                                     (Line 0 #"\#{ref} = \#{add_quote dbtable}"#)]   
   
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
   field2Ref (MkF isNull primType name pg_type castTo castFrom (MkTN ref dbtable m)) = (db_field2Ref (id2pk name))++"_"++(ref)

   export
   indentSDoc : Bits32 -> SDoc -> SDoc
   indentSDoc x (Line i t) = (Line (i+x) t)
   indentSDoc x (Def lines) = Def [ indentSDoc x l | l <- lines]
   indentSDoc x Sep = Sep
   
   export   
   field_show : Field -> SDoc         
   field_show f@(MkF isNull primType name pg_type castTo castFrom t@(MkTN ref dbtable m)) = Def [(Line 0 #"\#{field2Ref f}:Column"#),
                                                                                                 (Line 0 #"\#{field2Ref f}=\#{df}"#)] where
         df:String
         df=(show isNull)++" "++(show primType)++" "++(add_quote name)++" ("++(show pg_type)++") "++castTo++" "++castFrom++" "++(ref)
   
   public export
   data Schema : Type where
     Pk : (name:String) -> (db_field:String) -> (table:TableName) -> Schema
     Prim : (prim:OE.Field) -> Schema --prim field
     M2O : (rel: TableName) -> (db_field:String) ->(table:TableName) -> Schema -- ->(col : OE.Field)
     O2M : (db_field:String) -> (tn: TableName) -> Schema
     M2M : (f1:String) -> (f2:String) -> (tn: TableName) -> Schema
     --(table:TableName)->(pk:Schema)->
     Model : (table:TableName) -> (fields:List Schema) -> Schema
     Sch : (name:String) -> (models: List Schema) -> Schema
     
   export
   getPK_Field : String -> TableName -> Field
   getPK_Field db_field table = (MkF NotNull I_Bits32 db_field BigInt "(Just . cast)" "cast" table)
   
   export
   getPrimFields : Schema -> List (Maybe Field)
   getPrimFields (Pk name db_field table) = [Just (getPK_Field db_field table)]
   getPrimFields (Prim prim) = [Just prim]
   getPrimFields (M2O rel db_field table) = [Nothing]
   getPrimFields (O2M db_field tn) = [Nothing]
   getPrimFields (M2M f1 f2 tn) = [Nothing]
   getPrimFields (Model table fields) = concat (map getPrimFields fields)
   getPrimFields (Sch name models) = []
   
   export
   getFieldRefs : List (Maybe Field) -> List String
   getFieldRefs [] = []
   getFieldRefs (Nothing :: xs) = (getFieldRefs xs)
   getFieldRefs ((Just x) :: xs) = [(field2Ref x)]++(getFieldRefs xs)
   

   public export
   getPrimSDoc : Schema -> SDoc
   getPrimSDoc (Pk name db_field table) = Line 4 #"\#{id2pk db_field}:(idrisTpe \#{field2Ref f})"# where
      f : Field
      f = (getPK_Field db_field table)      
   getPrimSDoc (Prim prim) = Line 4 #"\#{id2pk (name prim)}:(idrisTpe \#{field2Ref prim})"# 
   
   getPrimSDoc (M2O rel db_field table) = Line 4 "--M2O"
   getPrimSDoc (O2M db_field tn) = Line 4 "--O2M"
   getPrimSDoc (M2M f1 f2 tn) = Line 4 "M2M"
   getPrimSDoc mod@(Model table fields) = Def [Sep,ns,Sep,primTab,Sep,rec,elabRec,np2Rec,Sep,read_rec_c,Sep,read_rec,main_read] where
      getPrimRecName : TableName -> String
      getPrimRecName tn = (getPrimModelNs tn)++".RecordPrim"
      getDomN : TableName -> String
      getDomN tn = (getPrimModelNs tn)++".domain"
      
      primColsR : TableName -> String
      primColsR tn = (getPrimModelNs tn)++".PrimCols"
      
      getTableR : TableName -> String
      getTableR tn = (ref tn)++"_NP"
      
      --table_Ref : String
      --table_Ref = (getTable4 table)
      
      fs : List String
      fs = getFieldRefs (getPrimFields mod)
      cols : String
      cols = fastConcat $ intersperse ", " fs
      ns : SDoc 
      ns = Def [Line 0 #"namespace \#{getPrimModelNs table}"#,
                Line 2 "domain : Op",
                Line 2 "domain = (True)",
                Line 2 "PrimCols : List Column",
                Line 2 #"PrimCols = [\#{cols}]"# ]
      primTab : SDoc
      primTab = Def [Line 2 #"\#{getTableR table} : Table"#,
                     Line 2 #"\#{getTableR table} = MkTable \#{add_quote (dbtable table)} \#{primColsR table}"#]

      rec : SDoc
      rec = Def ([Line 2 "record RecordPrim where",Line 4 "constructor MkRecordPrim"]++(map getPrimSDoc fields))
      elabRec : SDoc
      elabRec = Line 2 #"%runElab derive \#{add_quote (getPrimRecName table)} [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]"#      
      np2Rec :SDoc
      np2Rec = Def [Sep,
                    Line 2 #"toRecord : GetRow \#{getPrimModelNs table}.PrimCols -> \#{getPrimRecName table}"#,
                    Line 2 "toRecord = to . (\\x => MkSOP $ Z x)"]
      read_rec_c : SDoc
      read_rec_c = Def [Line 2  "export",
                        Line 2 #"read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List \#{getPrimRecName table} )"#,
                        Line 2  "read_records_c c op = do",
                        Line 4     #"rows <- get c \#{getTableR table} (columns \#{getTableR table}) (\#{getDomN table}&&op)"#,
                        Line 4     #"let ret_s = [ \#{getPrimModelNs table}.toRecord ox | ox <- rows]"#,
                        Line 4      "pure ret_s"]
      read_rec : SDoc
      read_rec = Def [Line 2  "export",
                      Line 2 #"read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List \#{getPrimRecName table} )"#,
                      Line 2  "read_records op = do",
                      Line 4     "c <- connect DB_URI",
                      Line 4     #"ret <- \#{getPrimModelNs table}.read_records_c c op"#,
                      Line 4     "pure ret"]
      main_read : SDoc
      main_read = Def [Sep,Line 2 "export",
                       Line 2 #"main_runET : (op:Op) -> IO (List \#{getPrimRecName table} )"#,
                       Line 2 #"main_runET op = do "#,
                       Line 4 #"Left err <- runEitherT (\#{getPrimModelNs table}.read_records op {io = EitherT SQLError IO} )"#,
                       Line 5     "| Right l1 => pure l1",
                       Line 4 "printLn err",
                       Line 4 "pure []",
                       Sep,
                       Line 2 "export",
                       Line 2 #"read : HasIO io => (op:Op) -> io (List \#{getPrimRecName table} )"#,
                       Line 2  "read op = do",
                       Line 4 #"l1 <- (liftIO $ (\#{getPrimModelNs table}.main_runET op))"#,
                       Line 4 "pure l1"]
      
      
   getPrimSDoc (Sch name models) = Sep
{-
  main_runET : (op:Op) -> IO (List RecordCols)
  main_runET op = do Left err <- runEitherT (read_records op {io = EitherT SQLError IO} )
                       | Right l1 => pure l1
                     printLn err
                     pure []

  export
  read : HasIO io => (op:Op) -> io (List RecordCols)
  read op = do  
     l1 <- (liftIO $ main_runET op)
     pure l1
-}   
   public export
   schema_tables : Schema -> List TableName
   schema_tables (Pk name db_field table) = [table]
   schema_tables (Prim prim) = []
   schema_tables (M2O rel db_field table) = []
   schema_tables (O2M db_field tn) = []
   schema_tables (M2M f1 f2 tn) = []
   schema_tables (Model tn []) = []
   schema_tables (Model tn (x :: xs)) = (schema_tables x) ++ (schema_tables (Model tn xs))
   schema_tables (Sch n []) = []
   schema_tables (Sch n (x::xs) ) = (schema_tables x) ++ (schema_tables (Sch n xs))
   
   export
   schema_show : Schema -> SDoc
   schema_show (Pk name db_field table) = field_show $ getPK_Field db_field table
   schema_show (Prim prim) = field_show $ prim
   schema_show (M2O rel db_field table) = Line 0 "--M2O"
   schema_show (O2M db_field tn) = Line 0 "--O2M"
   schema_show (M2M f1 f2 tn) = Line 0 "--M2M"
   schema_show (Model tn []) = Def [] 
   schema_show (Model tn xs) = Def ([Sep]++(map schema_show xs))
   schema_show (Sch n []) = Def []
   schema_show s@(Sch n xs) = Def [s_imp,t_names,modules,rec] where       
       s_imp:SDoc
       s_imp=Def [Line 0 #"module \#{n}"#, Sep,
              Line 0 "import PQ.CRUD",
              Line 0 "import PQ.FFI",
              Line 0 "import PQ.Schema",
              Line 0 "import PQ.Types", Sep,
              Line 0 "import Category.Transaction.Types",
              Line 0 "import Data.Ratio",Sep,
              Line 0 "import Generics.Derive",Sep,
              Line 0 "import JSON",Sep,
              Line 0 "import Ledger.PG.Config",
              Line 0 "import Control.Monad.Either",Sep,
              Line 0 "%language ElabReflection"]
              
       t_names:SDoc
       t_names = Def (map tn_show (schema_tables s))
       modules:SDoc
       modules = Def (map schema_show xs)
       rec : SDoc
       rec = Def (map getPrimSDoc xs)

       
     
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

{-
   public export
   validateSchema : Schema -> Bool
   validateSchema (Pk pk dbf t) = True
   validateSchema (Prim pk) = True   
   validateSchema (M2O rel f t) = True
   validateSchema (O2M dbf model) = True
   validateSchema (M2M f1 f2 model) = True
   validateSchema (Model fields) = ?validateSchema_rhs_5
   validateSchema (Sch models) = ?validateSchema_rhs_6

-}
