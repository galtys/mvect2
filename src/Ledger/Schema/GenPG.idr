module Ledger.Schema.GenPG

import Generics.Derive
import Data.SortedMap

import Ledger.Schema.Types

add_quotes : String -> String
add_quotes x = "\"" ++ x ++ "\""

--TableName

export
primModelRef : TableName -> String
primModelRef (MkTN ref dbtable m) = ("Prim"++m)

export
o2mModelRef : TableName -> String
o2mModelRef (MkTN ref dbtable m) = ("O2M"++m)

export
primRecRef : TableName -> String
primRecRef tn = (primModelRef tn)++".RecordPrim"

export
o2mRecRef : TableName -> String
o2mRecRef tn = (o2mModelRef tn)++".RecordO2M"

primDomainRef : TableName -> String
primDomainRef tn = (primModelRef tn)++".domain"


o2mDomainRef : TableName -> String
o2mDomainRef tn = (o2mModelRef tn)++".domain"

primColsRef : TableName -> String
primColsRef tn = (primModelRef tn)++".PrimCols"

tableRef : TableName -> String
tableRef tn = (ref tn)++"_NP"


--Field

export
id2pk : String -> String
id2pk x = if (x=="id") then "pk" else x

export
fieldRef : Field -> String
fieldRef (MkF isNull primType name pg_type castTo castFrom (MkTN ref dbtable m)) = (db_field2Ref (id2pk name))++"_"++(ref) where 
  db_field2Ref : String -> String
  db_field2Ref x = pack (map toUpper (unpack x))


export   
field_show : Field -> SDoc         
field_show f@(MkF isNull primType name pg_type castTo castFrom t@(MkTN ref dbtable m)) = Def [(Line 0 #"\#{fieldRef f}:Column"#),
                                                                                              (Line 0 #"\#{fieldRef f}=\#{df}"#)] where
      df:String
      df=(show isNull)++" "++(show primType)++" "++(add_quotes name)++" ("++(show pg_type)++") "++castTo++" "++castFrom++" "++(ref)

export
getPK_Field : String -> TableName -> Field
getPK_Field db_field table = (MkF NotNull I_Bits32 db_field BigInt "(Just . cast)" "cast" table)

export
getPrimFields : Schema -> List (Maybe Field)
getPrimFields (Pk name db_field table) = [Just (getPK_Field db_field table)]
getPrimFields (Prim prim) = [Just prim]
getPrimFields (M2O rel db_field table) = [Just (getPK_Field db_field table)]
getPrimFields (O2M db_field tn) = [Nothing]
getPrimFields (M2M f1 f2 tn) = [Nothing]
getPrimFields (Model table fields) = concat (map getPrimFields fields)
getPrimFields (Sch name models) = []

export
getFieldRefs : List (Maybe Field) -> List String
getFieldRefs [] = []
getFieldRefs (Nothing :: xs) = (getFieldRefs xs)
getFieldRefs ((Just x) :: xs) = [(fieldRef x)]++(getFieldRefs xs)


public export
getPrimSDoc : Schema -> SDoc
getPrimSDoc (Pk name db_field table) = Line 4 #"\#{id2pk db_field}:(idrisTpe \#{fieldRef f})"# where
   f : Field
   f = (getPK_Field db_field table)      
getPrimSDoc (Prim prim) = Line 4 #"\#{id2pk (name prim)}:(idrisTpe \#{fieldRef prim})"# 
getPrimSDoc (M2O rel db_field table) = Line 4 #"\#{id2pk db_field}:(idrisTpe \#{fieldRef f})"# where
   f : Field
   f = (getPK_Field db_field table)    
getPrimSDoc (O2M db_field tn) = Line 4 "--O2M"
getPrimSDoc (M2M f1 f2 tn) = Line 4 "M2M"
getPrimSDoc mod@(Model table fields) = Def [Sep,ns,Sep,primTab,Sep,rec,elabRec,
                                            np2Rec,Sep,read_rec_c,Sep,read_rec,main_read] where
   --table_Ref : String
   --table_Ref = (getTable4 table)

   fs : List String
   fs = getFieldRefs (getPrimFields mod)
   cols : String
   cols = fastConcat $ intersperse ", " fs
   ns : SDoc 
   ns = Def [Line 0 #"namespace \#{primModelRef table}"#,
             Line 2 "domain : Op",
             Line 2 "domain = (True)",
             Line 2 "PrimCols : List Column",
             Line 2 #"PrimCols = [\#{cols}]"# ]
   primTab : SDoc
   primTab = Def [Line 2 #"\#{tableRef table} : Table"#,
                  Line 2 #"\#{tableRef table} = MkTable \#{add_quotes (dbtable table)} \#{primColsRef table}"#]

   rec : SDoc
   rec = Def ([Line 2 "record RecordPrim where",Line 4 "constructor MkRecordPrim"]++(map getPrimSDoc fields))
   elabRec : SDoc
   elabRec = Line 2 #"%runElab derive \#{add_quotes (primRecRef table)} [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]"#      
   np2Rec :SDoc
   np2Rec = Def [Sep,
                 Line 2 #"toRecord : GetRow \#{primModelRef table}.PrimCols -> \#{primRecRef table}"#,
                 Line 2 "toRecord = to . (\\x => MkSOP $ Z x)"]
   read_rec_c : SDoc
   read_rec_c = Def [Line 2  "export",
                     Line 2 #"read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List \#{primRecRef table} )"#,
                     Line 2  "read_records_c c op = do",
                     Line 4     #"rows <- get c \#{tableRef table} (columns \#{tableRef table}) (\#{primDomainRef table}&&op)"#,
                     Line 4     #"let ret_s = [ \#{primModelRef table}.toRecord ox | ox <- rows]"#,
                     Line 4      "pure ret_s"]
   read_rec : SDoc
   read_rec = Def [Line 2  "export",
                   Line 2 #"read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List \#{primRecRef table} )"#,
                   Line 2  "read_records op = do",
                   Line 4     "c <- connect DB_URI",
                   Line 4     #"ret <- \#{primModelRef table}.read_records_c c op"#,
                   Line 4     "pure ret"]
   main_read : SDoc
   main_read = Def [Sep,Line 2 "export",
                    Line 2 #"main_runET : (op:Op) -> IO (List \#{primRecRef table} )"#,
                    Line 2 #"main_runET op = do "#,
                    Line 4 #"Left err <- runEitherT (\#{primModelRef table}.read_records op {io = EitherT SQLError IO} )"#,
                    Line 5     "| Right l1 => pure l1",
                    Line 4 "printLn err",
                    Line 4 "pure []",
                    Sep,
                    Line 2 "export",
                    Line 2 #"read : HasIO io => (op:Op) -> io (List \#{primRecRef table} )"#,
                    Line 2  "read op = do",
                    Line 4 #"l1 <- (liftIO $ (\#{primModelRef table}.main_runET op))"#,
                    Line 4 "pure l1"]
getPrimSDoc (Sch name models) = Sep


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
showPrim : Schema -> SDoc
showPrim (Pk name db_field table) = field_show $ getPK_Field db_field table
showPrim (Prim prim) = field_show $ prim
showPrim (M2O rel db_field table) = field_show $ getPK_Field db_field table
showPrim (O2M db_field tn) = Line 0 "--O2M"
showPrim (M2M f1 f2 tn) = Line 0 "--M2M"
showPrim (Model tn []) = Def [] 
showPrim (Model tn xs) = Def ([Sep]++(map showPrim xs))
showPrim (Sch n []) = Def []
showPrim s@(Sch n xs) = Def [s_imp,t_names,modules,prim] where       
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
    t_names = Def (map tn_show (schema_tables s)) where 
       tn_show : TableName -> SDoc
       tn_show (MkTN ref dbtable m) = Def [(Line 0 #"\#{ref}:String"#),
                                  (Line 0 #"\#{ref} = \#{add_quotes dbtable}"#)]   
    
    modules:SDoc
    modules = Def (map showPrim xs)
    prim : SDoc
    prim = Def (map getPrimSDoc xs)


export
getRelO2m : Schema -> SDoc
getRelO2m (Pk name db_field table) = Line 4 #"\#{id2pk db_field}:(idrisTpe \#{fieldRef f})"# where
   f : Field
   f = (getPK_Field db_field table)    
getRelO2m (Prim prim) = Line 4 #"\#{id2pk (name prim)}:(idrisTpe \#{fieldRef prim})"# 
getRelO2m (M2O rel db_field table) = Line 4 #"\#{id2pk db_field}:(idrisTpe \#{fieldRef f})"# where
   f : Field
   f = (getPK_Field db_field table)    
getRelO2m (O2M db_field tn) = Line 4 #"\#{id2pk db_field}:List \#{primRecRef tn}"#
getRelO2m (M2M f1 f2 tn) = Line 4 "M2M"
getRelO2m (Model table fields) = Def [Sep,ns,rec,elabRec,read_rec_c,read_rec,main_read] where
   ns : SDoc
   ns = Def [Line 0 #"namespace \#{o2mModelRef table}"#,
              Line 2 "domain : Op",
              Line 2 "domain = (True)"]
   rec : SDoc
   rec = Def ([Line 2 "record RecordO2M where",Line 4 "constructor MkRecordO2M"]++(map getRelO2m fields))
   elabRec : SDoc
   elabRec = Line 2 #"%runElab derive \#{add_quotes (o2mRecRef table)} [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]"#      
   
   read_rec_c : SDoc
   read_rec_c = Def [Line 2  "export",
                     Line 2 #"read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List \#{o2mRecRef table} )"#,
                     Line 2  "read_records_c c op = ?retddd"]
                     
   read_rec : SDoc
   read_rec = Def [Line 2  "export",
                   Line 2 #"read_records : HasIO io => MonadError SQLError io => (op:Op)->io (List \#{o2mRecRef table} )"#,
                   Line 2  "read_records op = do",
                   Line 4     "c <- connect DB_URI",
                   Line 4     #"ret <- \#{o2mModelRef table}.read_records_c c op"#,
                   Line 4     "pure ret"]
   main_read : SDoc
   main_read = Def [Sep,Line 2 "export",
                    Line 2 #"main_runET : (op:Op) -> IO (List \#{o2mRecRef table} )"#,
                    Line 2 #"main_runET op = do "#,
                    Line 4 #"Left err <- runEitherT (\#{o2mModelRef table}.read_records op {io = EitherT SQLError IO} )"#,
                    Line 5     "| Right l1 => pure l1",
                    Line 4 "printLn err",
                    Line 4 "pure []",
                    Sep,
                    Line 2 "export",
                    Line 2 #"read : HasIO io => (op:Op) -> io (List \#{o2mRecRef table} )"#,
                    Line 2  "read op = do",
                    Line 4 #"l1 <- (liftIO $ (\#{o2mModelRef table}.main_runET op))"#,
                    Line 4 "pure l1"]


             
getRelO2m (Sch name models) = Def (map getRelO2m models)

export
schema_show : Schema -> SDoc
schema_show s = Def [showPrim s, getRelO2m s]
