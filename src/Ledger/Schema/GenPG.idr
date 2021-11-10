module Ledger.Schema.GenPG

import Generics.Derive
import Data.SortedMap
import Data.String

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
primRecRef tn = (primModelRef tn)++".RecordModel"

export
o2mRecRef : TableName -> String
o2mRecRef tn = (o2mModelRef tn)++".RecordModel"

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

split_by : Char -> Bool
split_by '_' = True
split_by _ = False

capitalize : List Char -> String
capitalize [] = ""
capitalize (x::xs) = pack ([(toUpper x)]++xs)

db_field2Ref : String -> String
db_field2Ref x =  concat (map capitalize (map unpack (split split_by x))) --pack (map toUpper (unpack x))

export
fieldRef : Field -> String
fieldRef (MkF isNull primType name pg_type castTo castFrom (MkTN ref dbtable m)) = (db_field2Ref (id2pk name))++(ref)


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
getPrimFields (Prim prim) =              [Just prim]
getPrimFields (M2O rel db_field table) = [Just (getPK_Field db_field table)]

getPrimFields (O2M rec_field rel_f tn) = []
getPrimFields (M2M rec_field f1 f2 m2m_table tn) =           []
getPrimFields (Model table fields) = concat (map getPrimFields fields)
getPrimFields (Sch name models) = []

export
getRelRecFields : Schema -> List (String)
getRelRecFields (Pk name db_field table) = [(id2pk db_field)]
getRelRecFields (Prim prim) = [(name prim)]
getRelRecFields (M2O rel db_field table) = [(id2pk db_field)]

getRelRecFields (O2M rec_field rel_f tn) = [(id2pk rec_field)]
getRelRecFields (M2M rec_field f1 f2 m2m_table tn)  = [(id2pk rec_field)]
getRelRecFields (Model table fields) = concat (map getRelRecFields fields)
getRelRecFields (Sch name models) = []


{-
export
getO2MFields : Schema -> List Schema
getO2MFields (Pk name db_field table) = []
getO2MFields (Prim prim) = [] 
getO2MFields (M2O rel db_field table) = [] 
getO2MFields o2m@(O2M db_field tn) = [o2m] 
getO2MFields (M2M f1 f2 tn) = []
getO2MFields (Model table fields) = concat (map getO2MFields fields)
getO2MFields (Sch name models) = []
-}

isO2M : Schema -> Bool
isO2M (Pk name db_field table) = False
isO2M (Prim prim) = False
isO2M (M2O rel db_field table) = False
isO2M (O2M rec_field rel_f tn) = True
isO2M (M2M rec_field f1 f2 m2m_table tn)= False
isO2M (Model table fields) = False
isO2M (Sch name models) = False

isM2M : Schema -> Bool
isM2M (Pk name db_field table) = False
isM2M (Prim prim) = False
isM2M (M2O rel db_field table) = False
isM2M (O2M rec_field rel_f tn) = False
isM2M (M2M rec_field f1 f2 m2m_table tn)= True
isM2M (Model table fields) = False
isM2M (Sch name models) = False

isM2O : Schema -> Bool
isM2O (Pk name db_field table) = False
isM2O (Prim prim) = False
isM2O (M2O rel db_field table) = True
isM2O (O2M rec_field rel_f tn) = False
isM2O (M2M rec_field f1 f2 m2m_table tn)= False
isM2O (Model table fields) = False
isM2O (Sch name models) = False

model2Fields : List Schema -> List Field
model2Fields [] = []
model2Fields ((Pk name db_field table)::xs) = [(getPK_Field db_field table)]++(model2Fields xs)
model2Fields ((Prim prim)::xs) = [prim]++(model2Fields xs)
model2Fields ((M2O rel db_field table)::xs) = [(getPK_Field db_field table)]++(model2Fields xs)
model2Fields ((O2M rec_field rel_f tn)::xs) = []
model2Fields ((M2M rec_field f1 f2 m2m_table tn)::xs) = []
model2Fields ((Model table fields)::xs) = []
model2Fields ((Sch name models)::xs) = []


--fromMaybeString : List (Maybe String) -> List String


export
getFieldRefs : List (Maybe Field) -> List String
getFieldRefs [] = []
getFieldRefs (Nothing :: xs) = []
getFieldRefs ((Just x) :: xs) = [fieldRef x]++(getFieldRefs xs)

export
getFields : List (Maybe Field) -> List Field
getFields [] = []
getFields (Nothing :: xs) = (getFields xs)
getFields ((Just x) :: xs) = [x]++(getFields xs)

public export
getPrimSDoc : Schema -> SDoc
getPrimSDoc (Pk name db_field table) = Line 4 #"\#{id2pk db_field}:(idrisTpe \#{fieldRef f})"# where
   f : Field
   f = (getPK_Field db_field table)      
getPrimSDoc (Prim prim) = Line 4 #"\#{id2pk (name prim)}:(idrisTpe \#{fieldRef prim})"# 
getPrimSDoc (M2O rel db_field table) = Line 4 #"\#{id2pk db_field}:(idrisTpe \#{fieldRef f})"# where
   f : Field
   f = (getPK_Field db_field table)    
getPrimSDoc (O2M rec_field rel_f tn) = Line 4 "--O2M"
getPrimSDoc (M2M rec_field f1 f2 m2m_table tn) = Line 4 "--M2M"
getPrimSDoc mod@(Model table fields) = Def [Sep,ns,Sep,primTab,Sep,rec,elabRec,
                                            np2Rec,Sep,read_rec_c,Sep,read_rec,main_read] where
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
   rec = Def ([Line 2 "record RecordModel where",Line 4 "constructor MkRecordModel"]++(map getPrimSDoc fields))
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
schema_tables (O2M rec_field rel_f tn) = []
schema_tables (M2M rec_field f1 f2 m2m_table tn)= [m2m_table]
schema_tables (Model tn []) = []
schema_tables (Model tn (x :: xs)) = (schema_tables x) ++ (schema_tables (Model tn xs))
schema_tables (Sch n []) = []
schema_tables (Sch n (x::xs) ) = (schema_tables x) ++ (schema_tables (Sch n xs))

export
showPrim : Schema -> SDoc
showPrim (Pk name db_field table) = field_show $ getPK_Field db_field table
showPrim (Prim prim) = field_show $ prim
showPrim (M2O rel db_field table) = field_show $ getPK_Field db_field table
showPrim (O2M rec_field rel_f tn) = Line 0 "--O2M"
showPrim (M2M rec_field f1 f2 m2m_table tn) = Line 0 "--M2M"
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


zipN : List String -> List Bool -> List Int -> List (String,Bool,Int)
zipN xs ys zs = ?zipN_rhs

{-
          add_lines : (List PrimOrder.RecordModel)  -> io (List O2MOrder.RecordModel)
          add_lines [] = pure []
          add_lines ((PrimOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id requested_date)::xs) =do 
                    order_line  <- PrimOrderLine.read_records_c c ((ORDER_ID_OLT==(cast pk))&&op)
                    order_line2 <- PrimOrderLine.read_records_c c ((ORDER_ID_OLT==(cast pk))&&op)
                    
                    let ret = (O2MOrder.MkRecordModel pk origin order_policy date_order partner_id amount_tax state partner_invoice_id amount_untaxed amount_total name partner_shipping_id picking_policy carrier_id order_line requested_date order_line2)
                    ret_xs <- add_lines xs
                    pure ([ret]++ret_xs)

          ret_x : io (List O2MOrder.RecordModel)
          ret_x = do
            rows <- PrimOrder.read_records_c c op

            ret1 <- add_lines rows
            pure ret1 
-}
export
getRelO2m : Schema -> SDoc
getRelO2m (Pk name db_field table) = Line 4 #"\#{id2pk db_field}:(idrisTpe \#{fieldRef f})"# where
   f : Field
   f = (getPK_Field db_field table)    
getRelO2m (Prim prim) = Line 4 #"\#{id2pk (name prim)}:(idrisTpe \#{fieldRef prim})"# 
getRelO2m (M2O rel db_field table) = Line 4 #"\#{id2pk db_field}:List \#{primRecRef rel}"# where
   f : Field
   f = (getPK_Field db_field table)    
getRelO2m (O2M rec_field rel_f tn) = Line 4 #"\#{id2pk rec_field}:List \#{o2mRecRef tn}"#
getRelO2m (M2M rec_field f1 f2 m2m_table tn) = Line 4 #"\#{id2pk rec_field}:List \#{primRecRef tn}"#
getRelO2m mod@(Model table fields) = Def [Sep,ns,rec,elabRec,read_rec_c,add_muf,ret_x,read_rec,main_read,read_rec_ids,main_read_ids] where
   o2m_fields : List Schema
   o2m_fields = (filter isO2M fields)
   m2m_fields : List Schema
   m2m_fields = (filter isM2M fields)
   m2o_fields : List Schema
   m2o_fields = (filter isM2O fields)
   pkRef : String
   pkRef = (fieldRef (getPK_Field "pk" table) )
   ns : SDoc
   ns = Def [Line 0 #"namespace \#{o2mModelRef table}"#,
              Line 2 "domain : Op",
              Line 2 "domain = (True)"]
   rec : SDoc
   rec = Def ([Line 2 "record RecordModel where",Line 4 "constructor MkRecordModel"]++(map getRelO2m fields))
   elabRec : SDoc
   elabRec = Line 2 #"%runElab derive \#{add_quotes (o2mRecRef table)} [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]"#      

      
   rel_relRef : List Schema -> List (String,TableName,String)
   rel_relRef [] = []
   rel_relRef ((Pk name db_field x) :: xs) = []
   rel_relRef ((Prim prim) :: xs) = []
   rel_relRef ((M2O rel db_field x) :: xs) = [ (db_field,rel, (fieldRef (getPK_Field "pk" rel) ) )]++(rel_relRef xs)   
   rel_relRef ((O2M rec_field rel_f tn) :: xs) = [ (rec_field,tn, (fieldRef (getPK_Field rel_f tn) ) )]++(rel_relRef xs)
   rel_relRef ((M2M rec_field f1 f2 m2m_table tn) :: xs) = [ (rec_field,tn,dbtable m2m_table )]++(rel_relRef xs)
   rel_relRef ((Model x ys) :: xs) = []
   rel_relRef ((Sch name models) :: xs) = []
   
   rel_rec_names : String
   rel_rec_names = (fastConcat $ intersperse " " [ #"\#{rec_f}"# | (rec_f,rel,col) <- rel_relRef m2m_fields])
   
   ||| 
   primFieldNames : String
   primFieldNames = fastConcat $ intersperse " " (getFieldRefs (getPrimFields mod))
   
   dbFields : String
   dbFields = fastConcat $ intersperse " " (map id2pk (map name (getFields (getPrimFields mod))))
   
   relFields : String
   relFields = fastConcat $ intersperse " " (getRelRecFields mod)
   
   read_o2m : (rec_field:String) -> (col:String) -> TableName -> SDoc
   read_o2m rec_field col rel = Line 5 #"\#{rec_field} <- \#{o2mModelRef rel}.read_records_c c ((\#{col}==(cast pk))&&op)"#   
   read_o2m_SDoc : SDoc
   read_o2m_SDoc  = Def ([ (read_o2m db_f col rel) | (db_f,rel,col) <- rel_relRef o2m_fields ] )


   rel_relRefM2M : List Schema -> List (String,TableName,TableName,String)
   rel_relRefM2M [] = []
   rel_relRefM2M ((Pk name db_field x) :: xs) = []
   rel_relRefM2M ((Prim prim) :: xs) = []
   rel_relRefM2M ((M2O rel db_field x) :: xs) = []++(rel_relRefM2M xs)   
   rel_relRefM2M ((O2M rec_field rel_f tn) :: xs) = []++(rel_relRefM2M xs)
   rel_relRefM2M ((M2M rec_field f1 f2 m2m_table tn) :: xs) = [ (rec_field,tn,m2m_table,fieldRef f1)]++(rel_relRefM2M xs)
   rel_relRefM2M ((Model x ys) :: xs) = []
   rel_relRefM2M ((Sch name models) :: xs) = []
      
   read_m2m : (rec_field:String) -> (col:String) -> TableName -> TableName -> SDoc
   read_m2m rec_field col rel m2mt= Def [Line 5 #"\#{rec_field}_np <- getJoin c \#{tableRef rel} \#{tableRef m2mt} (columns \#{tableRef rel}) ((JC \#{pkRef} \#{col})&&(\#{pkRef}==(cast pk))&&op)"#,
                                     Line 5 #"let \#{rec_field}=[\#{primModelRef rel}.toRecord ox |ox <-\#{rec_field}_np]"#]
                                     
   read_m2m_SDoc : SDoc
   read_m2m_SDoc  = Def ([ (read_m2m db_f col rel m2mt) | (db_f,rel,m2mt,col) <- rel_relRefM2M m2m_fields ] )
   
   read_m2o : (rec_field:String) -> (col:String) -> TableName -> SDoc
   read_m2o rec_field col rel = Line 5 #"\#{rec_field} <- \#{primModelRef rel}.read_records_c c ((\#{col}==(cast \#{rec_field}))&&op)"#
   read_m2o_SDoc : SDoc
   read_m2o_SDoc  = Def ([ (read_m2o db_f col rel) | (db_f,rel,col) <- rel_relRef m2o_fields ] )
   
   read_rec_c : SDoc
   read_rec_c = Def [Line 2  "export",
                     Line 2 #"read_records_c : HasIO io => MonadError SQLError io => Connection -> (op:Op)->io (List \#{o2mRecRef table} )"#,
                     Line 2  "read_records_c c op = ret_x where"]      
   add_muf : SDoc
   add_muf = Def [Sep,Line 4 #"add_lines : (List \#{primRecRef table}) ->io (List  \#{o2mRecRef table})"#,
                      Line 4  "add_lines [] = pure []",
                      Line 4 #"add_lines ((\#{primModelRef table}.MkRecordModel \#{dbFields})::xs) = do"#,
                      read_o2m_SDoc,           
                      read_m2m_SDoc,
                      read_m2o_SDoc,
                      Line 5      #"let ret =(\#{o2mModelRef table}.MkRecordModel \#{relFields})"#,
                      Line 5      "ret_xs <- add_lines xs",
                      Line 5      "pure ([ret]++ret_xs)"]

   ret_x : SDoc
   ret_x = Def [Sep, Line 4 #"ret_x : io (List \#{o2mRecRef table})"#,
                     Line 4 "ret_x = do",
                       Line 5 #"rows <- \#{primModelRef table}.read_records_c c op"#,
                       Line 5 "ret1 <- add_lines rows",
                       Line 5 "pure ret1"]
   
   
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
   read_rec_ids : SDoc
   read_rec_ids = Def [Line 2  "export",
                   Line 2 #"read_records_ids : HasIO io => MonadError SQLError io => (op:Op)->io (List \#{o2mRecRef table} )"#,
                   Line 2  "read_records_ids op = do",
                   Line 4     "c <- connect DB_URI",
                   Line 4     #"ret <- \#{o2mModelRef table}.read_records_c c op"#,
                   Line 4     "pure ret"]
   main_read_ids : SDoc
   main_read_ids = Def [Sep,Line 2 "export",
                    Line 2 #"main_runET_ids : (op:Op) -> IO (List \#{o2mRecRef table} )"#,
                    Line 2 #"main_runET_ids op = do "#,
                    Line 4 #"Left err <- runEitherT (\#{o2mModelRef table}.read_records_ids op {io = EitherT SQLError IO} )"#,
                    Line 5     "| Right l1 => pure l1",
                    Line 4 "printLn err",
                    Line 4 "pure []",
                    Sep,
                    Line 2 "export",
                    Line 2 #"read_ids : HasIO io => (op:Op) -> io (List \#{o2mRecRef table} )"#,
                    Line 2  "read_ids op = do",
                    Line 4 #"l1 <- (liftIO $ (\#{o2mModelRef table}.main_runET_ids op))"#,
                    Line 4 "pure l1"]


             
getRelO2m (Sch name models) = Def (map getRelO2m models)

export
schema_show : Schema -> SDoc
schema_show s = Def [showPrim s, getRelO2m s]
