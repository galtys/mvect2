module Ledger.Schema.GenPG

import Generics.Derive
--import Data.SortedMap
import Data.String

import Ledger.Schema.Types

add_quotes : String -> String
add_quotes x = "\"" ++ x ++ "\""

--TableName

export
primModelRef : TableName -> String
primModelRef (MkTN ref dbtable m ism2m) = ("Prim"++m)

export
o2mModelRef : TableName -> String
o2mModelRef (MkTN ref dbtable m ism2m) = ("O2M"++m)

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

export
getPK_Field : String -> TableName -> Field
getPK_Field db_field table = (MkF NotNull I_Bits32 db_field BigInt "(Just . cast)" "cast" table)

export
getPrimFields : Schema -> List (Maybe Field)
getPrimFields (Pk name db_field table) = [Just (getPK_Field db_field table)]
getPrimFields (Prim prim) =              [Just prim]
getPrimFields (M2O rel db_field table) = [Just (db_field)]

getPrimFields (O2M rec_field rel_f tn) = []
getPrimFields (M2M rec_field f1 f2 m2m_table tn) =           []
getPrimFields (Model table fields) = concat (map getPrimFields fields)
getPrimFields (Sch name models) = []

export
getRelRecFields : Schema -> List (String)
getRelRecFields (Pk name db_field table) = [(id2pk db_field)]
getRelRecFields (Prim prim) = [(name prim)]
getRelRecFields (M2O rel db_field table) = [(id2pk (name db_field))]

getRelRecFields (O2M rec_field rel_f tn) = [(id2pk rec_field)]
getRelRecFields (M2M rec_field f1 f2 m2m_table tn)  = [(id2pk rec_field)]
getRelRecFields (Model table fields) = concat (map getRelRecFields fields)
getRelRecFields (Sch name models) = []

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
model2Fields ((M2O rel db_field table)::xs) = [(db_field)]++(model2Fields xs)
model2Fields ((O2M rec_field rel_f tn)::xs) = []
model2Fields ((M2M rec_field f1 f2 m2m_table tn)::xs) = []
model2Fields ((Model table fields)::xs) = []
model2Fields ((Sch name models)::xs) = []

db_field2Ref : String -> String
db_field2Ref x =  concat (map capitalize (map unpack (split split_by x)))

export
columnRef : Field -> String
columnRef (MkF isNull primType name pg_type castTo castFrom (MkTN ref dbtable m ism2m)) = (db_field2Ref (id2pk name))++(ref)

export
columnIdrisType : Field -> String
columnIdrisType (MkF NotNull primType name pg_type castTo castFrom (MkTN ref dbtable m ism2m)) = ret where
    ret : String
    ret = #"\#{show primType}"#
columnIdrisType (MkF Nullable primType name pg_type castTo castFrom (MkTN ref dbtable m ism2m)) = ret where
    ret : String
    ret = #"(Maybe \#{show primType})"#

export
getFieldRefs : List (Maybe Field) -> List String
getFieldRefs [] = []
getFieldRefs (Nothing :: xs) = []
getFieldRefs ((Just x) :: xs) = [columnRef x]++(getFieldRefs xs)

export
getFields : List (Maybe Field) -> List Field
getFields [] = []
getFields (Nothing :: xs) = (getFields xs)
getFields ((Just x) :: xs) = [x]++(getFields xs)

export
genSchemaTree : (parent: String) -> OE.Schema -> List (String, RelationType,String,String) 
genSchemaTree p (Pk name db_field table)= []
genSchemaTree p (Prim prim) = []
genSchemaTree p (M2O rel db_field table)= [(p, Tm2o, (name db_field), dbtable rel) ]
genSchemaTree p (O2M rec_field rel_f tn)= [(p, To2m, rec_field,dbtable $table rel_f) ]
genSchemaTree p (M2M rec_field f1 f2 m2m_table tn)= [(p, Tm2m, rec_field,dbtable $table f2) ]
genSchemaTree p (Model table fields) = concat $ map (genSchemaTree (dbtable table) ) fields
genSchemaTree p (Sch name xs) = concat $ map (genSchemaTree p) xs

public export
showPrimRecDef : Schema -> SDoc
showPrimRecDef (Pk name db_field table) = Line 4 #"\#{id2pk db_field}:\#{columnIdrisType f}"# where
   f : Field
   f = (getPK_Field db_field table)      
showPrimRecDef (Prim prim) = Line 4 #"\#{id2pk (name prim)}:\#{columnIdrisType prim}"# 
showPrimRecDef (M2O rel db_field table) = Line 4 #"\#{id2pk (name db_field)}:\#{columnIdrisType f}"# where
   f : Field
   f = db_field 
showPrimRecDef (O2M rec_field rel_f tn) = Line 4 "--O2M"
showPrimRecDef (M2M rec_field f1 f2 m2m_table tn) = Line 4 "--M2M"
showPrimRecDef mod@(Model table fields) = Def [Sep,ns,Sep,rec,elabRec] where --read_rec_c,Sep,read_rec,main_read
   fs : List String
   fs = getFieldRefs (getPrimFields mod)
   cols : String
   cols = fastConcat $ intersperse ", " fs
   
   ns : SDoc 
   ns = Def [Line 0 #"namespace \#{primModelRef table}"#] 
   rec : SDoc
   rec = Def ([Line 2 "public export",Line 2 "record RecordModel where",Line 4 "constructor MkRecordModel"]++(map showPrimRecDef fields))
   elabRec : SDoc
   elabRec = Line 2 #"%runElab derive \#{add_quotes (primRecRef table)} [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]"#
showPrimRecDef (Sch name models) = Def (map showPrimRecDef models) --S
   
public export
showPrimDef : Schema -> SDoc
showPrimDef (Pk name db_field table) = Line 4 #"\#{id2pk db_field}:\#{columnIdrisType f}"# where
   f : Field
   f = (getPK_Field db_field table)      
showPrimDef (Prim prim) = Line 4 #"\#{id2pk (name prim)}:\#{columnIdrisType prim}"# 
showPrimDef (M2O rel db_field table) = Line 4 #"\#{id2pk (name db_field)}:\#{columnIdrisType f}"# where
   f : Field
   f = db_field 
showPrimDef (O2M rec_field rel_f tn) = Line 4 "--O2M"
showPrimDef (M2M rec_field f1 f2 m2m_table tn) = Line 4 "--M2M"
showPrimDef mod@(Model table fields) = Def [ns,Sep,primTab,Sep,
                                            np2Rec,
                                            read_rec_c,Sep,read_rec,main_read] where
   fs : List String
   fs = getFieldRefs (getPrimFields mod)
   cols : String
   cols = fastConcat $ intersperse ", " fs
   ns : SDoc 
   ns = Def [Line 0 #"namespace \#{primModelRef table}"#,
             Line 2 "domain : Op",
             Line 2 "domain = (True)",
             Line 2 "export",
             Line 2 "PrimCols : List Column",
             Line 2 #"PrimCols = [\#{cols}]"# ]
   primTab : SDoc
   primTab = Def [Line 2 "public export",
                  Line 2 #"\#{tableRef table} : Table"#,
                  Line 2 #"\#{tableRef table} = MkTable \#{add_quotes (dbtable table)} \#{primColsRef table}"#]

   rec : SDoc
   rec = Def ([Line 2 "record RecordModel where",Line 4 "constructor MkRecordModel"]++(map showPrimDef fields))
   elabRec : SDoc
   elabRec = Line 2 #"%runElab derive \#{add_quotes (primRecRef table)} [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]"#      
   np2Rec :SDoc
   np2Rec = Def [Sep,
                 Line 2 "export",
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
                   Line 4     "finish c",
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
showPrimDef (Sch name models) = Def (map showPrimDef models) --Sep

export
showRelDefRec : Schema -> SDoc
showRelDefRec (Pk name db_field table) = Line 4 #"\#{id2pk db_field}:\#{columnIdrisType f}"# where
   f : Field
   f = (getPK_Field db_field table)    
showRelDefRec (Prim prim) = Line 4 #"\#{id2pk (name prim)}:\#{columnIdrisType prim}"# 
showRelDefRec (M2O rel db_field table) = Line 4 #"\#{id2pk (name db_field)}:List \#{primRecRef rel}"# where
   f : Field
   f = db_field 
showRelDefRec (O2M rec_field rel_f tn) = Line 4 #"\#{id2pk rec_field}:List \#{o2mRecRef tn}"#
showRelDefRec (M2M rec_field f1 f2 m2m_table tn) = Line 4 #"\#{id2pk rec_field}:List \#{primRecRef tn}"#
showRelDefRec mod@(Model table fields) = Def [Sep,ns,rec,elabRec] where
   o2m_fields : List Schema
   o2m_fields = (filter isO2M fields)
   m2m_fields : List Schema
   m2m_fields = (filter isM2M fields)
   m2o_fields : List Schema
   m2o_fields = (filter isM2O fields)
   isM2M_tab : Bool
   isM2M_tab = (isM2M table) 
   
   pkRef : String
   pkRef = (columnRef (getPK_Field "pk" table) )
   ns : SDoc
   ns = Def [Line 0 #"namespace \#{o2mModelRef table}"#]
   rec : SDoc
   rec = Def ([Line 2 "public export",Line 2 "record RecordModel where",Line 4 "constructor MkRecordModel"]++(map showRelDefRec fields))
   elabRec : SDoc
   elabRec = Line 2 #"%runElab derive \#{add_quotes (o2mRecRef table)} [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]"#
showRelDefRec (Sch name models) = Def (map showRelDefRec models) --Sep

export
showRelDef : Schema -> SDoc
showRelDef (Pk name db_field table) = Line 4 #"\#{id2pk db_field}:\#{columnIdrisType f}"# where
   f : Field
   f = (getPK_Field db_field table)    
showRelDef (Prim prim) = Line 4 #"\#{id2pk (name prim)}:\#{columnIdrisType prim}"# 
showRelDef (M2O rel db_field table) = Line 4 #"\#{id2pk (name db_field)}:List \#{primRecRef rel}"# where
   f : Field
   f = db_field 
showRelDef (O2M rec_field rel_f tn) = Line 4 #"\#{id2pk rec_field}:List \#{o2mRecRef tn}"#
showRelDef (M2M rec_field f1 f2 m2m_table tn) = Line 4 #"\#{id2pk rec_field}:List \#{primRecRef tn}"#
showRelDef mod@(Model table fields) = Def [Sep,ns,read_rec_c,add_muf,ret_x,read_rec,main_read,
                                                             ret_ids] where
   o2m_fields : List Schema
   o2m_fields = (filter isO2M fields)
   m2m_fields : List Schema
   m2m_fields = (filter isM2M fields)
   m2o_fields : List Schema
   m2o_fields = (filter isM2O fields)
   isM2M_tab : Bool
   isM2M_tab = (isM2M table) 
   
   pkRef : String
   pkRef = (columnRef (getPK_Field "pk" table) )
   ns : SDoc
   ns = Def [Line 0 #"namespace \#{o2mModelRef table}"#,
              Line 2 "domain : Op",
              Line 2 "domain = (True)",
              Line 2  "isM2M_tab : Bool",
              Line 2 #"isM2M_tab = \#{show isM2M_tab}"#]
   rec : SDoc
   rec = Def ([Line 2 "record RecordModel where",Line 4 "constructor MkRecordModel"]++(map showRelDef fields))
   elabRec : SDoc
   elabRec = Line 2 #"%runElab derive \#{add_quotes (o2mRecRef table)} [Generic, Meta, Show, Eq, Ord,RecordToJSON,RecordFromJSON]"#      

      
   rel_relRef : List Schema -> List (String,TableName,Field)
   rel_relRef [] = []
   rel_relRef ((Pk name db_field x) :: xs) = []
   rel_relRef ((Prim prim) :: xs) = []
   rel_relRef ((M2O rel db_field x) :: xs) = [(name db_field,rel, db_field)]++(rel_relRef xs)
   rel_relRef ((O2M rec_field rel_f tn) :: xs) = [(rec_field,tn, rel_f)]++(rel_relRef xs)
   rel_relRef ((M2M rec_field f1 f2 m2m_table tn) :: xs) = []
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
   
   read_o2m : (rec_field:String) -> (col:Field) -> TableName -> SDoc
   read_o2m rec_field col rel = Line 5 #"\#{rec_field} <- \#{o2mModelRef rel}.read_records_c c ((\#{columnRef col}==\#{showJust $ isNull col}(cast pk)))"# 
   read_o2m_SDoc : SDoc
   read_o2m_SDoc  = Def ([ (read_o2m db_f col rel) | (db_f,rel,col) <- rel_relRef o2m_fields ] )


   rel_relRefM2M : List Schema -> List (String,TableName,TableName,Field,Field)
   rel_relRefM2M [] = []
   rel_relRefM2M ((Pk name db_field x) :: xs) = []
   rel_relRefM2M ((Prim prim) :: xs) = []
   rel_relRefM2M ((M2O rel db_field x) :: xs) = []++(rel_relRefM2M xs)   
   rel_relRefM2M ((O2M rec_field rel_f tn) :: xs) = []++(rel_relRefM2M xs)
   rel_relRefM2M ((M2M rec_field f1 f2 m2m_table rel) :: xs) = [ (rec_field,rel,m2m_table,f1,f2)]++(rel_relRefM2M xs)
   rel_relRefM2M ((Model x ys) :: xs) = []
   rel_relRefM2M ((Sch name models) :: xs) = []
      
   read_m2m : (rec_field:String) -> TableName -> TableName -> Field -> Field -> SDoc
   read_m2m rec_field rel m2mt f1 f2= ret where
     pkRel : String
     pkRel = columnRef (getPK_Field "pk" rel)
     ret : SDoc
     ret =Def [Line 5 #"let muf_m2m = ((JC \#{pkRel} \#{columnRef f2})&&(\#{columnRef f1}==(cast pk)))"#,
               Line 5 #"\#{rec_field}_np<-getJoin c \#{tableRef rel} \#{tableRef m2mt} (columns \#{tableRef rel}) muf_m2m"#,
               Line 5 #"let \#{rec_field}=[\#{primModelRef rel}.toRecord ox |ox <-\#{rec_field}_np]"#]

   read_m2m_SDoc : SDoc
   read_m2m_SDoc  = Def ([ (read_m2m db_f rel m2mt f1 f2) | (db_f,rel,m2mt,f1,f2) <- rel_relRefM2M m2m_fields ] )
   
   read_m2o : (rec_field:String) -> (col:Field) -> TableName -> SDoc
   read_m2o rec_field col rel = ret where
      pkM2o : Field
      pkM2o = (getPK_Field "pk" rel)
      ret : SDoc
      ret = Def [Line 5 #"let muf_m2o = ((\#{columnRef pkM2o}==\#{showJust $ isNull col}(cast \#{rec_field}))) --&&op"#,
                 Line 5 #"\#{rec_field} <- \#{primModelRef rel}.read_records_c c muf_m2o"#]
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
                   Line 4     "finish c",                   
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
                    
   
   read_rec_c_ids : SDoc
   read_rec_c_ids = Def [Line 2  "export",
                         Line 2 #"read_records_c_ids : HasIO io => MonadError SQLError io => Connection -> List Bits32 -> (op:Op)->io (List \#{o2mRecRef table} )"#,
                         Line 2  "read_records_c_ids c [] op  = pure []",
                         Line 2  "read_records_c_ids c (x::xs) op = do",
                         Line 4      #"r <- read_records_c c (( \#{pkRef}==(cast x))&&op) "#,
                         Line 4      "r_xs <- read_records_c_ids c xs op",
                         Line 4      "pure (r++r_xs)" ]      
                    
   read_rec_ids : SDoc
   read_rec_ids = Def [Line 2  "export",
                   Line 2 #"read_records_ids : HasIO io => MonadError SQLError io => List Bits32 -> (op:Op)->io (List \#{o2mRecRef table} )"#,
                   Line 2  "read_records_ids xs op = do",
                   Line 4     "c <- connect DB_URI",
                   Line 4     #"ret <- \#{o2mModelRef table}.read_records_c_ids c xs op"#,
                   Line 4     "finish c",                   
                   Line 4     "pure ret"]
   main_read_ids : SDoc
   main_read_ids = Def [Sep,Line 2 "export",
                    Line 2 #"main_runET_ids : List Bits32 -> (op:Op) -> IO (List \#{o2mRecRef table} )"#,
                    Line 2 #"main_runET_ids xs op = do "#,
                    Line 4 #"Left err <- runEitherT (\#{o2mModelRef table}.read_records_ids xs op {io = EitherT SQLError IO} )"#,
                    Line 5     "| Right l1 => pure l1",
                    Line 4 "printLn err",
                    Line 4 "pure []",
                    Sep,
                    Line 2 "export",
                    Line 2 #"read_ids : HasIO io => List Bits32 -> (op:Op) -> io (List \#{o2mRecRef table} )"#,
                    Line 2  "read_ids xs op = do",
                    Line 4 #"l1 <- (liftIO $ (\#{o2mModelRef table}.main_runET_ids xs op))"#,
                    Line 4 "pure l1"]

   ret_ids : SDoc
   ret_ids = if (not isM2M_tab) then Def [read_rec_c_ids,read_rec_ids,main_read_ids] else Def []                
showRelDef (Sch name models) = Def (map showRelDef models)


||| Genereate Column definitions

export   
toColumnSDoc : Field -> SDoc         
toColumnSDoc f@(MkF isNull primType name pg_type castTo castFrom t@(MkTN ref dbtable m ism2m)) = Def [(Line 0 #"\#{columnRef f}:Column"#),
                                                                                              (Line 0 #"\#{columnRef f}=\#{df}"#)] where
      df:String
      df=(show isNull)++" "++(show primType)++" "++(add_quotes name)++" ("++(show pg_type)++") "++castTo++" "++castFrom++" "++(ref)

export
showColumnDef : Schema -> SDoc
showColumnDef (Pk name db_field table) = toColumnSDoc $ getPK_Field db_field table
showColumnDef (Prim prim) = toColumnSDoc $ prim
showColumnDef (M2O rel db_field table) = toColumnSDoc db_field 
showColumnDef (O2M rec_field rel_f tn) = Line 0 "--O2M"
showColumnDef (M2M rec_field f1 f2 m2m_table tn) = Line 0 "--M2M"
showColumnDef (Model tn []) = Def [] 
showColumnDef (Model tn xs) = Def ([Sep]++(map showColumnDef xs))
showColumnDef (Sch n []) = Def []
showColumnDef (Sch n xs) = Def [modules] where       
    modules:SDoc
    modules = Def (map showColumnDef xs)

export
showSchemaRecDef : Schema -> SDoc
showSchemaRecDef s = Def [showImports, showPrimRecDef s, showRelDefRec s] where
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
    
    get_name : Schema -> String
    get_name (Sch n xs) = n
    get_name _ = ""
    s_n : String
    s_n = get_name s
    
    showImports:SDoc
    showImports=Def [Line 0 #"module \#{s_n}RecDef"#,
                     Line 0 "import Category.Transaction.Types",
                     Line 0 "import Data.Ratio",Sep,
                     Line 0 "import Generics.Derive",Sep,
                     Line 0 "import JSON",Sep,
                     Line 0 "import Ledger.PG.Config",
                     Line 0 "import Control.Monad.Either",Sep,
                     Line 0 "%language ElabReflection"]

export
showSchemaDef : Schema -> SDoc
showSchemaDef s = Def [showImports, showTableDef, showColumnDef s, showPrimDef s, showRelDef s] where --showTableDef,
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

    showTableDef:SDoc
    showTableDef = Def (map tn_show (schema_tables s)) where 
       tn_show : TableName -> SDoc
       tn_show (MkTN ref dbtable m ism2m) = Def [(Line 0 #"\#{ref}:String"#),
                                                 (Line 0 #"\#{ref} = \#{add_quotes dbtable}"#)]   

    get_name : Schema -> String
    get_name (Sch n xs) = n
    get_name _ = ""
    s_n : String
    s_n = get_name s
        
    showImports:SDoc
    showImports=Def [Line 0 #"module \#{s_n}"#, Sep,
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
           Line 0 "import Odoo.Schema.PJBRecDef",Sep,
           Line 0 "%language ElabReflection"]
