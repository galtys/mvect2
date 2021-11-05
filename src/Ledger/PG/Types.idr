module Ledger.PG.Types

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types
import Generics.Derive
import Data.SortedMap
import Control.Monad.Either
import JSON

%language ElabReflection
{-
namespace TF
   mutual --add Enum Type as selection, add Interface Is Enum type
     --for m2o,add a proof that column is in model table
     public export
     data Field = Prim Column | M2O Model Column | O2M Model --| M2M Model 

     public export  
     record Model where
       constructor MkM
       table : Table
       pk : Column
       fields : (List Field)
-}
namespace OE
   public export
   data PrimTypes = I_Bits32|I_Price|I_Date|I_String
   %runElab derive "PrimTypes" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON] 
   data PgTypes = PG_BigInt | PG_Text | PG_VCH Int | PG_Double
   %runElab derive "PgTypes" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]

   data ToPG = Nto String -- will be Name of the function
   %runElab derive "ToPG" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]
   
   data FromPG = Nfrom String 
   %runElab derive "FromPG" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]   

   public export
   TableName : Type
   TableName = String
         
   public export
   record Field where
     constructor MkF
     isNull : Bool
     primType : PrimTypes
     name : String
     pg_type : PgTypes
     castTo : ToPG
     castFrom : FromPG
     table : TableName
   %runElab derive "Field" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]  
              
   public export
   data Schema : Type where
     Pk : (pk:OE.Field) -> Schema
     M2O : (model: Schema) -> (col : OE.Field) -> Schema
     O2M : (model: Schema) -> Schema
     M2M : (model: Schema) -> Schema
     Model : (table:TableName)->(pk:Schema)->(fields:List Schema) -> Schema
     Sch : (models: List Schema) -> Schema
     
   %runElab derive "Schema" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]        

   validateSchema : Schema -> Bool
   validateSchema (Pk pk) = True
   validateSchema (M2O model col) = ?validateSchema_rhs_2
   validateSchema (O2M model) = ?validateSchema_rhs_3
   validateSchema (M2M model) = ?validateSchema_rhs_4
   validateSchema (Model table pk fields) = ?validateSchema_rhs_5
   validateSchema (Sch models) = ?validateSchema_rhs_6
   
{-
export
getM2OColsL : List TF.Field -> List (Model,Column)
getM2OColsL [] = []
getM2OColsL ((Prim x) :: xs) = getM2OColsL xs
getM2OColsL ((M2O x y) :: xs) = [(( x),y)]++getM2OColsL xs
getM2OColsL ((O2M x) :: xs) = getM2OColsL xs

--  data Model = MkM 
export
getM2OCols : Model -> List (Model,Column)
getM2OCols (MkM table pk fields) = getM2OColsL fields

export
toFields : List Column -> List TF.Field
toFields [] = []
toFields (x :: xs) = [Prim x] ++ (toFields  xs)

export
getPrimCols : List TF.Field -> List Column
getPrimCols [] = []
getPrimCols ((Prim x) :: xs) = [x] ++ (getPrimCols xs)
getPrimCols (x :: xs) = (getPrimCols xs)
-}

{-
copyCol : Column -> Column
copyCol (MkField idrisTpe name pqType constraint deflt fromPQ toPQ table_name) = (MkField idrisTpe name pqType constraint deflt fromPQ toPQ table_name)

export
dedup : List String -> List String
dedup xs  = ret where
   tosm : List String -> SortedMap String ()
   tosm l = fromList [ (x,()) | x<-l]
   tol : SortedMap String () -> List (String, ())
   tol = toList   
   ret : List String
   ret = [a | (a,b) <- (tol (tosm xs)) ]



export --possible runtime error
getModelTableS : Model -> Maybe Table
getModelTableS (MkM fields) = ret  where
  rte : String
  rte = ?possible_runtime_error
  cols : List Column
  cols = getPrimCols fields
  tns : List String --list of table names
  tns = dedup (map table_name cols)
  safeHead : List String -> String
  safeHead [] = ""
  safeHead (x::xs) = x
  
  ret : Maybe Table  
  ret = if ((length tns)==1) then Just (MkTable (safeHead tns) cols) else Nothing
export
getModelTable : Model -> Table
getModelTable m = case (getModelTableS m) of
                    Nothing => (MkTable "" [])
                    Just t => t


export
getSQL2 :  (t        : Table)
       -> (cs       : List Column)
--       -> {auto 0 _ : Elems cs (columns t)}
       -> (query    : Op)
       -> String
getSQL2 t cs query =
  let cols = fastConcat $ intersperse ", " $ map name cs
   in #"SELECT \#{cols} FROM \#{t.name} WHERE \#{opToSQL query};"#

names2 : (cs : List Column) -> NP (K String) (GetTypes cs)
names2 []        = []
names2 (x :: xs) = x.name :: names2 xs

reader2 : (c : Column) -> String -> Maybe (GetTypeC c)
reader2 (MkField _ _ pqType _ _ fromPQ _ _) s =
  fromPQ (decodeDBType pqType s)

readers2 : (cs : List Column) -> NP (\t => String -> Maybe t) (GetTypes cs)
readers2 []        = []
readers2 (x :: xs) = reader2 x :: readers2 xs

export
get2 :  HasIO io
    => MonadError SQLError io
    => Connection
    -> (t        : Table)
    -> (cs       : List Column)
    ---> {auto 0 _ : Elems cs (columns t)}
    -> (query : Op)
    -> io (List $ GetRow cs)
get2 c t cs query = do
  res <- exec c (getSQL2 t cs query) TUPLES_OK
  getRows (names2 cs) (readers2 cs) res

-}
