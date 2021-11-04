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

mutual --add Enum Type as selection, add Interface Is Enum type
  public export
  data Field = Prim Column | M2O Model Column | O2M Model --| M2M Model 

  public export  
  record Model where
    constructor MkM
    table : Table
    fields : (List Field)
  
--  data Model = MkM 

export
toFields : List Column -> List Field
toFields [] = []
toFields (x :: xs) = [Prim x] ++ (toFields  xs)

export
getPrimCols : List Field -> List Column
getPrimCols [] = []
getPrimCols ((Prim x) :: xs) = [x] ++ (getPrimCols xs)
getPrimCols (x :: xs) = (getPrimCols xs)

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
