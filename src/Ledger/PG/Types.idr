module Ledger.PG.Types

import PQ.CRUD
import PQ.FFI
import PQ.Schema
import PQ.Types
import Generics.Derive

import JSON

%language ElabReflection

mutual
  public export
  data Field = Prim Column | M2O Model Column | O2M Model --| M2M Model
  public export
  data Model = MkM (List Field)

export
toFields : List Column -> List Field
toFields [] = []
toFields (x :: xs) = [Prim x] ++ (toFields  xs)

