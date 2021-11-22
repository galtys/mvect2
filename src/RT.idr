module RT

import Data.HashDB.Types
import Data.HashDB.DataIO
--import Crypto.Hash.SHA256
--import Data.Vect
--import Generics.Derive
--import JSON
--import Data.SortedMap
--%language ElabReflection

-- Using list str

{-
export
toHList : List String -> (HType, SortedMap TypePtr HType)
toHList [] = (nullStrListT,(insert k nullStrListT empty)) where
    k : TypePtr
    k = ptr nullStrListT
toHList (x::xs) =  (new, (insert k new prev_map)) where
     prev : (HType, SortedMap TypePtr HType)
     prev = toHList xs
     
     prev_ht : HType
     prev_ht = fst prev
     prev_map : SortedMap TypePtr HType
     prev_map = snd prev
     new : HType
     new = tCons x prev_ht StrListT
     k : TypePtr
     k = ptr new
-}



