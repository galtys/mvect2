module Data.Queue

import Data.SnocList
import Generics.Derive
import JSON

%language ElabReflection
--%language ElabReflection

namespace Queue
  public export
  data Queue : (a:Type) -> Type where
     MkQ : (f:List a) -> (r:SnocList a) -> Queue a  
  %runElab derive "Queue" [Generic, Meta, Eq, Ord, Show]
            
  export
  ||| Get an item if the queue s not empty
  head : Queue a -> Maybe a   
  head (MkQ [] r) = Nothing
  head (MkQ (x :: xs) r) = Just x 
  
  checkf : Queue a -> Queue a
  checkf (MkQ [] r) = MkQ (toList r) [<]
  checkf q = q 
  
  ||| Add an item to the end of the queue
  export
  snoc : {a:Type} -> Queue a -> a -> Queue a
  snoc (MkQ f r) x = checkf (MkQ f (r:<x))
  
  export
  tail : Queue a -> Queue a
  tail (MkQ [] r) = checkf (MkQ [] r)
  tail (MkQ (x :: xs) r) = checkf (MkQ xs r)  
  

export
q1 : Queue Int
q1 = MkQ [] [<]
export
q2 : Queue Int
q2 = snoc q1 10
export
q3 : Queue Int
q3 = snoc q2 20
export
q4 : Queue Int
q4 = snoc q3 120
{-
  export
  qtoList : Queue a -> List a
  qtoList x = case (head x) of
     Nothing => []
     Just y => [y] ++ (qtoList $ tail x)
  export
  toQueue : {a:Type} -> List a -> Queue a
  toQueue [] = MkQ [] [<]
  toQueue (x :: xs) = snoc (toQueue xs) x 
-}
