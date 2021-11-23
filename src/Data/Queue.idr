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
{-  
  data Muf : Type where
     C1 : Muf  
     C2 : Int -> Muf -> Muf
  %runElab derive "Muf" [Generic, Meta, Eq, Ord, Show]
  -}
  export  
  L1 : (k:Type) -> Type
  L1 k = SOP I [[],[k,(L1 k)] ]
  
  
  export
  S1 : (k:Type) -> Type
  S1 k = SOP I [[],[(S1 k),k] ]
  
  export
  Q1 : (k:Type) -> Type
  Q1 k = NP I [(L1 k),(S1 k)]


  Mus : L1 Int
  Mus = MkSOP (Z [])
  
  Mu1 : (L1 Int)
  Mu1 = MkSOP (S $ Z [4,Mus])
  
  Mu2 : (L1 Int)
  Mu2 = MkSOP (S $ Z [1,Mu1])
  
  --N1 : (k:Type) -> Type
  --N1 k = Z k --NS I [k]
    
  --L1Int : L1 Int
  --L1Int = MkSOP ()
  --Q1 : (k:Type) -> Type
  --Q1 k = SOP I [[],[k,(Q1 k)] ]
    
            
  export
  head : Queue a -> Maybe a   
  head (MkQ [] r) = Nothing
  head (MkQ (x :: xs) r) = Just x
  
  checkf : Queue a -> Queue a
  checkf (MkQ [] r) = MkQ (toList r) [<]
  checkf q = q 
  export
  snoc : {a:Type} -> Queue a -> a -> Queue a
  snoc (MkQ f r) x = checkf (MkQ f (r:<x))
  export
  tail : Queue a -> Queue a
  tail (MkQ [] r) = checkf (MkQ [] r)
  tail (MkQ (x :: xs) r) = checkf (MkQ xs r)  
  export
  qtoList : Queue a -> List a
  qtoList x = case (head x) of
     Nothing => []
     Just y => [y] ++ (qtoList $ tail x)
  export
  toQueue : {a:Type} -> List a -> Queue a
  toQueue [] = MkQ [] [<]
  toQueue (x :: xs) = snoc (toQueue xs) x 

q1 : Queue Int
q1 = MkQ [] [<]

q2 : Queue Int
q2 = snoc q1 10

q3 : Queue Int
q3 = snoc q2 20

q4 : Queue Int
q4 = snoc q3 120

  --snoc (MkQ f (sx :< x)) a = ?sdfsdf_2 --(MkQ f (sx :< x))
  
  --snoc (MkQ f [<]) a = ?sdfsdf_1 --(MkQ f [<])
  --snoc (MkQ f (sx :< x)) a = ?sdfsdf_2 --(MkQ f (sx :< x))



