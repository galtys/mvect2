module Category.Transaction.Demo2

import Data.Zippable
import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import JSON

import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.Types2
import Category.Transaction.Hom
import Category.Transaction.Journal
import Crypto.Hash.SHA256
import Data.Ratio

--import Category.PG.Order

%language ElabReflection

public export
tree:TreeB
tree= Node (Node (Leaf "1 ") "2 " (Leaf "3 "))
           "4 "
           (Leaf "5 ")
public export
show1:TreeB-> String
show1 (Leaf s)=s
show1 (Node l s r) = 
    show1 l ++ s ++ show1 r

-- cps
public export
show2 : TreeB -> (String->a) -> a
show2 (Leaf s) k = k s
show2 (Node lft s rgt) k= 
  show2 lft (\ls => 
     show2 rgt (\rs => 
       k (ls++s++rs)))

mutual
  done : String -> String
  done x = x

  next : (String,TreeB,(String->String))-> String -> String
  next (s,rgt,k) ls = show3 rgt (cont (ls,s,k))

  cont: (String,String,(String->String))-> String -> String
  cont (ls,s,k) rs = k (ls++s++rs)

  show3 : TreeB -> (String -> String) -> String
  show3 (Leaf s) k = k s
  show3 (Node lft s rgt) k = 
     show3 lft (next (s,rgt,k))

data Kont = Done
          | Next String TreeB Kont
          | Conc String String Kont
          
mutual
  --()
  --(String,Tree,String->String)
  --(String,String,String->String)
  apply : Kont -> String -> String
  apply Done s = s
  apply (Next s rgt k) ls = show4 rgt (Conc ls s k)
  apply (Conc ls s k)  rs = apply k (ls++s++rs)
  
  public export
  show4 : TreeB -> Kont -> String
  show4 (Leaf s) k         = apply k s
  show4 (Node lft s rgt) k = show4 lft (Next s rgt k)

  public export
  show4' : TreeB -> String
  show4' t = show4 t Done
  
data Stack = List (String,Either TreeB String)
{-
mutual
  apply2 : Stack -> String -> String
  apply2 [] s = s
  apply2 ((x,(Left rgt)::xs) s = show5 rgt ( (Right ):xs)
-}
public export
show3' : TreeB -> String
show3' t = show3 t (\x=>x) 
                            
public export
show2' : TreeB -> String
show2' t = show2 t (\x => x)

export
hilton : Address
hilton = MkA "Street" "" "London" "SU 4X" UK (MkC "Hilton")

export
p1:Product
p1 = ("p1",2)
export
p2:Product
p2 = ("p2",7)
export
p3:Product
p3 = ("p1",10)


export
confirm_so : OrderEvent ()
confirm_so = do
 let date = "2021-11-01"
     up1 = (toEX20 31.73)
     up2 = (toEX20 15.03)
     up3 = (toEX20 25.00)
     sub1 = (snd p1) * (cast up1)
     sub2 = (snd p2) * (cast up2)
     sub3 = (snd p3) * (cast up3)
               
     line1 = MkFx date hilton (MkH p1 up1 ("GBP",sub1) )     
     line2 = MkFx date hilton (MkH p2 up2 ("GBP",sub2)  )
     line3 = MkFx date hilton (MkH p3 up3 ("GBP",sub3)  )  
     
 Confirm (MkO Sale line1)
 Confirm (MkO Sale line2)
 Confirm (MkO Sale line3) 
 Pure ()

export
test_demo2 : IO ()
test_demo2 = do
  
  pure ()
  printLn $show1 tree
  printLn $show2' tree
  printLn $show3' tree
  printLn $show4' tree
