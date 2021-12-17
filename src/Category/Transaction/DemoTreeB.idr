module Category.Transaction.DemoTreeB

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
test_treeB : IO ()
  printLn $show1 tree
  printLn $show2' tree
  printLn $show3' tree
  printLn $show4' tree
