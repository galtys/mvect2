module Examples.Reset2

import Text.CSS
import Examples.CSS.Reset
import Examples.Util
import Rhone.JS
import Data.Ratio
import Category.Transaction.Types
import Category.Transaction.Hom

--%default total
public export
Ev : Type
Ev = Int8 -> Int8

{-
export
lbl : (text: String) -> (class : String) -> Node ev
lbl txt cl = label [classes [widgetLabel, cl]] [Text txt]
-}

btn :  (r : ElemRef Button)
    -> {auto 0 _ : ById r}
    -> Ev
    -> (lbl: String)
    -> Node Ev
btn r ev lbl =
  button [ref r, onClick ev, classes [widget,btn]] [Text lbl]

content : Node Ev
content =
  div [ class resetContent ]
      [ lbl "Reset counter:"    resetLbl, btn btnReset (const 0) "Reset"
      , lbl "Increase counter:" incLbl,   btn btnInc   (+ 1)     "+"
      , lbl "Decrease counter:" decLbl,   btn btnDec   (+ (-1))  "-"
      , lbl "Count:"            countLbl
      , div [ref out] []
      ]


public export
M : Type -> Type
M = DomIO Ev JSIO

msf : MSF M Ev ()
msf =
  accumulateWith apply 0 >>> fan_
    [ show     ^>> text out
    , (<= -10) ^>> disabledAt btnDec
    , (>=  10) ^>> disabledAt btnInc
    , (==   0) ^>> disabledAt btnReset
    ]
export
ui : M (MSF M Ev (), JSIO ())
ui = innerHtmlAt exampleDiv content $> (msf, pure ())

{-
toDx : String -> String -> Node Ev
toDx k dx =tr [] [td [] [fromString k], td [] [fromString dx], td [] [] ]
toCx : String -> String -> Node Ev
toCx k cx =tr [] [td [] [fromString k], td [] [], td [] [fromString cx] ] 

f2 : ProdKey -> String -> Node Ev
f2 ((PKCy DX z v)) y= toDx (show z) y
f2 ((PKCy CX z v)) y= toCx (show z) y
f2 ((PKUser DX z v)) y= toDx (show z) y
f2 ((PKUser CX z v)) y= toCx (show z) y
f2 ((PK32 DX z v)) y= toDx (show z) y
f2 ((PK32 CX z v)) y= toCx (show z) y
f2 ((PKPrice DX z w v)) y= toDx (show z++" "++show w) y
f2 ((PKPrice CX z w v)) y= toCx (show z++" "++show w) y
f2 ((FromInteger DX v)) y= toDx "Int" y
f2 ((FromInteger CX v)) y= toCx "Int" y
-}

f : Product  -> Node Ev
f (k,v) = tr [] [ td [] [fromString $show k],
                  td [] [fromString $show v] ] --f2 k (show v)

--fProduct2 : Product2 -> Node Ev
--fProduct2 (x, y) = f2 x (show y)

--muf : Hom1 -> Node Ev
--muf xs = tbody [] (map f xs) 

show_Hom1 : Hom1 -> Node Ev
show_Hom1 dx1 =
  table [ class "hover" ]
        [ thead []
                [tr [] 
                   [ th [Str "width" "200"] ["ProdKey"]
                   , th [Str "width" "50"]  ["Qty"] 
                    ]]
        ,tbody [] (map f dx1)                       
      ]
fql : QLine -> Node Ev
fql (MkQL dxpk bom q cxpk price) = tr [] [td [] [fromString $show dxpk],
                                      td [] [fromString $show q],
                                      td [] [fromString $show cxpk],
                                      td [] [fromString $show price]]

show_HomQLine : HomQLine -> Node Ev
show_HomQLine xs = div [] [

  h5 [] ["Lines"]
  ,table [ class "hover" ]
        [  thead []
                [tr [] 
                   [ th [Str "width" "200"] ["DX Key"]
                   , th [Str "width" "50"]  ["Qty"] 
                   , th [Str "width" "200"] ["CX Key"]
                   , th [Str "width" "50"] ["Price"] ]
                ]
         , tbody [] (map fql xs)        
        ]
  ,h5 [] ["colimLines"]        
  ,  table [ class "hover" ]
        [  thead []
                [tr [] 
                   [ th [Str "width" "200"] ["DX Key"]
                   , th [Str "width" "50"]  ["Qty"] 
                   , th [Str "width" "200"] ["CX Key"]
                   , th [Str "width" "50"] ["Price"] ]
                ]
         , tbody [] (map fql (colimQLine xs) )        
        ]
   ]
      
dx1 : Hom1 
dx1 = [ (pk32DX 1, 1), (pk32DX 3, 1), (pk32DX 4, 2)]



show_hom : Node Ev
show_hom = div [class "row"] [
  div [class "large-6 columns"] [
    h4 [] ["Target"] 
    ,(show_HomQLine demoQL )
  ]
  ,div [class "large-6 columns"] [
    h4 [] ["Transit"] 
    ,(show_Hom1 dx1 )
  ]
  ,hr [] []
  
  ]
{-
export
ui_t : M (MSF M Ev (), JSIO ())
ui_t = innerHtmlAt exampleDiv (show_Hom1 dx1) $> (msf, pure ())
-}

export
ui_t : M (MSF M Ev (), JSIO ())
--ui_t = innerHtmlAt exampleDiv (show_HomQLine (demoQL++(colimQLine demoQL))  ) $> (msf, pure ())
ui_t = innerHtmlAt exampleDiv show_hom  $> (msf, pure ())

-- Local Variables:
-- idris2-load-packages: ("contrib" "base" "rhone-js" "base" "contrib" "sop" "elab-util" "dom" "json" "rhone" "tailrec")
-- End:
