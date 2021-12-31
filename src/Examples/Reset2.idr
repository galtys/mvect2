module Examples.Reset2

import Text.CSS
import Examples.CSS.Reset
import Examples.Util
import Rhone.JS

import Data.Ratio
import Category.Transaction.Types

--%default total
public export
Ev : Type
Ev = Int8 -> Int8
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
      , lbl "Count:"            countLbl, div [ref out] []
      ]

dx1 : Hom1 
dx1 = [ (PK32 DX 1, 1), (PK32 DX 3, 1), (PK32 DX 4, 2)]

toDx : String -> String -> Node Ev
toDx k dx =tr [] [td [] [fromString k], td [] [fromString dx], td [] [] ] 

toCx : String -> String -> Node Ev
toCx k cx =tr [] [td [] [fromString k], td [] [], td [] [fromString cx] ] 


f : Product -> Node Ev
f ((PKCy DX z), y) = toDx (show z) (show y)
f ((PKCy CX z), y) = toCx (show z) (show y)
f ((PKUser DX z), y) = toDx (show z) (show y)
f ((PKUser CX z), y) = toCx (show z) (show y)
f ((PK32 DX z), y) = toDx (show z) (show y)
f ((PK32 CX z), y) = toCx (show z) (show y)
f ((PKPrice DX z w), y) = toDx (show z++" "++show w) (show y)
f ((PKPrice CX z w), y) = toCx (show z++" "++show w) (show y)
f ((FromInteger DX), y) = toDx "Int" (show y)
f ((FromInteger CX), y) = toCx "Int" (show y)

muf : Hom1 -> Node Ev
muf xs = tbody [] (map f xs) 

content_tab : Node Ev
content_tab =
  table [ class "hover" ]
        [ thead []
                [tr [] 
                   [ th [Str "width" "200"] ["ProdKey"]
                   , th [Str "width" "50"] ["DX"] 
                   , th [Str "width" "50"] ["CX"] ]]
        , (muf dx1)
                       
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

export
ui_t : M (MSF M Ev (), JSIO ())
ui_t = innerHtmlAt exampleDiv content_tab $> (msf, pure ())


-- Local Variables:
-- idris2-load-packages: ("contrib" "base" "rhone-js" "base" "contrib" "sop" "elab-util" "dom" "json" "rhone" "tailrec")
-- End:
