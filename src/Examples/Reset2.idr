module Examples.Reset2

import Text.CSS
import Examples.CSS.Reset
import Examples.Util
import Rhone.JS
import Data.MSF.Trans


{-
import Data.Ratio
import Category.Transaction.Types
import Category.Transaction.Types2
import Category.Transaction.RouteTypes
import Category.Transaction.Hom
import JSON
import Odoo.Schema.PJBRecDef
import UserDataDemo
import Category.Transaction.Demo2
import Category.Transaction.Owner
import Category.Transaction.Warehouse
import Category.Transaction.Route
import Data.SortedMap
import Control.Monad.State
import Data.List
-}
import Data.Zippable
--import Generics.Derive
import Data.SortedMap
import Control.Monad.State
--import Libc.Time
import JSON
import Category.Transaction.Qty
import Category.Transaction.Types
import Category.Transaction.RouteTypes
import Category.Transaction.Route
import Category.Transaction.Types2
import Category.Transaction.Hom
import Category.Transaction.Journal
import Category.Transaction.Owner
import Crypto.Hash.SHA256
import Data.Ratio
import Odoo.Schema.PJBRecDef
--import Odoo.Schema.PJB
import UserDataDemo
import Odoo.PG.BoM
import Config
import Data.HashDB.DataIO
import Data.HashDB.Types
import Control.Monad.Either

import Category.Transaction.Demo2
import Category.Transaction.Warehouse

--mport Data.Vect
--import Examples.CSS.MathGame
--import Examples.Util
--import Generics.Derive
--import Rhone.Canvas
--import Rhone.JS
--import System.Random
--import Text.CSS


namespace JSMem
   export 
   interpret_js : LiftJSIO m => WhsEvent ta -> StateT SystemState m ta       
   interpret_js = MemoryMap.interpret


--%default total
public export
Ev : Type
Ev = Int8 -> Int8

{-
export
lbl : (text: String) -> (class : String) -> Node ev
lbl txt cl = label [classes [widgetLabel, cl]] [Text txt]
-}
unMaybe : Maybe String -> String
unMaybe Nothing = ""
unMaybe (Just x) = x

show_ResPartner : String -> Maybe BrowseResPartner.RecordModel -> Node Ev
show_ResPartner x Nothing = div [class "callout"] [
        h4 [] [fromString x]
     ]
show_ResPartner x (Just (MkRecordModel pk name use_parent_address active street contract city zip country_id parent_id child_ids email street2)) = div [class "callout"] [
        h4 [] [fromString x]
        ,h4 [] [fromString name]
        ,p [] [fromString $unMaybe street]
        ,p [] [fromString $unMaybe street2]
        ,p [] [fromString $unMaybe zip]
     ]

show_Location : RouteTypes.Location -> Node Ev
show_Location Self = show_ResPartner "Self" Nothing
show_Location (In x) = show_ResPartner "In" (Just x)
show_Location (Out x) = show_ResPartner "Out" (Just x)
show_Location (Border x) = show_ResPartner "Border" (Just x)
show_Location Init = show_ResPartner "Init" Nothing
show_Location Loss = show_ResPartner "Loss" Nothing
show_Location (Control x y) = show_ResPartner "Control \{show x}" (Just y)
show_Location (Partner x y) = show_ResPartner "Partner \{show x}" (Just y)
show_Location (Transit x y) = show_ResPartner "Transit \{show x}" (Just y)
show_Location (Taxman x) = show_ResPartner "Taxman" (Just x)
show_Location (Bank x) = show_ResPartner "Bank" (Just x)


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

f : Product  -> Node Ev
f (k,v) = tr [] [ td [] [fromString $show k],
                  td [] [fromString $show v] ] --f2 k (show v)

show_Hom1 : Hom1 -> Node Ev
show_Hom1 dx1 =
  table [ class "unstriped hover" ]
        [ thead []
                [tr [] 
                   [ th [Str "width" "100"] ["ProdKey"]
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
  table [ class "unstriped hover" ]
        [  thead []
                [tr [] 
                   [ th [Str "width" "200"] ["DX Key"]
                   , th [Str "width" "50"]  ["Qty"] 
                   , th [Str "width" "50"] [""]
                   , th [Str "width" "50"] ["Price"] ]
                ]
         , tbody [] (map fql (colimQLine xs) )        
        ]
   ]
      
--dx1 : Hom1 
--dx1 = [ (pk32DX 1, 1), (pk32DX 3, 1), (pk32DX 4, 2)]



show_hom : Hom1 -> Node Ev
show_hom dx1 = div [class "grid-x grid-padding-x"] [
  
  div [class "medium-4 cell"] [
    h4 [] ["Deliver to:"]
  ]
    
  ,div [class "medium-5 cell"] [
    div [class "callout primary"] [
      h4 [] ["Target"] 
      ,(show_HomQLine demoQL )
    ]  
  ]  
  
  ,div [class "medium-3 cell"] [
    div [class "callout secondary"] [
      h4 [] ["Transit"] 
      ,(show_Hom1 dx1 )
    ]
  ]
  ,hr [] []
  
  ]
{-
export
ui_t : M (MSF M Ev (), JSIO ())
ui_t = innerHtmlAt exampleDiv (show_Hom1 dx1) $> (msf, pure ())

export
ui_t : M (MSF M Ev (), JSIO ())
--ui_t = innerHtmlAt exampleDiv (show_HomQLine (demoQL++(colimQLine demoQL))  ) $> (msf, pure ())
ui_t = innerHtmlAt exampleDiv show_hom  $> (msf, pure ())

-}


-- Local Variables:
-- idris2-load-packages: ("contrib" "base" "rhone-js" "base" "contrib" "sop" "elab-util" "dom" "json" "rhone" "tailrec")
-- End:

msf2 : MSF (StateT SystemState M) Ev ()
msf2 =  fan_ []
{-
             ifIs NewGame newGame
            , ifIs Check check
            , adjLang
            , get >>> dispGame
            ]
-}

export
ui2 : M (MSF M Ev (), JSIO ())
ui2 = do
  --innerHtmlAt exampleDiv (content EN)
  (reas22,h1) <- runStateT initState (JSMem.interpret_js (toWhs   demo_po_so)   ) 
    
  innerHtmlAt exampleDiv (show_hom h1)
  --ini <- randomGame EN
  

  
  pure (feedback initState (fromState msf2), pure ())
