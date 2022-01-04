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
        h6 [] [fromString x]
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
                                          
                                          td [] [fromString $show price],
                                          td [] [fromString $show (q*price)],
                                          td [] [fromString $show cxpk]]

show_HomQLine : HomQLine -> Node Ev
show_HomQLine xs = div [] [
  table [ class "unstriped hover" ]
        [  thead []
                [tr [] 
                   [ th [Str "width" "190"] ["DX Key"]
                   , th [Str "width" "50"]  ["Qty"] 
                
                   , th [Str "width" "40"]  ["Price"]
                   , th [Str "width" "40"]  ["Subtotal"]
                   , th [Str "width" "60"]  [""] ]
                ]
         , tbody [] (map fql (colimQLine xs) )        
        ]
   ]
{-      
dx1 : Hom1 
dx1 = [ (pk32DX 1, 1), (pk32DX 3, 1), (pk32DX 4, 2)]
-}
{-
show_fxevent : FxEvent -> Node Ev
show_fxevent (Fx121 x y) = div [class "callout"] [
                                p  [] [fromString x]
                                ,h4 [] ["Target"] 
                                ,(show_HomQLine $ toQLine $ toHom12 y)
                               ]  
show_fxevent (Fx11 x y) =  div [class "callout"] [
                                p  [] [fromString x]
                                ,h4 [] ["Target"] 
                                ,(show_HomQLine demoQL)
                               ]
-}
show_RouteKey : RouteKey -> String
show_RouteKey (MkRK date ref state) = "Date: \{date}, Ref: \{ref}, State: \{show state}"

show_Ref : Ref -> String --Node Ev
show_Ref (MkAllocationRef x) = " Allocation: \{x}"
show_Ref (MkRouteKeyRef (MkRK date ref state)) = " Route: \{ref}"


show_whsentry : (WhsEntry,RouteTypes.DocumentType) -> Node Ev
show_whsentry (MkWE ref (Fx121 date y), dt) = 
                         div [class "callout primary"] [
                                span  [] [fromString date ]
                                , span [class "doc-ref"] [fromString $ show_Ref ref]
                                ,h4 [] [fromString $ show dt] 
                                ,(show_HomQLine $ toQLine $ toHom12 y)
                                --, hr [] []
                         ]
show_whsentry (MkWE ref (Fx11 date y),dt) = 
                         div [class "callout"] [
                                span  [] [fromString date]
                                , span [class "doc-ref"] [fromString $ show_Ref ref]                                
                                ,h4 [] [fromString $ show dt] 
                                ,(show_Hom1 $ toHom1 y)
                                --, hr [] []                                
                         ]


data RouteLineGridItem = MkLoc RouteTypes.Location | MkWE (List (WhsEntry,RouteTypes.DocumentType))

route_grid_items : (List RouteLine) -> List RouteLineGridItem
route_grid_items [] = []
route_grid_items ((MkRL move f oh)::xs) = [MkLoc (from move),MkWE f,MkWE oh]++(route_grid_items xs)

--route_grid_items ((MkRL move f oh)::xs) = [MkLoc (from move),MkLoc (to move),MkWE f,MkWE oh]++(route_grid_items xs)


show_route_grid_item : RouteLineGridItem -> Node Ev
show_route_grid_item (MkLoc x) = (show_Location x)
show_route_grid_item (MkWE xs) = div [class "route-item"] (map show_whsentry xs)


show_hom : RouteData -> Node Ev
show_hom (MkRD  rk dir lines) = 
  div [class "grid-y"] [ -- grid-padding-y
  
    div [class "large-1 cell"] [
      h5 [] [fromString "\{show dir} Route"]
      ,p [] [fromString $ show_RouteKey rk]
    ]
    
    --,div [class "route-data large-12 cell"] (map show_route_grid_item (route_grid_items $ reverse lines) )
    ,div [class "route-data large-12 cell"] (map show_route_grid_item (route_grid_items lines) )    
{-  
    ,div [class "medium-3 cell"] [
      div [class "callout secondary"] [
        h4 [] ["Transit"] 
        ,(show_Hom1 [] )
      ]
    ]
    ,hr [] []
-}  
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
  (reas22,we) <- runStateT initState (JSMem.interpret_js (toWhs   demo_po_so)   ) 
    
  innerHtmlAt exampleDiv (show_hom we)
  --ini <- randomGame EN
  

  
  pure (feedback initState (fromState msf2), pure ())
