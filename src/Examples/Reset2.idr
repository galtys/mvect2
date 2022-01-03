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
   interpret : LiftJSIO m => WhsEvent ta -> StateT SystemState m ta       
   interpret  (NewRoute dt route) = do
                (MkSS fx_map routes led_map rjm j user_data)<-get             
                let route_ref = routeSha route                 
                    r_k : RouteKey
                    r_k = (MkRK dt route_ref Progress)                                                   
                    routes' : SortedMap RouteKey RouteSumT
                    routes' = insert r_k  route routes

                put (MkSS fx_map routes' led_map rjm j user_data)
                pure r_k

   interpret  (SetFxData r_k fx) = do
                (MkSS fx_map routes led_map rjm j user_data)<-get                          
                let fx_map' : SortedMap RouteKey FxData
                    fx_map' = insert r_k fx fx_map                 
                put (MkSS fx_map' routes led_map rjm j user_data)

   interpret (UpdateUserData user_data ) = do
                (MkSS fx_map routes led_map rjm j udm)<-get
                let udm' = userDataToMap user_data
                put (MkSS fx_map routes led_map rjm j udm')

   interpret (GetUserDataW ) = do
                (MkSS fx_map routes led_map rjm j user_data_map)<-get
                pure user_data_map

   interpret (CloseRoute route_ref@(MkRK date ref state)   ) = do     
               (MkSS fx_map routes led_map rjm j user_data_map)<-get
               case lookup route_ref routes of
                 Nothing => pure ()
                 (Just this) => do
                     let new_ref : RouteKey
                         new_ref = (MkRK date ref Completed)
                         routes' : SortedMap RouteKey RouteSumT
                         routes' = insert new_ref this routes

                         routes'' : SortedMap RouteKey RouteSumT
                         routes'' = delete route_ref routes'
                     put (MkSS fx_map routes'' led_map rjm j user_data_map)

               pure ()
   interpret (GetFxData rk) = do
               (MkSS fx_map routes led_map rjm j user_data_map)<-get
               pure (lookup rk fx_map)

   interpret (GetRoute rk) = do
               (MkSS fx_map routes led_map rjm j user_data_map)<-get
               pure (lookup rk routes)
   interpret (Put ref (MkMK f t ledger) je) = do
                (MkSS fx_map routes led_map rjm j user_data)<-get             
                let whs_e : WhsEntry
                    whs_e = MkWE ref je

                    key : MoveKey                 
                    key = (MkMK f t ledger)

                    kf : (RouteTypes.Location, Ledger)
                    kf = (f,ledger)

                    kt : (RouteTypes.Location, Ledger)
                    kt = (t,ledger)

                    Hom11_2_LM : Hom11 -> LocationMap
                    Hom11_2_LM h11 = led2'' where
                       led1' : LocationMap
                       led1' = update_ledger kf ( dx h11) led_map
                       led1'' : LocationMap
                       led1'' = update_ledger kf (invHom1 $ cx h11) led1'
                       led2' : LocationMap
                       led2' = update_ledger kt (invHom1 $ dx h11) led1''
                       led2'' : LocationMap
                       led2'' = update_ledger kt (cx h11) led2'

                    je2lm : FxEvent -> LocationMap
                    je2lm (Fx121 d h121 ) = Hom11_2_LM ( fromH121 h121 ) --(MkH11 (dx h121) (cx h121) )
                    je2lm (Fx11  d h11 ) = Hom11_2_LM h11

                    led' : LocationMap
                    led' = je2lm je

                case (lookup key rjm) of
                   Nothing => do
                      let rjm' = insert key [whs_e] rjm
                      put (MkSS fx_map routes led' rjm' j user_data)

                   Just je_list => do
                      let rjm' = insert key (whs_e::je_list) rjm
                      put (MkSS fx_map routes led' rjm' j user_data)
                pure ()

   interpret (Get key) = do 
        (MkSS fx_map routes led_map rjm j user_data)<-get
        let muf1 : Maybe (List WhsEntry)
            muf1 = (lookup key rjm)
        case muf1 of
           Just xs => pure xs
           Nothing => pure []

   interpret (Log x) = do
        (MkSS fx_map routes led_map rjm js user_data)<-get
        let js'= (x::js)
        --putStrLn $ show x
        --putStrLn ""
        put (MkSS fx_map routes led_map rjm js' user_data)

   interpret (Show x) = putStrLn $ show x
   interpret (Pure x) = pure x
   interpret (Bind x f) = do res <- interpret x
                             interpret (f res)




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
  (reas22,h1) <- runStateT initState (JSMem.interpret (toWhs   demo_po_so)   ) 
    
  innerHtmlAt exampleDiv (show_hom h1)
  --ini <- randomGame EN
  

  
  pure (feedback initState (fromState msf2), pure ())
