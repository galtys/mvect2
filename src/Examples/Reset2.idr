module Examples.Reset2

import Text.CSS
import Examples.CSS.Reset
import Examples.Util
import Rhone.JS
import Data.MSF.Trans


import Data.IORef
import Data.MSF
import JS


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








-------------------------------------------------

||| Web.Internal.UIEventsTypes
export data WebSocketEvent: Type where [external]

-- Web.Internal.Types
public export
JSType WebSocketEvent where
  parents =  [ JS.Object.Object ]
  mixins =  []


||| Web.Internal.WebSocketPrim
namespace WebSocketEvent
  -- message
  export
  %foreign "browser:lambda:x=>x.data"
  prim__data : WebSocketEvent -> PrimIO String

  export
  %foreign "browser:lambda:x=>x.origin"
  prim__origin : WebSocketEvent -> PrimIO String
    
  export
  %foreign "browser:lambda:x=>x.lastEventId"
  prim__lastEventId : WebSocketEvent -> PrimIO String
  
  export
  %foreign "browser:lambda:x=>x.source"
  prim__source : WebSocketEvent -> PrimIO String
  
  export
  %foreign "browser:lambda:x=>x.ports"
  prim__ports : WebSocketEvent -> PrimIO String

  -- close
  export
  %foreign "browser:lambda:x=>x.code"
  prim__code : WebSocketEvent -> PrimIO String
  
  export
  %foreign "browser:lambda:x=>x.reason"
  prim__reason : WebSocketEvent -> PrimIO String

  export
  %foreign "browser:lambda:x=>x.wasClean"
  prim__wasClean : WebSocketEvent -> PrimIO String

  -- error is using a generic event
  -- open is using a generic event


  --
  export
  getData : (0 _ : JSType t1)
         => {auto 0 _ : Elem WebSocketEvent (Types t1)}
         -> (obj : t1)
         -> JSIO String
  getData a = primJS $  WebSocketEvent.prim__data (up a)
          
  export
  origin : (0 _ : JSType t1)
         => {auto 0 _ : Elem WebSocketEvent (Types t1)}
         -> (obj : t1)
         -> JSIO String
  origin a = primJS $  WebSocketEvent.prim__origin (up a)
         
  export
  source : (0 _ : JSType t1)
         => {auto 0 _ : Elem WebSocketEvent (Types t1)}
         -> (obj : t1)
         -> JSIO String
  source a = primJS $  WebSocketEvent.prim__source (up a)
  
  
  
public export
record WsMessageInfo where
  constructor MkWsMI
  msg : String
  origin : String
  source : String
  
export
wsInfo : WebSocketEvent -> JSIO WsMessageInfo
wsInfo e =
  [| MkWsMI
     (getData e)
     (origin e)
     (source e)
   |]


public export
data EvWS = Msg WebSocketEvent 


{-   
public export
data WSEvent : Type -> Type where
   Msg : (WsMessageInfo -> Maybe a) -> WSEvent a
-}
   
public export
data WebsocketID : String -> Type where [external]
     --Socket: String -> WebsocketID

--WSid : Type
--Wsid = WebsocketID "ws://localhost:8000/websocket"

         
%foreign "browser:lambda:(s) => new WebSocket(s)"
prim__new_ws : (s:String) -> PrimIO (WebsocketID s)

%foreign "browser:lambda:(s,h) => s.addEventListener('open', h)"
prim__on_open : {s:String} -> (WebsocketID s) -> (WebSocketEvent -> IO Bits32) -> PrimIO ()

%foreign "browser:lambda:(s,h) => s.addEventListener('message', h)"
prim__on_message : {s:String} -> (WebsocketID s) -> (WebSocketEvent -> IO Bits32) -> PrimIO ()

export
ws_new : HasIO io => (s:String) -> io (WebsocketID s)
ws_new s =  primIO $ prim__new_ws s

    
export
ws_on_open : HasIO io => {s:String} -> (WebsocketID s) -> (WebSocketEvent -> JSIO ()) -> io (IO ())
ws_on_open s run = do
     ref <- newIORef (the Bits32 0)
     primIO $ prim__on_open s (\dt => runJS (run dt) >> readIORef ref)
     pure (writeIORef ref 1)

export
ws_on_message : HasIO io => {s:String} -> (WebsocketID s) -> (WebSocketEvent -> JSIO ()) -> io (IO ())
ws_on_message s run = do
     ref <- newIORef (the Bits32 0)
     primIO $ prim__on_message s (\dt => runJS (run dt) >> readIORef ref)
     pure (writeIORef ref 1)



--ws_open : 

--%foreign "browser:lambda:(s,h) => s.addEventListener('open', h)"



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
show_Location (Partner Sale y) = show_ResPartner "Customer " (Just y)
show_Location (Partner Purchase y) = show_ResPartner "Supplier " (Just y)
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

fdsa : Maybe BrowseProduct.RecordModel -> String
fdsa Nothing = ""
fdsa (Just x) = "[\{default_code x}] \{name_}" where
    get_name : List PrimProductTemplate.RecordModel -> String
    get_name [] = ""
    get_name  (x::xs) = name x
    name_ : String
    name_ = "\{get_name $product_tmpl_id x}"

bom_to_hom : BoM32 -> Hom1
bom_to_hom b32 = (variants_BoM32  (mult_BoM32 1 [b32]) )



f : UserDataMap -> Product  -> Node Ev
f udm (k,v) = tr [] [ td [] [fromString $show k],
                      td [] [fromString $ fdsa $ lookup k (products udm) ],
                      td [] [fromString $show v] ] --f2 k (show v)

show_Hom1 : UserDataMap  -> Hom1 -> Node Ev
show_Hom1 udm dx1 =
  table [ class "unstriped hover" ]
        [ thead []
                [tr [] 
                   [ th [] ["ProdKey"]
                   , th [] ["Description"]
                   , th []  ["Qty"] 
                    ]]
        ,tbody [] (map (f udm) dx1)                       
      ]
      
show_Hom1_tbody : UserDataMap  -> Hom1 -> Node Ev
show_Hom1_tbody udm dx1 =
  table [ class "unstriped hover" ]
        [ tbody [] (map (f udm) dx1)                       
        ]

fql : UserDataMap  -> QLine -> Node Ev
fql udm (MkQL dxpk Nothing q cxpk price) = tr [] [td [] [fromString $show dxpk],
                                              td [] [fromString $ fdsa $ lookup dxpk (products udm)], 
                                              td [] [fromString $show q],
                                          
                                              td [] [fromString $show price],
                                              td [] [fromString $show (q*price)],
                                              td [] [fromString $show cxpk]]

fql udm (MkQL dxpk (Just bom) q cxpk price) = tr [] [td [] [fromString $show dxpk],
                                              td [] [div [] [span [] [fromString $ fdsa $ lookup dxpk (products udm)]
                                                             --p [] [fromString $ show $ bom_to_hom bom]
                                                             ,(show_Hom1_tbody udm (bom_to_hom bom))
                                                             ]], 
                                                     --unMaybe
                                              td [] [fromString $show q],
                                          
                                              td [] [fromString $show price],
                                              td [] [fromString $show (q*price)],
                                              td [] [fromString $show cxpk]]

show_HomQLine : UserDataMap -> HomQLine -> Node Ev
show_HomQLine udm xs = div [] [
  table [ class "unstriped hover" ]
        [  thead []
                [tr [] 
                   [ th [] [""]
                   , th [] ["Description"]
                   , th []  ["Qty"] 
                
                   , th []  ["Price"]
                   , th []  ["Subtotal"]
                   , th []  [""] ]
                ]
                {-
                [tr [] 
                   [ th [Str "width" "20"] [""]
                   , th [Str "width" "210"] ["Description"]
                   , th [Str "width" "50"]  ["Qty"] 
                
                   , th [Str "width" "40"]  ["Price"]
                   , th [Str "width" "40"]  ["Subtotal"]
                   , th [Str "width" "60"]  [""] ]
                ]
                -}
         , tbody [] (map (fql udm) (colimQLine xs) )        
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


show_whsentry : UserDataMap -> (WhsEntry,RouteTypes.DocumentType) -> Node Ev
show_whsentry udm (MkWE ref (Fx121 date y), dt) = 
                         div [class "callout"] [
                                span  [] [fromString date ]
                                , span [class "doc-ref"] [fromString $ show_Ref ref]
                                ,h4 [] [fromString $ show dt] 
                                ,( (show_HomQLine udm) $ toQLine $ toHom12 y)
                                --, hr [] []
                         ]
show_whsentry udm (MkWE ref (Fx11 date y),dt) = 
                         div [class "callout"] [
                                span  [] [fromString date]
                                , span [class "doc-ref"] [fromString $ show_Ref ref]                                
                                ,h4 [] [fromString $ show dt] 
                                ,((show_Hom1 udm) $ toHom1 y)
                                --, hr [] []                                
                         ]


data RouteLineGridItem = MkLoc RouteTypes.Location | MkWE (List (WhsEntry,RouteTypes.DocumentType))

route_grid_items : (List RouteLine) -> List RouteLineGridItem
route_grid_items [] = []
route_grid_items ((MkRL move f oh)::xs) = [MkLoc (from move),MkWE f,MkWE oh]++(route_grid_items xs)

--route_grid_items ((MkRL move f oh)::xs) = [MkLoc (from move),MkLoc (to move),MkWE f,MkWE oh]++(route_grid_items xs)


show_route_grid_item : UserDataMap -> RouteLineGridItem -> Node Ev
show_route_grid_item udm (MkLoc x) = (show_Location x)
show_route_grid_item udm (MkWE xs) = div [class "route-item"] (map (show_whsentry udm) xs)


show_hom : (RouteData,UserDataMap) -> Node Ev
show_hom ((MkRD  rk dir lines), udm) = 
  div [class "grid-y"] [ -- grid-padding-y
  
    div [class "large-1 cell"] [
      h5 [] [fromString "\{show dir} Route"]
      ,p [] [fromString $ show_RouteKey rk]
    ]
    
    --,div [class "route-data large-12 cell"] (map show_route_grid_item (route_grid_items $ reverse lines) )
    ,div [class "route-data large-12 cell"] (map (show_route_grid_item udm) (route_grid_items lines) )    
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
            
public export
record DomEnv (event : Type) where
  constructor MkDomEnv
  pre      : String
  unique   : IORef Nat
  handler  : event -> JSIO ()
            
-}

export
ui2 : M (MSF M Ev (), JSIO ())
ui2 = do
  --innerHtmlAt exampleDiv (content EN)
  (reas22,we) <- runStateT initState (JSMem.interpret_js (toWhs   demo_po_so)   ) 
  
  ws <- ws_new "ws://localhost:8000/websocket"
  
  
  --h_open   <- DomEnv.handler <$> DomIO.env
  
  h_open   <- map Control.Monad.Dom.DomIO.DomEnv.handler DomIO.env
  
  --op <- ws_on_open ws (h_open . Msg)
  {-
  let ocas : (WebSocketEvent -> JSIO ())
      ocas = (h_open . Msg)
    -}
      
  h_msg   <- DomEnv.handler <$> DomIO.env
  --msg <- ws_on_message ws (h_open . Msg)
  
  --(WebsocketID "ws://localhost:8000/websocket")
  
  innerHtmlAt exampleDiv (show_hom we)
  --ini <- randomGame EN
  

  
  pure (feedback initState (fromState msf2), pure () )





