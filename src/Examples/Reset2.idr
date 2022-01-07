module Examples.Reset2

import Text.CSS
import Examples.CSS.Reset
import Examples.Util
import Rhone.JS
import Data.MSF.Trans
import Browser.WS2
import Browser.WebSocket


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
--import Data.HashDB.DataIO
--import Data.HashDB.Types
import Control.Monad.Either

import Category.Transaction.Demo2
import Category.Transaction.Warehouse


--import Examples.WsTest

public export
data EvWS = Msg BrowserEvent | Open BrowserEvent | Ocas --Open BrowserEvent

     
namespace JSMem
   export 
   interpret_js : LiftJSIO m => WhsEvent ta -> StateT SystemState m ta       
   interpret_js = MemoryMap.interpret


--%default total
public export
Ev : Type
Ev = EvWS --Int8 -> Int8

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

public export
M : Type -> Type
M = DomIO Ev JSIO

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
f udm (k,v) = tr [] [ c_pk,
                      name,
                      qty ]
      where c_pk : Node Ev
            c_pk = td [class "bom-item"] [fromString $show k]
            name : Node Ev
            name = td [class "bom-item"] [fromString $ fdsa $ lookup k (products udm) ]
            qty : Node Ev
            qty = td [class "bom-item"] [fromString $show v] --f2 k (show v)

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
  ]

    
export
get_msg' : LiftJSIO m => BrowserEvent -> m String   --BrowserEvent -> IO String
get_msg' e = do
    --x <- runJS (wsInfo e)
    x <- (get_data e)
    pure (x)

nextM : Ev -> StateT SystemState M (Event String)   --JSIO (Event String)
nextM (Msg d) = do
    x<- get_msg' d
    --u <- (liftJSIO . wsInfo d)
    pure (Ev x)
nextM _        = pure NoEv


onMsg :  MSF (StateT SystemState M) Ev () 
onMsg = (arrM nextM) ?>> arrM (\xl => printLn xl  ) >>> Trans.get >>> web_socket ^>> ifJust ( arrM $ \ws=>ws_close ws)


openM : Ev -> StateT SystemState M (Event () )   --JSIO (Event String)
openM (Open d) = pure (Ev ()) 
openM _        = pure NoEv


--sendMessage : Maybe 

onOpen :  MSF (StateT SystemState M) Ev () 
onOpen = (arrM openM) ?>> Trans.get >>> web_socket ^>> ifJust ( arrM $ \ws=>ws_send ws "Hello!" >> printLn "send Hello!")

msf2 : MSF (StateT SystemState M) Ev ()
msf2 =  fan_ [onMsg,
              onOpen]

export
ui2 : M (MSF M Ev (), JSIO ())
ui2 = do
  
  w_sock <- ws_new "ws://localhost:8000/websocket"  
  h_open   <- map Control.Monad.Dom.DomIO.DomEnv.handler DomIO.env  
  op <-  addEventListenerBE "open"  w_sock (h_open . Open)
  
  h_msg   <- DomEnv.handler <$> DomIO.env
  msg <-  addEventListenerBE "message"  w_sock (h_open . Msg)
  
  --msg <-  addEventListenerBE "open"  ws h_msg
  --msg <- ws_on_message ws (h_msg . Msg)
  --innerHtmlAt exampleDiv (content EN)
  
  (sstate,we) <- runStateT initState (JSMem.interpret_js (toWhs   demo_po_so)   )   
  innerHtmlAt exampleDiv (show_hom we)
  --ini <- randomGame EN
  --ws_send ws "Test message"
  --pure (feedback initState (fromState msf2),liftIO msg ) --   pure ()
  let new_sstate : SystemState
      new_sstate = record {web_socket = (Just w_sock) } sstate
      
  pure (feedback new_sstate (fromState msf2),pure () ) --   pure ()
  --pure (feedback initState (fromState msf2),pure () ) --   pure ()





-- Local Variables:
-- idris2-load-packages: ("contrib" "base" "rhone-js" "base" "contrib" "sop" "elab-util" "dom" "json" "rhone" "tailrec")
-- End:

