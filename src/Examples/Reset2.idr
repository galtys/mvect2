module Examples.Reset2

import Text.CSS
import Examples.CSS.Reset
import Examples.Util
import Rhone.JS
import Data.MSF.Trans
import Browser.WS2
import Browser.WebSocket
import Data.List1

import Data.IORef
import Data.MSF
import JS
import Data.List
import Data.List1
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
data EvWS = Msg BrowserEvent | Open BrowserEvent | Ocas | OpenRoute RouteKey | OpenAlloc AllocationRef | OpenRef Ref --Open BrowserEvent

     
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
show_ResPartner : String -> Maybe BrowseResPartner.RecordModel -> Node Ev
show_ResPartner x Nothing = 
     div [class "grid-x route-item-head callout"] [
        h6 [class "large-12 cell h4-right"] [fromString x]
     ]
show_ResPartner x (Just (MkRecordModel pk name use_parent_address active street contract city zip country_id parent_id child_ids email street2)) = 
    div [class "grid-x route-item-head callout"] [
        p  [class "large-12 cell para-padding h4-right"] [fromString $ (unMaybe street)++", "++(unMaybe street2)++", "++unMaybe zip]
      
    ]

show_route_doc_type : Maybe RouteSumT -> String
show_route_doc_type Nothing = ""
show_route_doc_type (Just rst) = show $ getDocRouteType rst

show_Name : String -> Maybe BrowseResPartner.RecordModel -> Node Ev
show_Name x Nothing = 
      div [class "grid-x route-item-head callout"] [
                 p [class "large-2 cell"] []
                 ,h6 [class "large-2 cell h4-left"] [fromString x]
                 
      ]
show_Name x (Just (MkRecordModel pk name use_parent_address active street contract city zip country_id parent_id child_ids email street2)) = 
     div [class "grid-x route-item-head callout"] [
        h4 [class "large-2 cell h4-left"] [fromString name]
        ,h6 [class "large-1 cell"] [fromString x]        
        ,p [class "large-7 cell"] []        
        ,p [class "large-2 cell h4-right"] [fromString email]        
     ]


show_Location : RouteTypes.Location -> Node Ev
show_Location Self = show_ResPartner "Self" Nothing
show_Location (In x) = show_ResPartner "Input" (Just x)
show_Location (Out x) = show_ResPartner "Output" (Just x)
show_Location (Border x) = show_ResPartner "Border" (Just x)
show_Location Init = show_ResPartner "Init" Nothing
show_Location Loss = show_ResPartner "Loss" Nothing
show_Location (Control x y) = show_ResPartner "Control \{show x}" (Just y)
show_Location (Partner Sale y) = show_ResPartner "Customer " (Just y)
show_Location (Partner Purchase y) = show_ResPartner "Supplier " (Just y)
show_Location (Transit x y) = show_ResPartner "Transit \{show x}" (Just y)
show_Location (Taxman x) = show_ResPartner "Taxman" (Just x)
show_Location (Bank x) = show_ResPartner "Bank" (Just x)

show_Owner : RouteTypes.Location -> Node Ev
show_Owner Self = show_Name "Self" Nothing
show_Owner (In x) = show_Name "Input" (Just x)
show_Owner (Out x) = show_Name "Output" (Just x)
show_Owner (Border x) = show_Name "Border" (Just x)
show_Owner Init = show_Name "Init" Nothing
show_Owner Loss = show_Name "Loss" Nothing
show_Owner (Control x y) = show_Name "Control \{show x}" (Just y)
show_Owner (Partner Sale y) = show_Name "Customer " (Just y)
show_Owner (Partner Purchase y) = show_Name "Supplier " (Just y)
show_Owner (Transit x y) = show_Name "Transit \{show x}" (Just y)
show_Owner (Taxman x) = show_Name "Taxman" (Just x)
show_Owner (Bank x) = show_Name "Bank" (Just x)


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
        [ thead []
                [tr [] 
                   [ th [] [""]
                   , th [] ["Description"]
                   , th []  ["Qty"] 
                
                   , th []  ["Price"]
                   , th []  ["Subtotal"]
                   , th []  [""] ]
                ]
         , tbody [] (map (fql udm) (colimQLine xs) )        
        ]
   ]
   

data RouteLineGridItem = MkLoc RouteTypes.Location | MkWE (List WhsEntry) | MkOwn RouteTypes.Location


show_RouteKey : RouteKey -> String
show_RouteKey (MkRK date ref state) = "Date: \{date}, Ref: \{ref}, State: \{show state}"

show_FxEvent : UserDataMap -> FxEvent -> Node Ev
show_FxEvent udm (Fx121 date y) =
                         div [class "callout"] [
                                span  [] [fromString date ]
                                ,( (show_HomQLine udm) $ toQLine $ toHom12 y)
                          ]
show_FxEvent udm (Fx11 date y) = 
                         div [class "callout"] [
                                span  [] [fromString date]
                                ,((show_Hom1 udm) $ toHom1 y)
                         ]
show_allocation_item : UserDataMap -> Ledger -> AllocationItem -> Node Ev
show_allocation_item ud ledger (MkAI supplier customer fx) =
        div [class "grid-x grid-padding-x"] [
            div [class "large-2 cell"] [fromString $ show_RouteKey supplier]
            ,div [class "large-2 cell"] [fromString $ show_RouteKey customer]        
            ,div [class "large-8 cell"] [show_FxEvent ud fx]
        ]
show_allocation_maybe : (Maybe AllocationEntry,UserDataMap) -> Node Ev 
show_allocation_maybe (Nothing, ud) = section [] []
show_allocation_maybe (Just (MkAE ledger moves), ud) = 
  section [] [
    div [class "callout"] (map (show_allocation_item ud ledger) moves)
  ]
get_route_number : SystemState -> RouteKey -> String
get_route_number ss rk@(MkRK date ref state) = 
       case (lookup rk (route_number ss)) of
         Nothing => ref
         (Just doc) => show doc

show_ref : SystemState -> Ref -> Node Ev
show_ref ss (MkAllocationRef ref) = tr [] [td [] ["Allocation"]
                                       , td [] []
                                       ,td [] [a [href "#",onClick (OpenAlloc ref)][fromString "\{ref}"]  ]
                                       ]  
show_ref ss route_ref@(MkRouteKeyRef rk@(MkRK date ref state)) = tr [] [td  [] [fromString $ show_route_doc_type route]
                                                          ,td [] [fromString "\{date}"]
                                                          ,td [] [a [href "#",onClick (OpenRoute rk)][ fromString $ get_route_number ss rk]] 
                                                        ] where
                     route : Maybe RouteSumT
                     route = lookup rk (routes ss)

show_refs : List Ref -> SystemState -> Node Ev
show_refs xs ss = section [] [table [class "hover"] 
                                    [thead []
                                          [tr []
                                             [ th [] ["Type"],
                                               th [] ["Date"],
                                               th [] ["Ref"]
                                             ]
                                          ]
                                     , tbody [] (map (show_ref ss) xs)
                                     ]     
                             ] 


drop_duplicates : Eq ty=>Ord ty=>List ty -> List ty
drop_duplicates xs = ret where 
    gs : List (List1 ty)
    gs = group $ sort xs
    
    ret : List ty
    ret = (map head gs)

show_refs_udm : (List Ref,UserDataMap) -> SystemState -> Node Ev
show_refs_udm (xx, y) ss = show_refs xx ss

show_route : SystemState -> RouteData -> Node Ev
show_route ss ( rd@(MkRD  rk dir lines m_rst) ) = 
  div [] [
     section [] [
       div [class "grid-y grid-padding-x"] [ 
         div [class "large-1 cell"] [
           h5 [] [fromString $ show_route_dt m_rst dir   ]
           ,p [] [fromString $ show_RouteKeyX rk]
         ]
       ,div [class "route-data large-11 cell"] (map show_route_grid_item (route_grid_items lines) )    
       ]
     ]
     ,section [] [
        h4 [class "h4-center"] ["Allocation"]
        ,(show_refs (toref $ drop_duplicates $listRouteKeys rd) ss)
     ]
  
  ] where  
  r_no : String
  r_no = unMaybe $ map show (lookup rk (route_number ss))
  
  show_RouteKeyX : RouteKey -> String
  show_RouteKeyX (MkRK date ref state) = "Date: \{date}, \{r_no}, State: \{show state}"
  
  
  show_route_dt : Maybe RouteSumT -> DirectionTag -> String
  show_route_dt Nothing dir = "\{show dir} Route"  
  show_route_dt m_rst y = show_route_doc_type m_rst
  
  toref : List RouteKey -> List Ref
  toref xs = map MkRouteKeyRef xs
  udm : UserDataMap
  udm = (user_data ss)
  
  route_grid_items : (List RouteLine) -> List RouteLineGridItem
  route_grid_items [] = []
  route_grid_items ((MkRL move f oh)::[]) = [MkOwn (from move),MkLoc (from move),  MkWE f,MkWE oh,    MkOwn (to move),MkLoc (to move)]
  route_grid_items ((MkRL move f oh)::xs) = [MkOwn (from move),MkLoc (from move),  MkWE f,MkWE oh]++(route_grid_items xs)
  
  show_Ref : Ref -> String
  show_Ref (MkAllocationRef x) = " Allocation: \{x}"
  show_Ref (MkRouteKeyRef this_rk@(MkRK date ref state)) = "  \{this_ref}" where
     this_ref : String
     this_ref = unMaybe $ map show (lookup this_rk (route_number ss))

  h2n : SortedMap H256 DocumentNumber
  h2n = (hash2name ss)
  
  show_whsentry : (WhsEntry) -> Node Ev
  show_whsentry ( we@(MkWE ref (Fx121 date y) mk)) = 
                         div [class "callout"] [
                                span  [] [fromString date ]
                                , span [class "doc-ref"] [fromString $ show_Ref ref]
                                ,h4 [class "h4-center"] [fromString $ show (getDocumentType we)] --, " ",fromString $ unMaybe $ map show (lookup (sha256 $ encode we) h2n)] 
                                ,h6 [class "h4-center"] [fromString $ unMaybe $ map show (lookup (sha256 $ encode we) h2n) ]
                                ,( (show_HomQLine udm) $ toQLine $ toHom12 y)

                         ]
  show_whsentry ( we@(MkWE ref (Fx11 date y) mk)) = 
                         div [class "callout"] [
                                span  [] [fromString date]
                                , span [class "doc-ref"] [fromString $ show_Ref ref]                                
                                ,h4 [class "h4-center"] [fromString $ show (getDocumentType we)] --, " ",fromString $ unMaybe $ map show (lookup (sha256 $ encode we) h2n)] 
                                ,h6 [class "h4-center"] [fromString $ unMaybe $ map show (lookup (sha256 $ encode we) h2n) ]                                
                                ,((show_Hom1 udm) $ toHom1 y)
                         ]
  
  show_route_grid_item : RouteLineGridItem -> Node Ev
  show_route_grid_item (MkLoc x) = (show_Location x)
  show_route_grid_item (MkOwn x) = (show_Owner x)
  show_route_grid_item (MkWE xs) = div [class "route-item"] (map show_whsentry xs)
  
  
{-
show_route1 : List1 (RouteData,UserDataMap) -> Node Ev
show_route1 (head ::: tail) = ?dasdfasd--(show_route head)

  div [class "grid-y"] [
     h3 [] ["Main"]
     ,(show_route head)
     ,h3 [] ["Related"]
     
  
  ]
-}





export
get_msg' : LiftJSIO m => BrowserEvent -> m String 
get_msg' e = do
    x <- (get_data e)
    pure (x)

nextM : Ev -> StateT SystemState M (Event String) 
nextM (Msg d) = do
    x<- get_msg' d
    pure (Ev x)
nextM _        = pure NoEv

onMsg :  MSF (StateT SystemState M) Ev () 
onMsg = (arrM nextM) ?>> arrM (\xl => printLn xl  ) >>> Trans.get >>> web_socket ^>> ifJust ( arrM $ \ws=>ws_close ws)

openM : Ev -> StateT SystemState M (Event () )   --JSIO (Event String)
openM (Open d) = pure (Ev ()) 
openM _        = pure NoEv
routeKeyM : Ev -> StateT SystemState M (Event (SystemState,RouteData) )   --JSIO (Event String)
routeKeyM (OpenRoute rk) = do
            ss <- get
            ret <- runStateT ss (JSMem.interpret_js (toWhs (get_hom' rk)  ))   
            --printLn $ lines $ (fst we)
            pure (Ev ret )             
routeKeyM _            = pure NoEv

allocationM : Ev -> StateT SystemState M (Event (Maybe AllocationEntry,UserDataMap) )   --JSIO (Event String)
allocationM (OpenAlloc ref) = do
            ss <- get
            (sstate,we) <- runStateT ss (JSMem.interpret_js (toWhs (read_allocation ref)  ))   
            --printLn $ lines $ (fst we)
            pure (Ev (we) )             
allocationM _            = pure NoEv

routeList1 : Ev -> StateT SystemState M (Event (List1 (RouteData,UserDataMap)) )   --JSIO (Event String)
routeList1 (OpenRoute rk) = do
            
            ss <- get
            (sstate,we) <- runStateT ss (JSMem.interpret_js (toWhs (read_route1 rk)  ))   
            --printLn $ lines $ (fst we)
            --pure (Ev (we) )             
            pure NoEv
            
routeList1 _            = pure NoEv


onRouteList1 : MSF (StateT SystemState M) Ev () 
onRouteList1 = (arrM routeList1) ?>> (arrM  (\xrf=>printLn "ocas") ) --(arrM $ \rf=> (innerHtmlAt formContentDiv (show_route1 rf))  )

onOpen :  MSF (StateT SystemState M) Ev () 
onOpen = (arrM openM) ?>> Trans.get >>> web_socket ^>> ifJust ( arrM $ \ws=>ws_send ws "Hello!" >> printLn "send Hello!")

msf2 : MSF (StateT SystemState M) Ev ()
msf2 =  fan_ [onMsg,
              onOpen,
              (arrM routeKeyM) ?>> (arrM $ \(sstate,ret)=> innerHtmlAt formContentDiv (show_route sstate ret)  ),
              (arrM allocationM) ?>> (arrM $ \ae=> innerHtmlAt formContentDiv (show_allocation_maybe ae)  ) ]

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
  
  
  (sstate,refs) <- runStateT sstate (JSMem.interpret_js (toWhs   list_refs )   )   
  innerHtmlAt exampleDiv (show_refs_udm  refs sstate)
  
  --innerHtmlAt formContentDiv (show_route_maybe we)    
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

