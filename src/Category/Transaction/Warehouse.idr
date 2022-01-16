module Category.Transaction.Warehouse

import Data.Zippable
import Generics.Derive
import Data.SortedMap
import Control.Monad.State
import Libc.Time

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


--%language ElabReflection
public export
update_ledger : (Location, Ledger) -> Hom1 -> LocationMap -> LocationMap 
update_ledger k [] m = m
update_ledger k@(ct,l) ( (pk,eq)::xs) m = ret where
          key : (Location, Ledger,ProdKey)
          key = (ct,l,pk)
          
          ret : LocationMap
          ret = case (lookup key m ) of
                  (Just q) => (update_ledger k xs (insert key (eq+q) m) )
                  Nothing => (update_ledger k xs  (insert key eq m)     )

namespace MemoryMap
   export
   interpret : HasIO io => WhsEvent a -> StateT SystemState io a       
   {-
   interpret  (SetAE ref entry) = do
                --(MkSS fx_map routes led_map rjm j user_data ws ae)<-get             
                ss <- get
                
                let ae = allocentry ss
                    ae' : SortedMap Ref AllocationEntry
                    ae' = insert ref entry ae
                    ss' : SystemState
                    ss' = record {allocentry = ae'} ss
                put ss' --(MkSS fx_map routes led_map rjm j user_data ws ae')
                
   interpret  (GetAE ref) = do
              --(MkSS fx_map routes led_map rjm j user_data ws ae)<-get                                        
              ss <- get
              let ret : Maybe AllocationEntry
                  ret = lookup ref (allocentry ss)
              pure ret
   -}              
   interpret  (ListRefs) = do
              --(MkSS fx_map routes led_map rjm j user_data ws ae)<-get
              ss <- get
              let a_keys : List RouteKey
                  a_keys = keys (allocentry ss) --ae
                  r_keys : List RouteKey --RouteKey
                  r_keys = (keys $ routes ss)
                  
              pure (a_keys++r_keys)
   interpret ListDocs = do
              ss <- get
              let dcs : List DocumentNumber
                  dcs = keys (name2hash ss)
              pure dcs --( keys (hash2name ss))
              
   interpret  (NewRoute dt route) = do
                --(MkSS fx_map routes led_map rjm j user_data ws ae)<-get             
                ss <- get
                let route_ref = routeSha dt route                 
                    r_k : RouteKey
                    r_k = (MkRK dt route_ref Progress)                                                   
                    routes' : SortedMap RouteKey RouteSumT
                    routes' = insert r_k  route (routes ss)

                put (record {routes=routes'} ss) --(MkSS fx_map routes' led_map rjm j user_data ws ae)
                pure r_k

   interpret  (SetFxData r_k fx) = do
                --(MkSS fx_map routes led_map rjm j user_data ws ae)<-get                          
                ss <- get
                let fx_map' : SortedMap RouteKey FxData
                    fx_map' = insert r_k fx (fx_map ss)
                put (record {fx_map=fx_map'} ss)--(MkSS fx_map' routes led_map rjm j user_data ws ae)

   interpret (UpdateUserData user_data ) = do
                --(MkSS fx_map routes led_map rjm j udm ws ae)<-get
                ss <- get
                let udm' = userDataToMap user_data 
                put (record {user_data=udm'} ss)     --(MkSS fx_map routes led_map rjm j udm' ws ae)

   interpret (GetUserDataW ) = do
                --(MkSS fx_map routes led_map rjm j user_data_map ws ae)<-get
                ss <- get
                pure (user_data ss)

   interpret (CloseRoute route_ref@(MkRK date ref state)   ) = do     
               --(MkSS fx_map routes led_map rjm j user_data_map ws ae)<-get
               ss <- get
               case lookup route_ref (routes ss) of
                 Nothing => pure ()
                 (Just this) => do
                     let new_ref : RouteKey
                         new_ref = (MkRK date ref Completed)
                         routes' : SortedMap RouteKey RouteSumT
                         routes' = insert new_ref this (routes ss)

                         routes'' : SortedMap RouteKey RouteSumT
                         routes'' = delete route_ref routes'
                     put (record {routes=routes''} ss)
                     --put (MkSS fx_map routes'' led_map rjm j user_data_map ws ae)

               pure ()
   interpret (GetFxData rk) = do
               --(MkSS fx_map routes led_map rjm j user_data_map ws ae)<-get
               ss <- get
               pure (lookup rk (fx_map ss) )

   interpret (GetRoute rk) = do
               --(MkSS fx_map routes led_map rjm j user_data_map ws ae)<-get
               ss <- get
               pure (lookup rk (routes ss) )
   interpret (Put ref key@(MkMK f t ledger) fe) = do
                --(MkSS fx_map routes led_map rjm j user_data ws ae)<-get             
                ss <- get
                let whs_e : WhsEntry
                    whs_e = MkWE ref fe key
                    
                    whs_e_cnt : String
                    whs_e_cnt = encode whs_e
                    
                    whs_h : H256
                    whs_h = (sha256 whs_e_cnt)
                    
                    doc_type : DocumentType
                    doc_type = getDocumentType whs_e 
                    
                    cnt : Int
                    cnt = case (lookup doc_type (counters ss)) of
                       Nothing => 0
                       Just c => c
                    
                    next : Int
                    next = cnt + 1
                    
                    new_doc_nr : DocumentNumber
                    new_doc_nr = DocNr doc_type Nothing next
                    --key : MoveKey                 
                    --key = (MkMK f t ledger)
                    counters' : SortedMap DocumentType Int
                    counters' = insert doc_type next (counters ss)
                    
                    name2hash' : SortedMap DocumentNumber H256
                    name2hash' = insert new_doc_nr whs_h (name2hash ss)
                    
                    hash2name' : SortedMap H256 DocumentNumber
                    hash2name' = insert whs_h new_doc_nr (hash2name ss)
                    
                    docs' : SortedMap H256 WhsEntry
                    docs' = insert whs_h whs_e (docs ss)
                    
                    kf : (Location, Ledger)
                    kf = (f,ledger)
                    kt : (Location, Ledger)
                    kt = (t,ledger)
                    Hom11_2_LM : Hom11 -> LocationMap
                    Hom11_2_LM h11 = led2'' where
                       led1' : LocationMap
                       led1' = update_ledger kf ( dx h11) (led_map ss)
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
                    led' = je2lm fe

                case (lookup key (jm ss) ) of
                   Nothing => do
                      let rjm' = insert key [whs_e] (jm ss) --rjm
                      
                      put (record {led_map=led', jm=rjm', name2hash=name2hash',hash2name=hash2name',counters=counters'} ss)
                      --put (MkSS fx_map routes led' rjm' j user_data ws ae)

                   Just je_list => do
                      let rjm' = insert key (whs_e::je_list) (jm ss) --rjm
                      put (record {led_map=led', jm=rjm', name2hash=name2hash',hash2name=hash2name',counters=counters'} ss)
                      --put (MkSS fx_map routes led' rjm' j user_data ws ae)
                pure new_doc_nr
   --interpret Get = Get               
   --interpret (Put ref (MkMK f t ledger) je) = do
   --             pure ()
   --interpret Get = Get
   interpret (SetRouteNumber doc rk) = do
        ss <- get
        let route_key' : SortedMap DocumentNumber RouteKey
            route_key' = insert doc rk (route_key ss)
            route_number' : SortedMap RouteKey DocumentNumber
            route_number' = insert rk doc (route_number ss)
            
        put (record {route_number = route_number', route_key=route_key'} ss)
        pure ()
   interpret (GetRouteNumber rk) = do
        ss <- get
        let num : Maybe DocumentNumber
            num = lookup rk (route_number ss)
        pure num
        
   interpret ListRoute = do
        ss <- get
        pure (keys (route_key ss))
        
   interpret (Get key) = do 
        --(MkSS fx_map routes led_map rjm j user_data ws ae)<-get
        ss <- get
        let muf1 : Maybe (List WhsEntry)
            muf1 = (lookup key (jm ss))
        case muf1 of
           Just xs => pure xs
           Nothing => pure []

   interpret (Log x) = do
        --(MkSS fx_map routes led_map rjm js user_data ws ae)<-get
        ss <- get
        let js'= (x:: (journal ss) )
        --putStrLn $ show x
        --putStrLn ""
        put (record {journal=js'} ss)
        --put (MkSS fx_map routes led_map rjm js' user_data ws ae)

   interpret (Show x) = putStrLn $ show x
   interpret (Pure x) = pure x
   interpret (Bind x f) = do res <- MemoryMap.interpret x
                             MemoryMap.interpret (f res)




export
runHCommandST : HasIO io=>MonadError DBError io => HCommand a -> String->StateT SystemState io a
runHCommandST (Store x tag) dir = storeHType x dir 
runHCommandST (Read x) dir = readHType x dir 
runHCommandST (Log x ) dir= printLn x
runHCommandST (Show x) dir = printLn $ show x
runHCommandST (LinkError x) dir = throwError EHashLink
runHCommandST (DecodeError x) dir= throwError (ErrorJS "Error while decoding JS")
runHCommandST (Pure val) dir = pure val
runHCommandST (Bind c f) dir = do 
                    res <- runHCommandST c dir
                    runHCommandST (f res) dir

lookup_route_st : HasIO io=>MonadError DBError io=>RouteKey->io (Maybe RouteSumT)
lookup_route_st rk = do
     x<-DirectoryMap.lookup rk ROUTE_DIR
     pure x
lookup_fxdata : HasIO io=>MonadError DBError io=>RouteKey->io (Maybe FxData)
lookup_fxdata rk = do
     x<-DirectoryMap.lookup rk FX_DIR
     pure x
lookup_typeptr : HasIO io=>MonadError DBError io=>MoveKey->io (Maybe TypePtr)
lookup_typeptr mk = do
     x<-DirectoryMap.lookup mk ROUTE_JOURNAL_DIR
     pure x

lookup_userdata : HasIO io=>MonadError DBError io=>io (Maybe UserData)
lookup_userdata = do
     x<-DirectoryMap.lookup "UserData" STATE_DIR
     pure x

{-
  HCMD_DIR = "/home/jan/github.com/mvect2/data/hcmd/"
  FX_DIR = "/home/jan/github.com/mvect2/data/fx/"
  ROUTE_DIR = "/home/jan/github.com/mvect2/data/route/"
  LED_DIR = "/home/jan/github.com/mvect2/data/led/"
  ROUTE_JOURNAL_DIR = "/home/jan/github.com/mvect2/data/route_journal/"
  JOURNAL_DIR = "/home/jan/github.com/mvect2/data/journal/"
  STATE_DIR = "/home/jan/github.com/mvect2/data/state/"
-}
                                        
namespace DirMap
   export
   lt_oje : HType
   lt_oje = toType "OwnerJournalEvent"
   lt_whse : HType
   lt_whse = toType "WhsEntry"
   
   lt_bom : HType
   lt_bom = toType "BrowseBoM.RecordModel"
   lt_product : HType 
   lt_product = toType "BrowseProduct.RecordModel"
   
   export
   new_list : (lt:HType)->HCommand TypePtr
   new_list lt = do
         x <- DBListStr.new lt 
         Pure x
   append_OwnerJournalEvent : TypePtr ->OwnerJournalEvent->HCommand TypePtr
   append_OwnerJournalEvent p_h oje = do
                let cnt : String
                    cnt = encode oje
                head <- Read p_h
                new_head <- DBListStr.append cnt head lt_oje
                Pure (ptr new_head)
   append_WhsEntry : (lt:HType) -> TypePtr ->WhsEntry->HCommand TypePtr
   append_WhsEntry lt p_h item = do
                let cnt : String
                    cnt = encode item
                head <- Read p_h
                new_head <- DBListStr.append cnt head lt
                Pure (ptr new_head)
   read_WhsEntry : (lt:HType) -> TypePtr ->HCommand (Maybe (List WhsEntry))
   read_WhsEntry lt pt = DBList.read pt lt
   
   read_BoM : (lt:HType) -> TypePtr ->HCommand (Maybe (List BrowseBoM.RecordModel))
   read_BoM lt pt = DBList.read pt lt 
   read_Product : (lt:HType) -> TypePtr ->HCommand (Maybe (List BrowseProduct.RecordModel))
   read_Product lt pt = DBList.read pt lt 
   
   export
   interpret_d : HasIO io=>MonadError DBError io=>WhsEvent a->StateT SystemState io a--io a
   {-
   interpret_d  (SetAE ref entry) = do
                --(MkSS fx_map routes led_map rjm j user_data ws ae)<-get             
                --let ae' : SortedMap Ref AllocationEntry
                --    ae' = insert ref entry ae
                    
                --put (MkSS fx_map routes led_map rjm j user_data ws ae')
                pure ()
      
   interpret_d  (GetAE ref) = do
              --(MkSS fx_map routes led_map rjm j user_data ws ae)<-get                                        
              --let ret : Maybe AllocationEntry
              --    ret = lookup ref ae
              pure Nothing
                
   -}
   interpret_d   ListRefs = pure []
   interpret_d ListDocs = pure []
   
   interpret_d (Put ref mkey@(MkMK f t ledger) je) = do   
                --(MkSS fx_map routes led_map rjm j user_data)<-get             
                let whs_e : WhsEntry
                    whs_e = MkWE ref je mkey
                
                mp_x <- lookup_typeptr mkey --DirectoryMap.lookup mkey 
                case mp_x of
                   Nothing => do
                      p_new <- runHCommandST (new_list lt_whse) HCMD_DIR
                      ret <- runHCommandST (append_WhsEntry lt_whse p_new whs_e) HCMD_DIR
                      retx<-DirectoryMap.insert mkey ret ROUTE_JOURNAL_DIR
                      pure (DocName "interpret_d")
                   Just p_list => do
                      ret <- runHCommandST (append_WhsEntry lt_whse p_list whs_e) HCMD_DIR
                      retx<-DirectoryMap.insert mkey ret ROUTE_JOURNAL_DIR
                      pure (DocName "interpret_d")
                
   interpret_d (Get mkey) = do 
        --(MkSS fx_map routes led_map rjm j user_data)<-get
        mp_x <- lookup_typeptr mkey
        case mp_x of
          Nothing => pure []
          Just p_h => do
              ret <- runHCommandST (read_WhsEntry lt_whse p_h) HCMD_DIR
              case ret of
                 Nothing => pure []
                 Just lst => pure lst
   interpret_d (SetRouteNumber doc rk) = do
        pure ()
   interpret_d (GetRouteNumber rk) = pure Nothing
           
   interpret_d (ListRoute ) = do
        pure []
        
   interpret_d (Log x) = do
        p_head<-DirectoryMap.lookup "journal_head" STATE_DIR        
        --let --js'= (x::js)
        case p_head of
           Nothing => pure ()
           Just p_x => do
               ret <- runHCommandST (append_OwnerJournalEvent p_x x) JOURNAL_DIR
               ret<-DirectoryMap.insert "journal_head" ret STATE_DIR
               pure ()   
   
   interpret_d  (NewRoute dt route) = do

                let route_ref = routeSha dt route                 
                    r_k : RouteKey
                    r_k = (MkRK dt route_ref Progress)
                ret<-DirectoryMap.insert r_k route ROUTE_DIR
                --put (MkSS fx_map routes' led_map rjm j user_data)
                pure r_k

   interpret_d  (SetFxData r_k fx) = do
                ret<-DirectoryMap.insert r_k fx FX_DIR           
                --put (MkSS fx_map' routes led_map rjm j user_data)
                pure ()
   interpret_d (UpdateUserData user_data ) = do
     ret<-DirectoryMap.insert "UserData" user_data STATE_DIR
     {-
     p_bom_list <- runHCommandST (DBList.write (boms user_data) lt_bom) BOM_DIR
     p_prod_list <- runHCommandST (DBList.write (products user_data) lt_product) PRODUCT_DIR
                 
     ret<-DirectoryMap.insert "BrowseBoM.RecordModel" p_bom_list STATE_DIR
     ret<-DirectoryMap.insert "BrowseProduct.RecordModel" p_bom_list STATE_DIR        
     --
     --
     -}          
     --(MkSS fx_map routes led_map rjm j udm)<-get
     --let udm' = userDataToMap user_data
     --put (MkSS fx_map routes led_map rjm j udm')
     pure ()
   interpret_d (GetUserDataW ) = do
     ret <- lookup_userdata
     case ret of
       Nothing => pure (userDataToMap (MkUD [] [] [] []))
       Just ud => pure (userDataToMap ud)
     --(MkSS fx_map routes led_map rjm j user_data_map)<-get
     {-
     mp_bom<-DirectoryMap.lookup "BrowseBoM.RecordModel" STATE_DIR        
     mp_prod<-DirectoryMap.lookup "BrowseProduct.RecordModel" STATE_DIR        
     case (mp_bom,mp_prod) of 
        (Just p_bom,Just p_prod) => do
           mb <- runHCommandST (read_BoM lt_bom p_bom) BOM_DIR
           mp <- runHCommandST (read_Product lt_product p_prod) PRODUCT_DIR
           case (mb,mp) of 
              (Just xb,Just xp) => pure (userDataToMap (MkUD xp [] xb []))
              _ => pure (userDataToMap (MkUD [] [] [] []))
        _ => pure (userDataToMap (MkUD [] [] [] []))
      -}
   interpret_d (CloseRoute route_ref@(MkRK date ref state)   ) = do     
               m_r <- lookup_route_st route_ref
               case m_r of
                  Nothing => pure ()
                  (Just r) => do
                       let new_ref : RouteKey
                           new_ref = (MkRK date ref Completed)
                       ret<-DirectoryMap.insert new_ref r ROUTE_DIR
                       ret<-DirectoryMap.delete route_ref ROUTE_DIR                       
                       pure ()
   interpret_d (GetFxData rk) = do
               ret <- lookup_fxdata rk
               pure ret --(lookup rk fx_map)

   interpret_d (GetRoute rk) = do
               m_r <- lookup_route_st rk
               pure m_r --(lookup rk routes)
                    

   interpret_d (Show x) = putStrLn $ show x
   interpret_d (Pure x) = pure x
   interpret_d (Bind x f) = do 
                          res <- interpret_d x
                          interpret_d (f res)
   
