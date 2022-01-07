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
   interpret  (NewRoute dt route) = do
                (MkSS fx_map routes led_map rjm j user_data ws)<-get             
                let route_ref = routeSha dt route                 
                    r_k : RouteKey
                    r_k = (MkRK dt route_ref Progress)                                                   
                    routes' : SortedMap RouteKey RouteSumT
                    routes' = insert r_k  route routes

                put (MkSS fx_map routes' led_map rjm j user_data ws)
                pure r_k

   interpret  (SetFxData r_k fx) = do
                (MkSS fx_map routes led_map rjm j user_data ws)<-get                          
                let fx_map' : SortedMap RouteKey FxData
                    fx_map' = insert r_k fx fx_map                 
                put (MkSS fx_map' routes led_map rjm j user_data ws)

   interpret (UpdateUserData user_data ) = do
                (MkSS fx_map routes led_map rjm j udm ws)<-get
                let udm' = userDataToMap user_data
                put (MkSS fx_map routes led_map rjm j udm' ws)

   interpret (GetUserDataW ) = do
                (MkSS fx_map routes led_map rjm j user_data_map ws)<-get
                pure user_data_map

   interpret (CloseRoute route_ref@(MkRK date ref state)   ) = do     
               (MkSS fx_map routes led_map rjm j user_data_map ws)<-get
               case lookup route_ref routes of
                 Nothing => pure ()
                 (Just this) => do
                     let new_ref : RouteKey
                         new_ref = (MkRK date ref Completed)
                         routes' : SortedMap RouteKey RouteSumT
                         routes' = insert new_ref this routes

                         routes'' : SortedMap RouteKey RouteSumT
                         routes'' = delete route_ref routes'
                     put (MkSS fx_map routes'' led_map rjm j user_data_map ws)

               pure ()
   interpret (GetFxData rk) = do
               (MkSS fx_map routes led_map rjm j user_data_map ws)<-get
               pure (lookup rk fx_map)

   interpret (GetRoute rk) = do
               (MkSS fx_map routes led_map rjm j user_data_map ws)<-get
               pure (lookup rk routes)
   interpret (Put ref (MkMK f t ledger) je) = do
                pure ()
   --interpret Get = Get

   interpret (Get key) = do 
        (MkSS fx_map routes led_map rjm j user_data ws)<-get
        let muf1 : Maybe (List WhsEntry)
            muf1 = (lookup key rjm)
        case muf1 of
           Just xs => pure xs
           Nothing => pure []

   interpret (Log x) = do
        (MkSS fx_map routes led_map rjm js user_data ws)<-get
        let js'= (x::js)
        --putStrLn $ show x
        --putStrLn ""
        put (MkSS fx_map routes led_map rjm js' user_data ws)

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
   interpret_d (Put ref mkey@(MkMK f t ledger) je) = do   
                --(MkSS fx_map routes led_map rjm j user_data)<-get             
                let whs_e : WhsEntry
                    whs_e = MkWE ref je
                
                mp_x <- lookup_typeptr mkey --DirectoryMap.lookup mkey 
                case mp_x of
                   Nothing => do
                      p_new <- runHCommandST (new_list lt_whse) HCMD_DIR
                      ret <- runHCommandST (append_WhsEntry lt_whse p_new whs_e) HCMD_DIR
                      retx<-DirectoryMap.insert mkey ret ROUTE_JOURNAL_DIR
                      pure ()
                   Just p_list => do
                      ret <- runHCommandST (append_WhsEntry lt_whse p_list whs_e) HCMD_DIR
                      retx<-DirectoryMap.insert mkey ret ROUTE_JOURNAL_DIR
                      pure ()
                pure ()
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
   
