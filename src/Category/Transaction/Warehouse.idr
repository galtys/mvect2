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
   interpret : WhsEvent a -> StateT SystemState IO a       
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

                    kf : (Location, Ledger)
                    kf = (f,ledger)

                    kt : (Location, Ledger)
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
        putStrLn $ show x
        putStrLn ""
        put (MkSS fx_map routes led_map rjm js' user_data)

   interpret (Show x) = putStrLn $ show x
   interpret (Pure x) = pure x
   interpret (Bind x f) = do res <- interpret x
                             interpret (f res)


namespace DirectoryMap
   export
   interpret_d : WhsEvent a -> StateT SystemState IO a       
   interpret_d  (NewRoute dt route) = do
                (MkSS fx_map routes led_map rjm j user_data)<-get             
                let route_ref = routeSha route                 
                    r_k : RouteKey
                    r_k = (MkRK dt route_ref Progress)                                                   
                    routes' : SortedMap RouteKey RouteSumT
                    routes' = insert r_k  route routes

                put (MkSS fx_map routes' led_map rjm j user_data)
                pure r_k

   interpret_d  (SetFxData r_k fx) = do
                (MkSS fx_map routes led_map rjm j user_data)<-get                          
                let fx_map' : SortedMap RouteKey FxData
                    fx_map' = insert r_k fx fx_map                 
                put (MkSS fx_map' routes led_map rjm j user_data)

   interpret_d (UpdateUserData user_data ) = do
                (MkSS fx_map routes led_map rjm j udm)<-get
                let udm' = userDataToMap user_data
                put (MkSS fx_map routes led_map rjm j udm')

   interpret_d (GetUserDataW ) = do
                (MkSS fx_map routes led_map rjm j user_data_map)<-get
                pure user_data_map

   interpret_d (CloseRoute route_ref@(MkRK date ref state)   ) = do     
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
   interpret_d (GetFxData rk) = do
               (MkSS fx_map routes led_map rjm j user_data_map)<-get
               pure (lookup rk fx_map)

   interpret_d (GetRoute rk) = do
               (MkSS fx_map routes led_map rjm j user_data_map)<-get
               pure (lookup rk routes)
   interpret_d (Put ref (MkMK f t ledger) je) = do
                (MkSS fx_map routes led_map rjm j user_data)<-get             
                let whs_e : WhsEntry
                    whs_e = MkWE ref je

                    key : MoveKey                 
                    key = (MkMK f t ledger)

                    kf : (Location, Ledger)
                    kf = (f,ledger)

                    kt : (Location, Ledger)
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

   interpret_d (Get key) = do 
        (MkSS fx_map routes led_map rjm j user_data)<-get
        let muf1 : Maybe (List WhsEntry)
            muf1 = (lookup key rjm)
        case muf1 of
           Just xs => pure xs
           Nothing => pure []

   interpret_d (Log x) = do
        (MkSS fx_map routes led_map rjm js user_data)<-get
        let js'= (x::js)
        putStrLn $ show x
        putStrLn ""
        put (MkSS fx_map routes led_map rjm js' user_data)

   interpret_d (Show x) = putStrLn $ show x
   interpret_d (Pure x) = pure x
   interpret_d (Bind x f) = do 
                      res <- interpret_d x
                      interpret_d (f res)


