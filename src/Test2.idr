module Test2

import Category.Transaction.Types2
import Category.Transaction.Demo2
import Control.Monad.Either
import Category.Transaction.Owner
import Category.Transaction.Warehouse
import Control.Monad.State
import Data.HashDB.Types
import Data.HashDB.DataIO
import Config
import JSON
import Crypto.Hash.SHA256

export
run_interpret_d : HasIO io => MonadError DBError io=>io ()
run_interpret_d = do  
     p_nil <- runHCommand (DirMap.new_list DirMap.lt_oje) JOURNAL_DIR
     ret<-DirectoryMap.insert "journal_head" p_nil STATE_DIR
     
     reas <- execStateT initState (interpret_d demo_po_so_whs)
     pure ()
