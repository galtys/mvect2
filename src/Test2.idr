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
     let new_list : HCommand TypePtr
         new_list = do
             x <- DBListStr.new DirMap.lt_oje
             Pure x
     p_nil <- runHCommand new_list JOURNAL_DIR
     --printLn p_nil
     --printLn (sha256 $ encode "journal_head")
     ret<-DirectoryMap.insert "journal_head" p_nil STATE_DIR
     
     reas <- execStateT initState (interpret_d demo_po_so_whs)
     pure ()
