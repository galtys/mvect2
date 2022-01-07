module Examples.Ws_test

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


get_msg : LiftJSIO m => BrowserEvent -> m String
get_msg e = do
    x <- wsInfo e
    pure (msg x)


