module Examples.WsTest

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
export
get_msg' : LiftJSIO m => BrowserEvent -> m String   --BrowserEvent -> IO String
get_msg' e = do
    --x <- runJS (wsInfo e)
    x <- (get_data e)
    pure (x)
-}

{-
get_msg : LiftJSIO m => BrowserEvent -> m String
get_msg e = do
    --x <- wsInfo e
    x <- runJS $ wsInfo e
    pure (msg x)

-}
