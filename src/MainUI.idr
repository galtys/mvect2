module MainUI

import Examples.Selector
import Rhone.JS
import Crypto.Hash.SHA256
import Text.Html
%default total

covering
main : IO ()
main = do
   consoleLog "ocas32"
   consoleLog $ sha256 "ocas"
   consoleLog $ render content
   runJS . ignore $ reactimateDomIni "table" "select" Selector.ui32
