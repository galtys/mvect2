module MainUI

import Examples.Selector
import Rhone.JS
import Crypto.Hash.SHA256
import Text.Html
%default total

covering
main : IO ()
main = do
   consoleLog "ocas"
   consoleLog $ sha256 "ocas"
   consoleLog $ render content
   runJS . ignore $ reactimateDomIni "balls" "select" ui
