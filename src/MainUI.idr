module MainUI

import Examples.Selector
import Rhone.JS

%default total

covering
main : IO ()
main = runJS . ignore $ reactimateDomIni "reset" "select" ui
