module Utils (clearScreen) where

import System.Info (os)
import System.Process (callCommand)

clearScreen = callCommand $ if os == "mingw32" then "cls" else "clear"
