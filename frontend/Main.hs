module Main where

import Common (haskellMisoComponent)
import Miso (miso, run)
import Miso.String


main :: IO ()
main = run (miso haskellMisoComponent)
