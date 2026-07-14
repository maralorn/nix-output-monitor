module Main (main) where

import Relude
import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "nix-output-monitor" =<< getArgs
