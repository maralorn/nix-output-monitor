module Main where

import Data.Text.Lazy.IO as LTextIO
import IO
import Parser
import Relude
import Relude.Unsafe
import System.Process
import Test.HUnit

import Update
import Prelude ()

main :: IO ()
main = do
  counts <-
    runTestTT $
      test
        [ "Golden 1" ~: do
            -- First we ensure that the necessary derivations exist in the store
            callProcess "nix-build" ["test/golden1.nix", "--no-out-link"]
            log <- LTextIO.readFile "test/golden1.log"
            firstState <- initalState
            stateVar <- newTVarIO firstState
            endState <- processText parser stateVar updateState Nothing log
            expectedState <- read . toString <$> LTextIO.readFile "test/golden1.state"
            assertEqual "State matches" expectedState
              endState{startTime = read "2021-03-04 00:35:28.627866489 UTC", buildReports = mempty}
        ]
  if Test.HUnit.errors counts + failures counts == 0 then exitSuccess else exitFailure
