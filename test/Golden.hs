module Main where

import Relude

import Data.Text.IO as TextIO
import Relude.Unsafe
import Test.HUnit

import NOM.Update
import NOM.IO
import NOM.Parser
import NOM.State

main :: IO ()
main = do
  counts <-
    runTestTT $
      test
        [ "Golden 1" ~: do
            -- First we ensure that the necessary derivations exist in the store
            --pathForced <- doesFileExist "/nix/store/5gki8h3p9g63mp9q6xy9z7hlq1haqq6j-remote-build"
            --callProcess "nix-build" ["test/golden1.nix", "--no-out-link"]
            firstState <- initalState
            endState <-
              liftIO (TextIO.readFile "test/golden1.nix-build-output")
                & processTextStream parser updateState Nothing firstState
            print endState{buildReports = mempty, startTime = read "2021-03-04 00:35:28.627866489 UTC"}
            expectedState <- read . toString <$> TextIO.readFile "test/golden1.state"
            assertEqual
              "State matches"
              expectedState
              endState{startTime = read "2021-03-04 00:35:28.627866489 UTC", buildReports = mempty}
        ]
  if Test.HUnit.errors counts + failures counts == 0 then exitSuccess else exitFailure
