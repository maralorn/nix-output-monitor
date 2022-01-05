module Main where

import Relude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.String as String
import System.Process (readProcessWithExitCode)
import System.Random (randomIO)
import Test.HUnit

import NOM.IO
import NOM.Parser
import NOM.State
import NOM.Update

golden1 :: Test
golden1 =
  "Golden 1" ~: do
    seed <- randomIO @Int
    (_, output, errors) <- readProcessWithExitCode "nix-build" ["test/golden1.nix", "--no-out-link", "--argstr", "seed", show seed] ""
    let noOfBuilds = 4
    firstState <- initalState
    endState <- processTextStream parser updateState Nothing firstState (pure $ toText errors)
    print endState
    assertBool "Everything built" (Set.null $ outstandingBuilds endState)
    assertBool "No running builds" (Set.null $ fold $ runningBuilds endState)
    assertEqual "Builds completed" noOfBuilds (Set.size $ fold $ completedBuilds endState)
    let outputStorePaths = mapMaybe parseStorePath (String.lines output)
    assertEqual "All output paths parsed" noOfBuilds (length outputStorePaths)
    let outputDerivations = mapMaybe (`Map.lookup` outputToDerivation endState) outputStorePaths
    assertEqual "Derivations for all outputs have been found" noOfBuilds (length outputDerivations)
    assertBool "All found derivations have successfully been built" (all (`Set.member` (fold $ completedBuilds endState)) outputDerivations)

main :: IO ()
main = do
  counts <- runTestTT $ test [golden1]
  if Test.HUnit.errors counts + failures counts == 0 then exitSuccess else exitFailure
