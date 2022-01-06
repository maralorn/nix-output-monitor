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
import System.Environment (lookupEnv)

tests :: [Bool -> Test]
tests = [golden1]

label :: (Semigroup a, IsString a) => Bool -> a -> a
label withNix name = name <> if withNix then " with nix" else " with log from file"

main :: IO ()
main = do
  withNix <- isNothing <$> lookupEnv "TESTS_FROM_FILE"
  counts <- runTestTT $
    test $ do
      test' <- tests
      if withNix
        then do
          test' <$> [True, False]
        else pure (test' False)
  if Test.HUnit.errors counts + failures counts == 0 then exitSuccess else exitFailure

testBuild :: String -> (String -> BuildState -> IO ()) -> Bool -> Test
testBuild name asserts withNix =
  label withNix name ~: do
    let callNix = do
          seed <- randomIO @Int
          readProcessWithExitCode
            "nix-build"
            ["test/" <> name <> ".nix", "--no-out-link", "--argstr", "seed", show seed]
            ""
            <&> (\(_, a, b) -> (a, b))
        readFiles = (,) <$> readFile ("test/" <> name <> ".stdout") <*> readFile ("test/" <> name <> ".stderr")
    (output, errors) <- if withNix then callNix else readFiles
    firstState <- initalState
    endState <- processTextStream parser updateState Nothing firstState (pure $ toText errors)
    asserts output endState

golden1 :: Bool -> Test
golden1 = testBuild "golden1" $ \output endState -> do
    let noOfBuilds = 4
    assertBool "Everything built" (Set.null $ outstandingBuilds endState)
    assertBool "No running builds" (Set.null $ fold $ runningBuilds endState)
    assertEqual "Builds completed" noOfBuilds (Set.size $ fold $ completedBuilds endState)
    let outputStorePaths = mapMaybe parseStorePath (String.lines output)
    assertEqual "All output paths parsed" noOfBuilds (length outputStorePaths)
    let outputDerivations = mapMaybe (`Map.lookup` outputToDerivation endState) outputStorePaths
    assertEqual "Derivations for all outputs have been found" noOfBuilds (length outputDerivations)
    assertBool "All found derivations have successfully been built" (all (`Set.member` (fold $ completedBuilds endState)) outputDerivations)
