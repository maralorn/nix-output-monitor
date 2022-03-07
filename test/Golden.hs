module Main where

import Relude

import qualified Data.String as String
import Data.Tuple.Extra (secondM)
import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)
import System.Random (randomIO)
import Test.HUnit (
  Counts (errors, failures),
  Test,
  Testable (test),
  assertBool,
  assertEqual,
  runTestTT,
  (~:),
 )

import NOM.IO (processTextStream)
import NOM.Parser (parser)
import NOM.State (
  DependencySummary (..),
  DerivationId,
  NOMV1State (..),
  getStorePathId,
  initalState,
  out2drv,
 )
import qualified NOM.State.CacheId.Map as CMap
import qualified NOM.State.CacheId.Set as CSet
import NOM.Update (
  detectLocalFinishedBuilds,
  maintainState,
  parseStorePath,
  updateState,
 )
import NOM.Update.Monad (UpdateMonad)
import NOM.Util (
  forMaybeM,
  passThroughBuffer,
  (<.>>),
  (|>),
 )

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

testBuild :: String -> (String -> NOMV1State -> IO ()) -> Bool -> Test
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
    endState <- processTextStream parser (passThroughBuffer (preserveStateSnd . updateState)) (second maintainState) Nothing finalizer (Nothing, firstState) (pure $ toText errors)
    asserts output (snd endState)

finalizer :: UpdateMonad m => (a, NOMV1State) -> m (a, NOMV1State)
finalizer = secondM (execStateT detectLocalFinishedBuilds)

preserveStateSnd :: Functor m => ((istate, state) -> m (istate, Maybe state)) -> (istate, state) -> m (istate, state)
preserveStateSnd update (i, s) = (i, s) |> update <.>> second (fromMaybe s)

golden1 :: Bool -> Test
golden1 = testBuild "golden1" $ \output endState@MkNOMV1State{fullSummary = summary@MkDependencySummary{..}} -> do
  let noOfBuilds = 4
  assertBool "Everything built" (CSet.null plannedBuilds)
  print summary
  assertBool "No running builds" (CMap.null runningBuilds)
  assertEqual "Builds completed" noOfBuilds (CMap.size completedBuilds)
  let outputStorePaths = mapMaybe parseStorePath (String.lines output)
  assertEqual "All output paths parsed" noOfBuilds (length outputStorePaths)
  let outputDerivations :: [DerivationId]
      outputDerivations =
        flip evalState endState $
          forMaybeM outputStorePaths $
            getStorePathId >=> out2drv
  assertEqual "Derivations for all outputs have been found" noOfBuilds (length outputDerivations)
  assertBool "All found derivations have successfully been built" (CSet.isSubsetOf (CSet.fromFoldable outputDerivations) (CMap.keysSet completedBuilds))
