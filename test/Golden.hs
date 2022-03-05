module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.String as String
import NOM.IO
import NOM.Parser
import NOM.State
import NOM.State (out2drv)
import NOM.Update
import NOM.Update.Monad (UpdateMonad)
import NOM.Util (passThroughBuffer, (.>), (<.>>), (<|>>), (|>), forMaybeM)
import Relude
import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)
import System.Random (randomIO)
import Test.HUnit
import qualified NOM.State.CacheId.Map as CMap
import qualified NOM.State.CacheId.Set as CSet
import NOM.Util (secondM)

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
    endState <- processTextStream parser (passThroughBuffer (preserveStateSnd . updateState)) Nothing finalizer (Nothing, firstState) (pure $ toText errors)
    asserts output (snd endState)

finalizer :: UpdateMonad m => (a, NOMV1State) -> m (a, NOMV1State)
finalizer = secondM (execStateT detectLocalFinishedBuilds)

preserveStateSnd :: Functor m => ((istate, state) -> m (istate, Maybe state)) -> (istate, state) -> m (istate, state)
preserveStateSnd update (i, s) = (i, s) |> update <.>> second (fromMaybe s)

golden1 :: Bool -> Test
golden1 = testBuild "golden1" $ \output endState@MkNOMV1State {fullSummary = summary@MkDependencySummary {..}} -> do
  let noOfBuilds = 4
  assertBool "Everything built" (CSet.null plannedBuilds)
  print summary
  assertBool "No running builds" (CMap.null runningBuilds)
  assertEqual "Builds completed" noOfBuilds (CMap.size completedBuilds)
  let outputStorePaths = mapMaybe parseStorePath (String.lines output)
  assertEqual "All output paths parsed" noOfBuilds (length outputStorePaths)
  let 
    outputDerivations :: [DerivationId]
    outputDerivations = flip evalState endState $ forMaybeM outputStorePaths $
       getStorePathId >=> out2drv
  assertEqual "Derivations for all outputs have been found" noOfBuilds (length outputDerivations)
  assertBool "All found derivations have successfully been built" (CSet.isSubsetOf (CSet.fromFoldable outputDerivations) (CMap.keysSet completedBuilds))
