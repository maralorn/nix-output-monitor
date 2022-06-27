module Main (main) where

import Relude

import Data.String qualified as String
import System.Environment qualified
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
import NOM.Parser (parseStorePath, parser)
import NOM.State (
  DependencySummary (..),
  DerivationId,
  NOMV1State (..),
  getStorePathId,
  initalState,
  out2drv,
 )
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.Update (
  detectLocalFinishedBuilds,
  maintainState,
  updateState,
 )
import NOM.Update.Monad (UpdateMonad)
import NOM.Util (forMaybeM)
import NOM.Print (Config(..))

tests :: [Bool -> Test]
tests = [golden1]

label :: (Semigroup a, IsString a) => Bool -> a -> a
label withNix name = name <> if withNix then " with nix" else " with log from file"

main :: IO ()
main = do
  withNix <- isNothing <$> System.Environment.lookupEnv "TESTS_FROM_FILE"
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
    endState <- processTextStream (MkConfig False False)parser (preserveStateSnd . updateState) (second maintainState) Nothing finalizer (Nothing, firstState) (pure $ Right (encodeUtf8 errors))
    asserts output (snd endState)

finalizer :: UpdateMonad m => StateT (a, NOMV1State) m ()
finalizer = do
  (a, oldState) <- get
  newState <- execStateT detectLocalFinishedBuilds oldState
  put (a, newState)

preserveStateSnd :: Monad m => ((istate, state) -> m (errors, (istate, Maybe state))) -> StateT (istate, state) m errors
preserveStateSnd update = do
  (i, s) <- get
  (errors, (newI, newS)) <- lift $ update (i, s)
  put (newI, fromMaybe s newS)
  pure errors

golden1 :: Bool -> Test
golden1 = testBuild "golden1" $ \output endState@MkNOMV1State{fullSummary = MkDependencySummary{..}} -> do
  let noOfBuilds :: Int
      noOfBuilds = 4
  assertBool "Everything built" (CSet.null plannedBuilds)
  assertBool "No running builds" (CMap.null runningBuilds)
  assertEqual "Builds completed" noOfBuilds (CMap.size completedBuilds)
  let outputStorePaths = mapMaybe parseStorePath (String.lines output)
  assertEqual "All output paths parsed" noOfBuilds (length outputStorePaths)
  let outputDerivations :: [DerivationId]
      outputDerivations =
        flip evalState endState $
          forMaybeM outputStorePaths \path -> do
            pathId <- getStorePathId path
            out2drv pathId
  assertEqual "Derivations for all outputs have been found" noOfBuilds (length outputDerivations)
  assertBool "All found derivations have successfully been built" (CSet.isSubsetOf (CSet.fromFoldable outputDerivations) (CMap.keysSet completedBuilds))
