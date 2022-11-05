module Main (main) where

import Relude

import Data.Text qualified as Text
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

import Control.Monad.Writer.Strict (WriterT (runWriterT))
import NOM.Builds (parseStorePath)
import NOM.IO (processTextStream)
import NOM.IO.ParseStream.Attoparsec (parseStreamAttoparsec)
import NOM.Parser (parser)
import NOM.Print (Config (..))
import NOM.State (
  DependencySummary (..),
  DerivationId,
  NOMV1State (..),
  getStorePathId,
  initalStateFromBuildPlatform,
  outPathToDerivation,
 )
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.Update (
  detectLocalFinishedBuilds,
  maintainState,
  updateStateNixOldStyleMessage,
 )
import NOM.Update.Monad (UpdateMonad)
import NOM.Util (forMaybeM)

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

testBuild :: String -> (Text -> NOMV1State -> IO ()) -> Bool -> Test
testBuild name asserts withNix =
  label withNix name ~: do
    let callNix = do
          seed <- randomIO @Int
          readProcessWithExitCode
            "nix-build"
            ["test/" <> name <> ".nix", "--no-out-link", "--argstr", "seed", show seed]
            ""
            <&> (\(_, a, b) -> (toText a, encodeUtf8 b))
        readFiles = (,) . decodeUtf8 <$> readFileBS ("test/" <> name <> ".stdout") <*> readFileBS ("test/" <> name <> ".stderr")
    (output, errors) <- if withNix then callNix else readFiles
    firstState <- initalStateFromBuildPlatform (Just "x86_64-linux")
    endState <- processTextStream (MkConfig False False) (parseStreamAttoparsec parser) (preserveStateSnd . updateStateNixOldStyleMessage) (second maintainState) Nothing finalizer (Nothing, firstState) (pure $ Right errors)
    asserts output (snd endState)

finalizer :: UpdateMonad m => StateT (a, NOMV1State) m ()
finalizer = do
  (a, oldState) <- get
  newState <- execStateT (runWriterT detectLocalFinishedBuilds) oldState
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
  let outputStorePaths = mapMaybe parseStorePath (Text.lines output)
  assertEqual "All output paths parsed" noOfBuilds (length outputStorePaths)
  let outputDerivations :: [DerivationId]
      outputDerivations =
        flip evalState endState $
          forMaybeM outputStorePaths \path -> do
            pathId <- getStorePathId path
            outPathToDerivation pathId
  assertEqual "Derivations for all outputs have been found" noOfBuilds (length outputDerivations)
  assertBool "All found derivations have successfully been built" (CSet.isSubsetOf (CSet.fromFoldable outputDerivations) (CMap.keysSet completedBuilds))
