module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (onException)
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Data.ByteString.Char8 qualified as ByteString
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import NOM.Builds (parseStorePath)
import NOM.Error (NOMError)
import NOM.IO (processTextStream)
import NOM.IO.Input (NOMInput (..), UpdateResult (..), inputStream)
import NOM.IO.Input.JSON ()
import NOM.IO.Input.OldStyle (OldStyleInput)
import NOM.NixMessage.JSON (NixJSONMessage)
import NOM.Print (Config (..))
import NOM.State (
  DependencySummary (..),
  DerivationId,
  NOMState (..),
  getStorePathId,
  outPathToDerivation,
 )
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.State.Initial (initialStateFromBuildPlatform)
import NOM.Update (checkFinishedBuilds, maintainState)
import NOM.Update.Monad (UpdateMonad)
import NOM.Util (forMaybeM)
import Relude
import Streamly.Data.Stream qualified as Stream
import System.Environment qualified
import System.Process.Typed qualified as Process
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

tests :: [TestConfig -> Test]
tests = [goldenStandard, goldenFail]

label :: (Semigroup a, IsString a) => TestConfig -> a -> a
label config name = "golden test " <> name <> " for " <> (if config.oldStyle then "old-style messages" else "json messages") <> if config.withNix then " with nix" else " with log from file"

allBools :: [Bool]
allBools = [True, False]

main :: IO ()
main = do
  with_nix <- isNothing <$> System.Environment.lookupEnv "TESTS_FROM_FILE"
  counts <- runTestTT
    $ test
    $ do
      test' <- tests
      if with_nix
        then do
          test' <$> [MkTestConfig{..} | withNix <- allBools, oldStyle <- allBools]
        else test' <$> [MkTestConfig{withNix = with_nix, ..} | oldStyle <- allBools]
  if Test.HUnit.errors counts + failures counts == 0 then exitSuccess else exitFailure

data TestConfig = MkTestConfig {withNix :: Bool, oldStyle :: Bool}

streamNixCommand ::
  TestConfig ->
  Process.ProcessConfig () () () ->
  (Stream.Stream IO ByteString -> IO r) ->
  IO (r, STM Text, Process.ExitCode)
streamNixCommand test_config process_config use_stream = do
  let process_config_with_handles =
        Process.setStdout Process.byteStringOutput
          . Process.setStderr Process.createPipe
          $ process_config
  Process.withProcessWait @IO process_config_with_handles \process -> do
    let out = decodeUtf8 <$> Process.getStdout process
    let err = Stream.catRights $ (if test_config.oldStyle then inputStream OldStyleInput else inputStream NixJSONMessage) (Process.getStderr process)

    result <- use_stream err
    exitCode <- Process.waitExitCode process
    pure (result, out, exitCode)

streamFromFiles ::
  String ->
  TestConfig ->
  (Stream.Stream IO ByteString -> IO r) ->
  IO (r, STM Text, Process.ExitCode)
streamFromFiles name config use_stream = do
  let suffix = if config.oldStyle then "" else ".json"
  out <- readFileBS ("test/golden/" <> name <> "/stdout" <> suffix)
  err <- readFileBS ("test/golden/" <> name <> "/stderr" <> suffix)
  result <- use_stream $ if config.oldStyle then Stream.fromPure err else Stream.fromList (ByteString.lines err)
  pure (result, pure (decodeUtf8 out), Process.ExitSuccess)

testBuild :: String -> TestConfig -> (Text -> NOMState -> IO ()) -> Test
testBuild name config asserts =
  label config name ~: do
    seed <- randomIO @Int
    let go = if config.withNix then streamNixCommand config command else streamFromFiles name config
         where
          command =
            if config.oldStyle
              then
                Process.proc
                  "nix-build"
                  ["test/golden/" <> name <> "/default.nix", "--no-out-link", "--argstr", "seed", show seed]
              else
                Process.proc
                  "nix-build"
                  ["test/golden/" <> name <> "/default.nix", "--no-out-link", "--argstr", "seed", show seed, "-v", "--log-format", "internal-json"]

    (end_state, output, _) <- go (if config.oldStyle then testProcess @OldStyleInput else testProcess @NixJSONMessage)
    output' <- atomically output

    onException (asserts output' end_state) $ do
      TextIO.putStrLn output'
      print end_state

testProcess :: forall input. (NOMInput input) => Stream.Stream IO ByteString -> IO NOMState
testProcess input = withParser @input \streamParser -> do
  first_state <- initialStateFromBuildPlatform (Just "x86_64-linux")
  processTextStream @input @NOMState (MkConfig False False) streamParser stateUpdater (\now -> maintainState now) Nothing finalizer first_state (Right <$> input)

stateUpdater :: forall input m. (NOMInput input, UpdateMonad m) => input -> StateT NOMState m ([NOMError], ByteString, Bool)
stateUpdater input = do
  old_state <- get
  new_state <- (.newState) <$> updateState @input input old_state
  put new_state
  pure (mempty, mempty, False)

finalizer :: (MonadIO m, UpdateMonad m) => StateT NOMState m ()
finalizer = do
  old_state <- get
  liftIO $ threadDelay 1_000_000 -- Wait for the store to settle before we finally check for completed builds
  new_state <- execStateT (runWriterT checkFinishedBuilds) old_state
  put new_state

goldenStandard :: TestConfig -> Test
goldenStandard config = testBuild "standard" config \nix_output endState@MkNOMState{fullSummary = MkDependencySummary{..}} -> do
  let noOfBuilds :: Int
      noOfBuilds = 4
  assertBool ("There should be no running builds but there is " <> show plannedBuilds) (CSet.null plannedBuilds)
  assertBool ("All builds should be finished but there is " <> show runningBuilds) (CMap.null runningBuilds)
  assertEqual
    ("The number of completed builds should be " <> show noOfBuilds <> " but is " <> show completedBuilds)
    noOfBuilds
    (CMap.size completedBuilds)
  when config.oldStyle $ do
    let outputStorePaths = mapMaybe parseStorePath (Text.lines nix_output)
    assertEqual "All output paths parsed" noOfBuilds (length outputStorePaths)
    let outputDerivations :: [DerivationId]
        outputDerivations = flip evalState endState $ forMaybeM outputStorePaths \path -> do
          pathId <- getStorePathId path
          outPathToDerivation pathId
    assertEqual "Derivations for all outputs have been found" noOfBuilds (length outputDerivations)
    assertBool "All found derivations have successfully been built" (CSet.isSubsetOf (CSet.fromFoldable outputDerivations) (CMap.keysSet completedBuilds))

goldenFail :: TestConfig -> Test
goldenFail config = testBuild "fail" config \_ MkNOMState{fullSummary = d@MkDependencySummary{..}} -> do
  assertEqual ("There should be one waiting build in " <> show d) 1 (CSet.size plannedBuilds)
  assertEqual ("There should be one failed build in " <> show d) 1 (CMap.size failedBuilds)
  assertEqual ("There should be no completed builds in " <> show d) 0 (CMap.size completedBuilds)
  assertEqual ("There should be one unfinished build " <> show d) 1 (CMap.size runningBuilds)
