module Main (main) where

import Control.Monad.Trans.Writer.CPS (runWriterT)
import Data.ByteString.Char8 qualified as ByteString
import Data.Text qualified as Text
import NOM.Builds (parseStorePath)
import NOM.Error (NOMError)
import NOM.IO (processTextStream)
import NOM.IO.Input (NOMInput (..), UpdateResult (..))
import NOM.IO.Input.JSON ()
import NOM.IO.Input.OldStyle (OldStyleInput)
import NOM.NixMessage.JSON (NixJSONMessage)
import NOM.Print (Config (..))
import NOM.State (
  DependencySummary (..),
  DerivationId,
  NOMState (..),
  getStorePathId,
  initalStateFromBuildPlatform,
  outPathToDerivation,
 )
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.Update (
  detectLocalFinishedBuilds,
  maintainState,
 )
import NOM.Update.Monad (UpdateMonad)
import NOM.Util (forMaybeM)
import Optics ((%~), (.~), (^.))
import Relude
import Streamly.Data.Stream qualified as Stream
-- import System.Environment qualified
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
import Text.Pretty.Simple (pShow)
import System.IO (openFile, hClose, hPutStrLn)
import Data.Text.IO qualified as Text.IO

-- tests :: [TestConfig -> Test]
-- tests = [goldenStandard, goldenFail]

label :: (Semigroup a, IsString a) => TestConfig -> a -> a
label config name = "golden test " <> name <> " for " <> (if config.oldStyle then "old-style messages" else "json messages") <> if config.withNix then " with nix" else " with log from file"

-- allBools :: [Bool]
-- allBools = [True, False]

main :: IO ()
main = do
  -- with_nix <- isNothing <$> System.Environment.lookupEnv "TESTS_FROM_FILE"
  counts <- runTestTT
    $ test
    $ do
      -- test' <- tests
      -- if with_nix
      --   then do
      --     test' <$> [MkTestConfig{..} | withNix <- allBools, oldStyle <- allBools]
      --   else test' <$> [MkTestConfig{withNix = with_nix, ..} | oldStyle <- allBools]
      [goldenStandard $ MkTestConfig{withNix = False, oldStyle = True}]
  if Test.HUnit.errors counts + failures counts == 0 then exitSuccess else exitFailure

data TestConfig = MkTestConfig {withNix :: Bool, oldStyle :: Bool}

testBuild :: String -> TestConfig -> (Text -> NOMState -> IO ()) -> Test
testBuild name config asserts =
  label config name ~: do
    let suffix = if config.oldStyle then "" else ".json"
        callNix = do
          seed <- randomIO @Int
          let command =
                if config.oldStyle
                  then
                    Process.proc
                      "nix-build"
                      ["test/golden/" <> name <> "/default.nix", "--no-out-link", "--argstr", "seed", show seed, "-v"]
                  else
                    Process.proc
                      "nix"
                      ["build", "-f", "test/golden/" <> name <> "/default.nix", "--no-link", "--argstr", "seed", show seed, "-v", "--log-format", "internal-json"]
          Process.readProcess command
            <&> (\(_, stdout', stderr') -> (decodeUtf8 stdout', toStrict stderr'))
        readFiles = (,) . decodeUtf8 <$> readFileBS ("test/golden/" <> name <> "/stdout" <> suffix) <*> readFileBS ("test/golden/" <> name <> "/stderr" <> suffix)
    (output, errors) <- if config.withNix then callNix else readFiles
    end_state <- if config.oldStyle then testProcess @OldStyleInput name (Stream.fromPure errors) else testProcess @NixJSONMessage name (Stream.fromList (ByteString.lines errors))

    handle <- openFile "stderr-from-nix.log" WriteMode
    ByteString.hPutStrLn handle errors
    hClose handle

    handle2 <- openFile "stdout-from-nix.log" WriteMode
    Text.IO.hPutStrLn handle2 output
    hClose handle2


    asserts output end_state

testProcess :: forall input. (Show (UpdaterState input), NOMInput input) => String -> Stream.Stream IO ByteString -> IO NOMState
testProcess name input = withParser @input \streamParser -> do
  first_state <- firstState @input <$> initalStateFromBuildPlatform (Just "x86_64-linux")

  handle <- openFile "processing.log" AppendMode
  hPutStrLn handle name
  end_state <- processTextStream @input @(UpdaterState input) (MkConfig False False) streamParser stateUpdater (\now -> nomState @input %~ maintainState now) 
      (Just (\st _ time  -> fromLazy (pShow (st, time)), handle))
      (finalizer @input) first_state (Right <$> input)
  hClose handle

  pure (end_state ^. nomState @input)

stateUpdater :: forall input m. (NOMInput input, UpdateMonad m) => input -> StateT (UpdaterState input) m ([NOMError], ByteString, Bool)
stateUpdater input = do
  old_state <- get
  new_state <- (.newState) <$> updateState @input input old_state
  put new_state
  pure (mempty, mempty, False)

finalizer :: forall input m. (NOMInput input, UpdateMonad m) => StateT (UpdaterState input) m ()
finalizer = do
  old_state <- get
  new_state <- execStateT (runWriterT detectLocalFinishedBuilds) (old_state ^. nomState @input)
  put (nomState @input .~ new_state $ old_state)

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

-- goldenFail :: TestConfig -> Test
-- goldenFail config = testBuild "fail" config \_ MkNOMState{fullSummary = d@MkDependencySummary{..}} -> do
--   assertEqual ("There should be one waiting build in " <> show d) 1 (CSet.size plannedBuilds)
--   assertEqual ("There should be one failed build in " <> show d) 1 (CMap.size failedBuilds)
--   assertEqual ("There should be no completed builds in " <> show d) 0 (CMap.size completedBuilds)
--   assertEqual ("There should be one unfinished build " <> show d) 1 (CMap.size runningBuilds)
