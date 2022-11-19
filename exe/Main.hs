module Main (main) where

import Control.Exception qualified as Exception
import Control.Monad.Writer.Strict (WriterT (runWriterT))
import Data.ByteString qualified as ByteString
import Data.Hermes qualified as JSON
import Data.Text.IO (hPutStrLn)
import Data.Time (ZonedTime)
import Data.Version (showVersion)
import GHC.IO.Exception (ExitCode (ExitFailure))
import NOM.Error (NOMError)
import NOM.IO (Stream, StreamParser, interact, readTextChunks)
import NOM.IO.ParseStream.Attoparsec (parseStreamAttoparsec)
import NOM.IO.ParseStream.Simple (parseStreamSimple)
import NOM.NixMessage.JSON (NixJSONMessage)
import NOM.NixMessage.OldStyle (NixOldStyleMessage)
import NOM.Parser (parser)
import NOM.Parser.JSON.Hermes (parseJSON)
import NOM.Print (Config (..), stateToText)
import NOM.Print.Table (markup, red)
import NOM.State (NOMV1State (..), ProgressState (..), failedBuilds, fullSummary, initalStateFromBuildPlatform)
import NOM.State.CacheId.Map qualified as CMap
import NOM.Update (detectLocalFinishedBuilds, maintainState, updateStateNixJSONMessage, updateStateNixOldStyleMessage)
import NOM.Update.Monad (UpdateMonad)
import Optics (Lens', gfield, (%), (%~), (.~), (^.), _2)
import Optics qualified
import Paths_nix_output_monitor (version)
import Relude
import Streamly.Internal.Data.Time.Units (AbsTime)
import System.Console.ANSI qualified as Terminal
import System.Console.Terminal.Size (Window)
import System.Environment qualified as Environment
import System.IO.Error qualified as IOError
import System.Process.Typed qualified as Process

outputHandle ∷ Handle
outputHandle = stderr

defaultConfig ∷ Config
defaultConfig =
  MkConfig
    { piping = False
    , silent = False
    }

main ∷ IO Void
main = do
  args ← Environment.getArgs
  prog_name ← Environment.getProgName
  case (args, prog_name) of
    (["--version"], _) → do
      hPutStrLn stderr ("nix-output-monitor " <> fromString (showVersion version))
      exitWith =<< Process.runProcess (Process.proc "nix" ["--version"])
    (nix_args, "nom-build") → do
      exitWith =<< runMonitoredCommand defaultConfig (Process.proc "nix-build" ("-v" : "--log-format" : "internal-json" : nix_args))
    (nix_args, "nom-shell") → do
      exitOnFailure =<< runMonitoredCommand defaultConfig{silent = True} (Process.proc "nix-shell" (("-v" : "--log-format" : "internal-json" : nix_args) <> ["--run", "exit"]))
      exitWith =<< Process.runProcess (Process.proc "nix-shell" nix_args)
    ("build" : nix_args, _) → do
      exitWith =<< runMonitoredCommand defaultConfig (Process.proc "nix" ("build" : "-v" : "--log-format" : "internal-json" : nix_args))
    ("shell" : nix_args, _) → do
      exitOnFailure =<< runMonitoredCommand defaultConfig{silent = True} (Process.proc "nix" (("shell" : "-v" : "--log-format" : "internal-json" : nix_args) <> ["--command", "sh", "-c", "exit"]))
      exitWith =<< Process.runProcess (Process.proc "nix" ("shell" : nix_args))
    ("develop" : nix_args, _) → do
      exitOnFailure =<< runMonitoredCommand defaultConfig{silent = True} (Process.proc "nix" (("develop" : "-v" : "--log-format" : "internal-json" : nix_args) <> ["--command", "sh", "-c", "exit"]))
      exitWith =<< Process.runProcess (Process.proc "nix" ("develop" : nix_args))
    ([], _) → do
      finalState ← monitorHandle (Proxy @(Maybe NixOldStyleMessage, ByteString)) defaultConfig{piping = True} stdin
      if CMap.size finalState.fullSummary.failedBuilds + length finalState.nixErrors == 0
        then exitSuccess
        else exitFailure
    (["--json"], _) → do
      finalState ← monitorHandle (Proxy @(Either NOMError NixJSONMessage)) defaultConfig{piping = True} stdin
      if CMap.size finalState.fullSummary.failedBuilds + length finalState.nixErrors == 0
        then exitSuccess
        else exitFailure
    xs → do
      hPutStrLn stderr helpText
      -- It's not a mistake if the user requests the help text, otherwise tell
      -- them off with a non-zero exit code.
      if any (liftA2 (||) (== "-h") (== "--help")) xs then exitSuccess else exitFailure

exitOnFailure ∷ Process.ExitCode → IO ()
exitOnFailure = \case
  code@Process.ExitFailure{} → exitWith code
  _ → pass

printIOException ∷ IOError.IOError → IO ()
printIOException io_exception = do
  let error_msg = case (IOError.isDoesNotExistError io_exception, IOError.ioeGetFileName io_exception) of
        (True, Just cmd) → "Command '" <> toText cmd <> "' not available from $PATH."
        _ → show io_exception
  hPutStrLn stderr $ markup red ("nix-output-monitor: " <> error_msg)

runMonitoredCommand ∷ Config → Process.ProcessConfig () () () → IO Process.ExitCode
runMonitoredCommand config process_config = do
  let process_config_with_handles =
        Process.setStdout Process.createPipe $
          Process.setStderr
            Process.createPipe
            process_config
  Exception.handle ((ExitFailure 1 <$) . printIOException) $
    Process.withProcessWait process_config_with_handles \process → do
      void $ monitorHandle (Proxy @(Either NOMError NixJSONMessage)) config (Process.getStderr process)
      exitCode ← Process.waitExitCode process
      output ← ByteString.hGetContents (Process.getStdout process)
      unless (ByteString.null output) $ ByteString.hPut stdout output
      pure exitCode

data UpdateResult a = MkUpdateResult
  { errors ∷ [NOMError]
  , output ∷ ByteString
  , newStateToPrint ∷ Maybe NOMV1State
  , newState ∷ UpdaterState a
  }
  deriving stock (Generic)

data ProcessState a = MkProcessState
  { updaterState ∷ UpdaterState a
  , printFunction ∷ Maybe (Window Int) → (ZonedTime, AbsTime) → Text
  }
  deriving stock (Generic)

class NOMInput a where
  type UpdaterState a
  firstState ∷ NOMV1State → UpdaterState a
  updateState ∷ UpdateMonad m ⇒ a → UpdaterState a → m (UpdateResult a)
  nomState ∷ Lens' (UpdaterState a) NOMV1State
  inputStream ∷ Handle → Stream (Either NOMError ByteString)
  withParser ∷ (StreamParser a → IO t) → IO t

instance NOMInput (Either NOMError NixJSONMessage) where
  withParser body = JSON.withHermesEnv (body . parseStreamSimple . parseJSON)
  type UpdaterState (Either NOMError NixJSONMessage) = NOMV1State
  inputStream = readTextChunks
  nomState = Optics.equality'
  firstState = id
  {-# INLINE updateState #-}
  updateState input old_state = mkUpdateResult <$> updateStateNixJSONMessage input old_state
   where
    mkUpdateResult ((errors, output), new_state) =
      MkUpdateResult
        { errors
        , output
        , newStateToPrint = new_state
        , newState = fromMaybe old_state new_state
        }

instance NOMInput (Maybe NixOldStyleMessage, ByteString) where
  withParser body = body (parseStreamAttoparsec parser)
  type UpdaterState (Maybe NixOldStyleMessage, ByteString) = (Maybe AbsTime, NOMV1State)
  inputStream = readTextChunks
  nomState = _2
  firstState = (Nothing,)
  {-# INLINE updateState #-}
  updateState input old_state = mkUpdateResult <$> updateStateNixOldStyleMessage input old_state
   where
    mkUpdateResult ((errors, output), (new_timestamp, new_state)) =
      MkUpdateResult
        { errors
        , output
        , newStateToPrint = new_state
        , newState = (new_timestamp, fromMaybe (snd old_state) new_state)
        }

monitorHandle ∷ ∀ a. NOMInput a ⇒ Proxy a → Config → Handle → IO NOMV1State
monitorHandle _ config input_handle = withParser @a \streamParser → do
  finalState ←
    do
      Terminal.hHideCursor outputHandle
      hSetBuffering stdout (BlockBuffering (Just 1_000_000))

      current_system ← Exception.handle ((Nothing <$) . printIOException) $ Just . decodeUtf8 <$> Process.readProcessStdout_ (Process.proc "nix" ["eval", "--extra-experimental-features", "nix-command", "--impure", "--raw", "--expr", "builtins.currentSystem"])
      first_state ← initalStateFromBuildPlatform current_system
      let first_process_state = MkProcessState (firstState @a first_state) (stateToText config first_state)
      interact config streamParser (processStateUpdater @a config) (gfield @"updaterState" % nomState @a %~ maintainState) (.printFunction) (finalizer config) (inputStream @a input_handle) outputHandle first_process_state
      `Exception.finally` do
        Terminal.hShowCursor outputHandle
        ByteString.hPut outputHandle "\n" -- We print a new line after finish, because in normal nom state the last line is not empty.
  pure (finalState.updaterState ^. nomState @a)

{-# INLINE processStateUpdater #-}
processStateUpdater ∷
  ∀ a m.
  (NOMInput a, UpdateMonad m) ⇒
  Config →
  a →
  StateT (ProcessState a) m ([NOMError], ByteString)
processStateUpdater config input = do
  old_state ← get
  updater_result ← updateState input (old_state.updaterState)
  put
    MkProcessState
      { updaterState = updater_result.newState
      , printFunction = maybe old_state.printFunction (stateToText config) updater_result.newStateToPrint
      }
  pure (updater_result.errors, updater_result.output)

finalizer ∷
  ∀ a m.
  (NOMInput a, UpdateMonad m) ⇒
  Config →
  StateT (ProcessState a) m ()
finalizer config = do
  old_state ← get
  newState ← (gfield @"progressState" .~ Finished) <$> execStateT (runWriterT detectLocalFinishedBuilds) (old_state.updaterState ^. nomState @a)
  put
    MkProcessState
      { updaterState = nomState @a .~ newState $ old_state.updaterState
      , printFunction = stateToText config newState
      }

helpText ∷ Text
helpText =
  unlines
    [ "nix-output-monitor usages:"
    , "  Wrappers:"
    , "    nom build <nix-args>"
    , "    nom shell <nix-args>"
    , "    nom develop <nix-args>"
    , ""
    , "    nom-build <nix-args>"
    , "    nom-shell <nix-args>"
    , ""
    , "  Direct piping:"
    , "    via json parsing:"
    , "      nix build --log-format internal-json -v <nix-args> |& nom --json"
    , "      nix-build --log-format internal-json -v <nix-args> |& nom --json"
    , ""
    , "    via human-readable log parsing:"
    , "      nix-build |& nom"
    , ""
    , "    Don‘t forget to redirect stderr, too. That's what the & does."
    , ""
    , "Flags:"
    , "  --version  Show version."
    , "  -h, --help Show this help."
    , "  --json     Parse input as nix internal-json"
    , ""
    , "Please see the readme for more details:"
    , "https://git.maralorn.de/nix-output-monitor/about/"
    ]
