{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Relude

import Control.Exception qualified as Exception
import Control.Monad.Writer.Strict (WriterT (runWriterT))
import Data.ByteString qualified as ByteString
import Data.Hermes qualified as JSON
import Data.Text.IO (hPutStrLn)
import Data.Time (ZonedTime)
import Data.Version (showVersion)
import GHC.IO.Exception (ExitCode (ExitFailure))
import Optics (gfield, view, (%~), (.~), _2, _3)
import Paths_nix_output_monitor (version)
import System.Console.ANSI qualified as Terminal
import System.Console.Terminal.Size (Window)
import System.Environment qualified as Environment
import System.IO.Error qualified as IOError
import System.Process.Typed qualified as Process

import NOM.Error (NOMError)
import NOM.IO (StreamParser, interact)
import NOM.IO.ParseStream.Attoparsec (parseStreamAttoparsec)
import NOM.IO.ParseStream.Simple (parseStreamSimple)
import NOM.NixMessage.JSON (NixJSONMessage)
import NOM.NixMessage.OldStyle (NixOldStyleMessage)
import NOM.Parser (parser)
import NOM.Parser.JSON.Hermes (parseJSON)
import NOM.Print (Config (..), stateToText)
import NOM.Print.Table (markup, red)
import NOM.State (NOMV1State (..), ProcessState (..), failedBuilds, fullSummary, initalStateFromBuildPlatform)
import NOM.State.CacheId.Map qualified as CMap
import NOM.Update (detectLocalFinishedBuilds, maintainState, updateStateNixJSONMessage, updateStateNixOldStyleMessage)
import NOM.Update.Monad (UpdateMonad)
import NOM.Util (addPrintCache)
import Streamly.Internal.Data.Time.Units (AbsTime)

outputHandle :: Handle
outputHandle = stderr

defaultConfig :: Config
defaultConfig =
  MkConfig
    { piping = False
    , silent = False
    }

main :: IO Void
main = do
  args <- Environment.getArgs
  prog_name <- Environment.getProgName
  case (args, prog_name) of
    (["--version"], _) -> do
      hPutStrLn stderr ("nix-output-monitor " <> fromString (showVersion version))
      exitWith =<< Process.runProcess (Process.proc "nix" ["--version"])
    (nix_args, "nom-build") -> do
      exitWith =<< runMonitoredCommand defaultConfig (Process.proc "nix-build" ("-v" : "--log-format" : "internal-json" : nix_args))
    (nix_args, "nom-shell") -> do
      exitOnFailure =<< runMonitoredCommand defaultConfig{silent = True} (Process.proc "nix-shell" (("-v" : "--log-format" : "internal-json" : nix_args) <> ["--run", "exit"]))
      exitWith =<< Process.runProcess (Process.proc "nix-shell" nix_args)
    ("build" : nix_args, _) -> do
      exitWith =<< runMonitoredCommand defaultConfig (Process.proc "nix" ("build" : "-v" : "--log-format" : "internal-json" : nix_args))
    ("shell" : nix_args, _) -> do
      exitOnFailure =<< runMonitoredCommand defaultConfig{silent = True} (Process.proc "nix" (("shell" : "-v" : "--log-format" : "internal-json" : nix_args) <> ["--command", "sh", "-c", "exit"]))
      exitWith =<< Process.runProcess (Process.proc "nix" ("shell" : nix_args))
    ("develop" : nix_args, _) -> do
      exitOnFailure =<< runMonitoredCommand defaultConfig{silent = True} (Process.proc "nix" (("develop" : "-v" : "--log-format" : "internal-json" : nix_args) <> ["--command", "sh", "-c", "exit"]))
      exitWith =<< Process.runProcess (Process.proc "nix" ("develop" : nix_args))
    ([], _) -> do
      finalState <- monitorHandle (Proxy @(Maybe NixOldStyleMessage, ByteString)) defaultConfig{piping = True} stdin
      if CMap.size finalState.fullSummary.failedBuilds + length finalState.nixErrors == 0
        then exitSuccess
        else exitFailure
    (["--json"], _) -> do
      finalState <- monitorHandle (Proxy @(Either NOMError NixJSONMessage)) defaultConfig{piping = True} stdin
      if CMap.size finalState.fullSummary.failedBuilds + length finalState.nixErrors == 0
        then exitSuccess
        else exitFailure
    xs -> do
      hPutStrLn stderr helpText
      -- It's not a mistake if the user requests the help text, otherwise tell
      -- them off with a non-zero exit code.
      if any (liftA2 (||) (== "-h") (== "--help")) xs then exitSuccess else exitFailure

exitOnFailure :: Process.ExitCode -> IO ()
exitOnFailure = \case
  code@Process.ExitFailure{} -> exitWith code
  _ -> pass

printIOException :: IOError.IOError -> IO ()
printIOException io_exception = do
  let error_msg = case (IOError.isDoesNotExistError io_exception, IOError.ioeGetFileName io_exception) of
        (True, Just cmd) -> "Command '" <> toText cmd <> "' not available from $PATH."
        _ -> show io_exception
  hPutStrLn stderr $ markup red ("nix-output-monitor: " <> error_msg)

runMonitoredCommand :: Config -> Process.ProcessConfig () () () -> IO Process.ExitCode
runMonitoredCommand config process_config = do
  let process_config_with_handles =
        Process.setStdout Process.createPipe $
          Process.setStderr
            Process.createPipe
            process_config
  Exception.handle ((ExitFailure 1 <$) . printIOException) $
    Process.withProcessWait process_config_with_handles \process -> do
      void $ monitorHandle (Proxy @(Either NOMError NixJSONMessage)) config (Process.getStderr process)
      exitCode <- Process.waitExitCode process
      output <- ByteString.hGetContents (Process.getStdout process)
      unless (ByteString.null output) $ ByteString.hPut stdout output
      pure exitCode

class NOMInput a where
  type AdditionalState a
  firstAdditionalState :: AdditionalState a
  updateState :: UpdateMonad m => a -> (AdditionalState a, NOMV1State) -> m (([NOMError], ByteString), (AdditionalState a, Maybe NOMV1State))
  withParser :: (StreamParser a -> IO t) -> IO t

instance NOMInput (Either NOMError NixJSONMessage) where
  withParser body = JSON.withHermesEnv (body . parseStreamSimple . parseJSON)
  type AdditionalState (Either NOMError NixJSONMessage) = ()
  firstAdditionalState = ()
  updateState input = fmap (second ((),)) <$> updateStateNixJSONMessage input . snd

instance NOMInput (Maybe NixOldStyleMessage, ByteString) where
  withParser body = body (parseStreamAttoparsec parser)
  type AdditionalState (Maybe NixOldStyleMessage, ByteString) = Maybe AbsTime
  firstAdditionalState = Nothing
  updateState = updateStateNixOldStyleMessage

monitorHandle :: forall a. NOMInput a => Proxy a -> Config -> Handle -> IO NOMV1State
monitorHandle _ config input_handle = withParser @a \streamParser -> do
  (_, finalState, _) <-
    do
      Terminal.hHideCursor outputHandle
      hSetBuffering stdout (BlockBuffering (Just 1_000_000))

      current_system <- Exception.handle ((Nothing <$) . printIOException) $ Just . decodeUtf8 <$> Process.readProcessStdout_ (Process.proc "nix" ["eval", "--impure", "--raw", "--expr", "builtins.currentSystem"])
      firstState <- initalStateFromBuildPlatform current_system
      let firstCompoundState = (firstAdditionalState @a, firstState, stateToText config firstState)
      interact config streamParser (compoundStateUpdater @a config) (_2 %~ maintainState) compoundStateToText (finalizer config) input_handle outputHandle firstCompoundState
      `Exception.finally` do
        Terminal.hShowCursor outputHandle
        ByteString.hPut outputHandle "\n" -- We print a new line after finish, because in normal nom state the last line is not empty.
  pure finalState

type CompoundState istate = (istate, NOMV1State, Maybe (Window Int) -> (ZonedTime, AbsTime) -> Text)

compoundStateToText :: (a, b, c) -> c
compoundStateToText = view _3

{-# INLINE compoundStateUpdater #-}
compoundStateUpdater ::
  forall a m.
  (NOMInput a, UpdateMonad m) =>
  Config ->
  a ->
  StateT (CompoundState (AdditionalState a)) m ([NOMError], ByteString)
compoundStateUpdater config input = do
  oldState <- get
  (!errors, !newState) <- addPrintCache updateState (stateToText config) input oldState
  put newState
  pure errors

finalizer ::
  UpdateMonad m => Config -> StateT (CompoundState a) m ()
finalizer config = do
  (n, !oldState, _) <- get
  newState <- (gfield @"processState" .~ Finished) <$> execStateT (runWriterT detectLocalFinishedBuilds) oldState
  put (n, newState, stateToText config newState)

helpText :: Text
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
