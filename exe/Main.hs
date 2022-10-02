module Main (main) where

import Relude

import Control.Exception qualified as Exception
import Data.ByteString qualified as ByteString
import Data.Generics.Product (typed)
import Data.Hermes qualified as JSON
import Data.Text.IO (hPutStrLn)
import Data.Time (UTCTime, ZonedTime)
import Data.Version (showVersion)
import GHC.IO.Exception (ExitCode (ExitFailure))
import Optics (view, (%~), (.~), _2, _3)
import Paths_nix_output_monitor (version)
import System.Console.ANSI qualified as Terminal
import System.Console.Terminal.Size (Window)
import System.Environment qualified as Environment
import System.IO.Error qualified as IOError
import System.Process.Typed qualified as Process

import NOM.Error (NOMError)
import NOM.IO (interact, StreamParser)
import NOM.Parser (NixEvent, parser)
import NOM.Print (Config (..), stateToText)
import NOM.Print.Table (markup, red)
import NOM.State (NOMV1State (nixErrors), ProcessState (..), failedBuilds, fullSummary, initalState)
import NOM.State.CacheId.Map qualified as CMap
import NOM.Update (detectLocalFinishedBuilds, maintainState, updateState)
import NOM.Update.Monad (UpdateMonad)
import NOM.Util (addPrintCache)
import NOM.IO.ParseStream.Attoparsec (parseStreamAttoparsec)
import NOM.IO.ParseStream.Simple (parseStreamSimple)
import NOM.Parser.JSON.Hermes (parseJSON)
import Streamly.Prelude qualified as Streamly

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
      finalState <- withOldStyleParser (monitorHandle defaultConfig{piping = True} stdin)
      if CMap.size finalState.fullSummary.failedBuilds + length finalState.nixErrors == 0
        then exitSuccess
        else exitFailure
    (["--json"], _) -> do
      finalState <- withJSONParser (monitorHandle defaultConfig{piping = False} stdin)
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

runMonitoredCommand :: Config -> Process.ProcessConfig () () () -> IO Process.ExitCode
runMonitoredCommand config process_config = do
  let process_config_with_handles =
        Process.setStdout Process.createPipe $
          Process.setStderr
            Process.createPipe
            process_config
      catch_io_exception :: IOError.IOError -> IO Process.ExitCode
      catch_io_exception io_exception = do
        let error_msg = case (IOError.isDoesNotExistError io_exception, IOError.ioeGetFileName io_exception) of
              (True, Just cmd) -> "Command '" <> toText cmd <> "' not available from $PATH."
              _ -> show io_exception
        hPutStrLn stderr $ markup red ("nix-output-monitor: " <> error_msg)
        pure (ExitFailure 1)
  Exception.handle catch_io_exception $
    Process.withProcessWait process_config_with_handles \process -> do
      void $ withJSONParser (monitorHandle config (Process.getStderr process))
      exitCode <- Process.waitExitCode process
      output <- ByteString.hGetContents (Process.getStdout process)
      unless (ByteString.null output) $ ByteString.hPut stdout output
      pure exitCode

withJSONParser :: ((Streamly.SerialT IO ByteString -> Streamly.SerialT IO (Maybe NixEvent, ByteString)) -> IO a) -> IO a
withJSONParser body = JSON.withHermesEnv \env -> body (parseStreamSimple (Just . parseJSON env))
withOldStyleParser :: Monad m => ((Streamly.SerialT m ByteString -> Streamly.SerialT m (Maybe NixEvent, ByteString)) -> t) -> t
withOldStyleParser body = body (parseStreamAttoparsec parser)

monitorHandle :: Config -> Handle -> StreamParser (Maybe NixEvent) -> IO NOMV1State
monitorHandle config input_handle streamParser = do
  (_, finalState, _) <-
    do
      Terminal.hHideCursor outputHandle
      hSetBuffering stdout (BlockBuffering (Just 1_000_000))

      firstState <- initalState
      let firstCompoundState = (Nothing, firstState, stateToText config firstState)
      interact config streamParser (compoundStateUpdater config) (_2 %~ maintainState) compoundStateToText (finalizer config) input_handle outputHandle firstCompoundState
      `Exception.finally` do
        Terminal.hShowCursor outputHandle
        ByteString.hPut outputHandle "\n" -- We print a new line after finish, because in normal nom state the last line is not empty.
  pure finalState

type CompoundState = (Maybe UTCTime, NOMV1State, Maybe (Window Int) -> ZonedTime -> Text)

compoundStateToText :: (a, b, c) -> c
compoundStateToText = view _3

compoundStateUpdater ::
  UpdateMonad m =>
  Config ->
  (Maybe NixEvent, ByteString) ->
  StateT CompoundState m ([NOMError], ByteString)
compoundStateUpdater config input = do
  oldState <- get
  (!errors, !newState) <- addPrintCache updateState (stateToText config) input oldState
  put newState
  pure errors

finalizer ::
  UpdateMonad m => Config -> StateT CompoundState m ()
finalizer config = do
  (n, !oldState, _) <- get
  newState <- (typed .~ Finished) <$> execStateT detectLocalFinishedBuilds oldState
  put (n, newState, stateToText config newState)

helpText :: Text
helpText =
  unlines
    [ "Usage: nix-build |& nom"
    , ""
    , "Run any nix command (nixos-rebuild,nix-build,home-manager switch,"
    , "not nix build.) and pipe stderr and stdout into nom."
    , ""
    , "Don‘t forget to redirect stderr, too. That's what the & does."
    , ""
    , "Please see the readme for more details:"
    , "https://github.com/maralorn/nix-output-monitor"
    ]
