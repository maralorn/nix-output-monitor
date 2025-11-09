module Main (main) where

import Control.Concurrent (ThreadId, myThreadId, throwTo)
import Control.Exception qualified as Exception
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Data.ByteString qualified as ByteString
import Data.IORef qualified as IORef
import Data.Text.IO (hPutStrLn)
import Data.Time (ZonedTime)
import Data.Version (showVersion)
import GHC.IO.Exception (ExitCode (ExitFailure))
import NOM.Error (NOMError)
import NOM.IO (interact)
import NOM.IO.Input (NOMInput (..), UpdateResult (..), inputStream)
import NOM.IO.Input.JSON ()
import NOM.IO.Input.OldStyle (OldStyleInput)
import NOM.NixMessage.JSON (NixJSONMessage)
import NOM.Print (Config (..), stateToText)
import NOM.Print.Table (markup, red)
import NOM.State (DependencySummary (..), NOMState (..), ProgressState (..), initalStateFromBuildPlatform)
import NOM.State.CacheId.Map qualified as CMap
import NOM.Update (detectLocalFinishedBuilds, maintainState)
import NOM.Update.Monad (UpdateMonad)
import Optics ((%), (%~), (.~), (^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Paths_nix_output_monitor (version)
import Relude
import System.Console.ANSI qualified as Terminal
import System.Console.Terminal.Size (Window)
import System.Environment qualified as Environment
import System.IO.Error qualified as IOError
import System.Posix.Signals qualified as Signals
import System.Process.Typed (proc, runProcess)
import System.Process.Typed qualified as Process

type MainThreadId = ThreadId

data ProcessState a = MkProcessState
  { updaterState :: UpdaterState a
  , printFunction :: Maybe (Window Int) -> (ZonedTime, Double) -> Text
  }

makeFieldLabelsNoPrefix ''ProcessState

outputHandle :: Handle
outputHandle = stderr

defaultConfig :: Config
defaultConfig =
  MkConfig
    { piping = False
    , silent = False
    }

replaceCommandWithExit :: [String] -> [String]
replaceCommandWithExit = (<> ["--command", "sh", "-c", "exit"]) . takeWhile (\x -> x /= "--command" && x /= "-c")

knownSubCommands :: [String]
knownSubCommands = ["build", "shell", "develop"]

knownFlags :: [String]
knownFlags = ["--version", "-h", "--help", "--json"]

withJSON :: [String] -> [String]
withJSON x = "-v" : "--log-format" : "internal-json" : x

main :: IO Void
main = do
  installSignalHandlers

  prog_name <- Environment.getProgName
  args <- Environment.getArgs

  lookupEnv "NIX_GET_COMPLETIONS" >>= \case
    Just _ -> printNixCompletion prog_name args
    Nothing -> runApp prog_name args

runApp :: String -> [String] -> IO Void
runApp = \cases
  _ ["--version"] -> do
    hPutStrLn stderr ("nix-output-monitor " <> fromString (showVersion version))
    exitWith =<< runProcess (proc "nix" ["--version"])
  "nom-build" args -> exitWith =<< runMonitoredCommand defaultConfig (proc "nix-build" (withJSON args))
  "nom-shell" args -> do
    exitOnFailure =<< runMonitoredCommand defaultConfig{silent = True} (proc "nix-shell" (withJSON args <> ["--run", "exit"]))
    exitWith =<< runProcess (proc "nix-shell" args)
  "nom" ("build" : args) -> exitWith =<< runMonitoredCommand defaultConfig (proc "nix" ("build" : withJSON args))
  "nom" ("shell" : args) -> do
    exitOnFailure =<< runMonitoredCommand defaultConfig{silent = True} (proc "nix" ("shell" : withJSON (replaceCommandWithExit args)))
    exitWith =<< runProcess (proc "nix" ("shell" : args))
  "nom" ("develop" : args) -> do
    exitOnFailure =<< runMonitoredCommand defaultConfig{silent = True} (proc "nix" ("develop" : withJSON (replaceCommandWithExit args)))
    exitWith =<< runProcess (proc "nix" ("develop" : args))
  "nom" [] -> do
    finalState <- monitorHandle OldStyleInput defaultConfig{piping = True} stdin
    if CMap.size finalState.fullSummary.failedBuilds + length finalState.nixErrors == 0
      then exitSuccess
      else exitFailure
  "nom" ["--json"] -> do
    finalState <- monitorHandle NixJSONMessage defaultConfig{piping = True} stdin
    if CMap.size finalState.fullSummary.failedBuilds + length finalState.nixErrors == 0
      then exitSuccess
      else exitFailure
  _ xs -> do
    hPutStrLn stderr helpText
    -- It's not a mistake if the user requests the help text, otherwise tell
    -- them off with a non-zero exit code.
    if any (liftA2 (||) (== "-h") (== "--help")) xs then exitSuccess else exitFailure

printNixCompletion :: String -> [String] -> IO Void
printNixCompletion = \cases
  "nom" [input] -> do
    putStrLn "normal"
    mapM_ putStrLn $ findMatches input (knownSubCommands <> knownFlags)
    exitSuccess
  "nom" args@(sub_cmd : _)
    | sub_cmd `elem` knownSubCommands ->
        exitWith =<< Process.runProcess (Process.proc "nix" args)
  prog args -> do
    putTextLn $ "No completion support for " <> unwords (toText <$> prog : args)
    exitFailure

findMatches :: String -> [String] -> [String]
findMatches input = filter (input `isPrefixOf`)

installSignalHandlers :: IO ()
installSignalHandlers = do
  mainThreadId <- myThreadId >>= IORef.newIORef
  _ <- Signals.installHandler Signals.sigTERM (Signals.CatchInfo $ quitSignalHandler mainThreadId) Nothing
  _ <- Signals.installHandler Signals.sigINT (Signals.CatchInfo $ quitSignalHandler mainThreadId) Nothing
  pass

quitSignalHandler :: IORef MainThreadId -> Signals.SignalInfo -> IO ()
quitSignalHandler iomtid _ = do
  mtid <- IORef.readIORef iomtid
  Terminal.hShowCursor outputHandle
  -- The RTS runtime kills for us the sub-threads when the main thread
  -- is terminated.
  throwTo mtid $ ExitFailure 1

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
        Process.setStdout Process.createPipe
          . Process.setStderr Process.createPipe
          $ process_config
  Exception.handle ((ExitFailure 1 <$) . printIOException)
    $ Process.withProcessWait process_config_with_handles \process -> do
      void $ monitorHandle NixJSONMessage config (Process.getStderr process)
      exitCode <- Process.waitExitCode process
      output <- ByteString.hGetContents (Process.getStdout process)
      unless (ByteString.null output) $ ByteString.hPut stdout output
      pure exitCode

monitorHandle :: forall a -> (NOMInput a) => Config -> Handle -> IO NOMState
monitorHandle update config input_handle = withParser \streamParser -> do
  finalState <-
    do
      Terminal.hHideCursor outputHandle
      hSetBuffering stdout (BlockBuffering (Just 1_000_000))

      current_system <- Exception.handle ((Nothing <$) . printIOException) $ Just . decodeUtf8 <$> Process.readProcessStdout_ (Process.proc "nix" ["eval", "--extra-experimental-features", "nix-command", "--impure", "--raw", "--expr", "builtins.currentSystem"])
      first_state <- initalStateFromBuildPlatform current_system
      let first_process_state = MkProcessState (firstState @update first_state) (stateToText config first_state)
      interact @update config streamParser (processStateUpdater config) (\now -> #updaterState % nomState @update %~ maintainState now) (.printFunction) (finalizer config) (inputStream update input_handle) outputHandle first_process_state
      `Exception.finally` do
        Terminal.hShowCursor outputHandle
        ByteString.hPut outputHandle "\n" -- We print a new line after finish, because in normal nom state the last line is not empty.
  pure (finalState.updaterState ^. nomState @update)

{-# INLINE processStateUpdater #-}
processStateUpdater ::
  forall a m.
  (NOMInput a, UpdateMonad m) =>
  Config ->
  a ->
  StateT (ProcessState a) m ([NOMError], ByteString, Bool)
processStateUpdater config input = do
  old_state <- get
  updater_result <- updateState input old_state.updaterState
  put
    MkProcessState
      { updaterState = updater_result.newState
      , printFunction = maybe old_state.printFunction (stateToText config) updater_result.newStateToPrint
      }
  pure
    ( updater_result.errors
    , updater_result.output
    , not (null updater_result.errors)
        || not (ByteString.null updater_result.output)
        || isJust updater_result.newStateToPrint
    )

finalizer ::
  forall a m.
  (NOMInput a, UpdateMonad m) =>
  Config ->
  StateT (ProcessState a) m ()
finalizer config = do
  old_state <- get
  newState <- (#progressState .~ Finished) <$> execStateT (runWriterT detectLocalFinishedBuilds) (old_state.updaterState ^. nomState @a)
  put
    MkProcessState
      { updaterState = nomState @a .~ newState $ old_state.updaterState
      , printFunction = stateToText config newState
      }

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
    , "    Don't forget to redirect stderr, too. That's what the & does."
    , ""
    , "Flags:"
    , "  --version  Show version."
    , "  -h, --help Show this help."
    , "  --json     Parse input as nix internal-json"
    , ""
    , "Please see the readme for more details:"
    , "https://code.maralorn.de/maralorn/nix-output-monitor#readme"
    ]
