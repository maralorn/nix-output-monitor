module Main where

import Relude

import Data.Text.IO (hPutStrLn)
import Data.Version (showVersion)
import Optics (view, _2, _3, (.~))
import System.Environment (getArgs)
import System.Console.Terminal.Size (Window)
import Data.Time (UTCTime, ZonedTime)

import Paths_nix_output_monitor (version)

import NOM.IO (interact)
import NOM.Parser (parser)
import NOM.Print (stateToText)
import NOM.State (NOMV1State, initalState, getFailedBuilds, ProcessState (..))
import NOM.Update (detectLocalFinishedBuilds, updateState)
import NOM.Update.Monad (UpdateMonad)
import NOM.Util (addPrintCache, passThroughBuffer, (.>), (<|>>), (<||>), (|>))
import qualified Data.Map.Strict as Map
import Data.Generics.Product (typed)

main :: IO ()
main = do
  System.Environment.getArgs >>= \case
    [] -> pass
    ["--version"] -> do
      hPutStrLn stderr ("nix-output-monitor " <> fromString (showVersion version))
      exitSuccess
    xs -> do
      hPutStrLn stderr helpText
      -- It's not a mistake if the user requests the help text, otherwise tell
      -- them off with a non-zero exit code.
      if any ((== "-h") <||> (== "--help")) xs then exitSuccess else exitFailure
  firstState <- initalState
  finalState <- interact parser (passThroughBuffer (addPrintCache updateState stateToText)) (view _3) finalizer (Nothing, firstState, stateToText firstState)
  if (view _2 finalState |> getFailedBuilds .> Map.size) == 0
    then exitSuccess
    else exitFailure

finalizer :: UpdateMonad m => ((a, NOMV1State, Maybe (Window Int) -> ZonedTime -> Text) -> m (a, NOMV1State, Maybe (Window Int) -> ZonedTime -> Text))
finalizer (n, s, _) = detectLocalFinishedBuilds s <|>> fromMaybe s .> (typed .~ Finished) .> (\s' -> (n, s', stateToText s'))

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
