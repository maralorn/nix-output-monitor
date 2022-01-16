module Main where

import Relude

import Data.Text.IO (hPutStrLn)
import Data.Version (showVersion)
import System.Environment (getArgs)

import Paths_nix_output_monitor (version)

import NOM.IO (interact)
import NOM.Parser (parser)
import NOM.Print (stateToText)
import NOM.Update (updateState)
import NOM.State (initalState, failedBuilds)
import NOM.Util ( countPaths, (<||>) )

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
  finalState <- interact parser updateState stateToText firstState
  if countPaths (failedBuilds finalState) == 0
    then exitSuccess
    else exitFailure

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
