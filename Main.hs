module Main where

import Relude
import Prelude ()

import qualified Data.Set as Set
import Data.Text.IO (hPutStrLn)
import Data.Version (showVersion)
import IO (processStream)
import Parser (parser)
import Paths_nix_output_monitor (version)
import Print (stateToText)
import System.Environment (getArgs)
import Update (countPaths, failedLocalBuilds, failedRemoteBuilds, initalState, updateState)

main :: IO ()
main = do
  getArgs >>= \case
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
  finalState <- processStream parser firstState updateState stateToText
  if Set.size (failedLocalBuilds finalState) + countPaths (failedRemoteBuilds finalState) == 0
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

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
