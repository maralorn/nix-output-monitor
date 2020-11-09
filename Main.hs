{-# LANGUAGE LambdaCase #-}
module Main where

import           Prelude                        ( )
import           Relude

import           Parser
import           Print
import           Update
import           IO
import           Data.Time
import           Data.Text.IO                   ( hPutStrLn )
import           System.Environment             ( getArgs )
import           Paths_nix_output_monitor       ( version )
import           Data.Version                   ( showVersion )

main :: IO ()
main = do
  getArgs >>= \case
    []            -> pure ()
    ["--version"] -> do
      hPutStrLn stderr
                ("nix-output-monitor " <> fromString (showVersion version))
      exitSuccess
    xs -> do
      hPutStrLn stderr helpText
      -- It's not a mistake if the user requests the help text, otherwise tell
      -- them off with a non-zero exit code.
      if any ((== "-h") <||> (== "--help")) xs then exitSuccess else exitFailure
  now <- getCurrentTime
  processStream parser (initalState now) updateState stateToText

helpText :: Text
helpText = unlines
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
