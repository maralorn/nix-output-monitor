module Main where

import           Prelude                        ( )
import           Relude

import           Parser
import           Print
import           Update
import           IO
import           Data.Time

main :: IO ()
main = do
  now      <- getCurrentTime
  processStream parser (initalState now) updateState stateToText
