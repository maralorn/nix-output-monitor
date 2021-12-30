module NOM.Debug where

import Relude

import Data.Set (union)
import Relude.Unsafe as Unsafe (fromJust)
import System.Directory (doesPathExist)

import NOM.Update (
  BuildState (completedBuilds, outstandingBuilds, runningBuilds),
  drv2out,
 )

printState :: BuildState -> IO ()
printState endState = do
  let po x = do
        let out = toText . Unsafe.fromJust $ drv2out endState x
        exists <- doesPathExist $ toString out
        pure $
          "  "
            <> toText x
            <> " -> "
            <> (toText . Unsafe.fromJust $ drv2out endState x)
            <> if exists then " (exists)" else " (DOES NOT EXIST)"
  putTextLn "OutstandingBuilds:"
  mapM_ (putTextLn <=< po) (outstandingBuilds endState)
  putTextLn "RunningBuilds:"
  mapM_
    (putTextLn <=< po . fst)
    (foldl' union mempty $ runningBuilds endState)
  putTextLn "CompletedBuilds:"
  mapM_ (putTextLn <=< po) (foldl' union mempty $ completedBuilds endState)
