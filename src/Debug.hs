module Debug where

import Relude
import Prelude ()

import Data.Set (union)
import Relude.Unsafe as Unsafe (fromJust)
import System.Directory (doesPathExist)

import Update (
  BuildState (completedLocalBuilds, completedRemoteBuilds, outstandingBuilds, runningLocalBuilds, runningRemoteBuilds),
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
  putTextLn "RunningLocalBuilds:"
  mapM_ (putTextLn <=< po . fst) (runningLocalBuilds endState)
  putTextLn "CompletedLocalBuilds:"
  mapM_ (putTextLn <=< po) (completedLocalBuilds endState)
  putTextLn "RunningRemoteBuilds:"
  mapM_
    (putTextLn <=< po . fst)
    (foldr union mempty $ runningRemoteBuilds endState)
  putTextLn "CompletedRemoteBuilds:"
  mapM_ (putTextLn <=< po) (foldr union mempty $ completedRemoteBuilds endState)
