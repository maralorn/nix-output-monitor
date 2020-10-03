module Print where

import           Prelude                        ( )
import           Parser                         ( StorePath(..)
                                                , Host(..)
                                                , Derivation(..)
                                                )
import           Relude
import           Update
import           Data.Time
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set


printRatio :: Int -> Int -> Text
printRatio part total = "(" <> show part <> "/" <> show total <> ")"

stateToText :: BuildState -> Text
stateToText buildState@BuildState { outstandingBuilds, outstandingDownloads, plannedCopies, runningRemoteBuilds, runningLocalBuilds, completedLocalBuilds, completedDownloads, completedUploads, currentTime, startTime, completedRemoteBuilds, errors }
  | totalBuilds + plannedCopies == 0
  = ""
  | otherwise
  = "\n"
    <> (if runningBuilds > 0
         then printBuilds currentTime runningRemoteBuilds runningLocalBuilds
         else ""
       )
    <> (if numHosts > 0 then printHosts buildState else "")
    <> unlines errors
    <> "Total - Builds: "
    <> show (runningBuilds)
    <> " running, "
    <> show (numCompletedBuilds)
    <> " done, "
    <> show numOutstandingBuilds
    <> " missing; Downloads: "
    <> show downloadsDone
    <> " done, "
    <> show (length outstandingDownloads)
    <> " missing; running for "
    <> timeDiff currentTime startTime
 where
  numHosts =
    length (Map.keysSet runningRemoteBuilds)
      + length (Map.keysSet completedRemoteBuilds)
      + length (Map.keysSet completedUploads)
  runningBuilds = countPaths runningRemoteBuilds + length runningLocalBuilds
  numCompletedBuilds =
    countPaths completedRemoteBuilds + length completedLocalBuilds
  numOutstandingBuilds = length outstandingBuilds
  totalBuilds = numOutstandingBuilds + runningBuilds + numCompletedBuilds
  downloadsDone = countPaths completedDownloads

printHosts :: BuildState -> Text
printHosts BuildState { runningRemoteBuilds, runningLocalBuilds, completedLocalBuilds, completedDownloads, completedUploads, completedRemoteBuilds }
  = unlines
    (  "Local builds: "
    <> show (Set.size runningLocalBuilds)
    <> " running, "
    <> show (Set.size completedLocalBuilds)
    <> " done"
    :  remoteLabels
    )
 where
  remoteLabels =
    (\h ->
        let running = l h runningRemoteBuilds
            done    = l h completedRemoteBuilds
        in  "Remote host "
              <> toText h
              <> ": "
              <> c
                   (running + done)
                   (  "Builds: "
                   <> show running
                   <> " running, "
                   <> show done
                   <> " done; "
                   )
              <> cs "Uploads: "   (l h completedUploads)   "; "
              <> cs "Downloads: " (l h completedDownloads) "; "
      )
      <$> hosts
  hosts =
    toList
      $  Map.keysSet runningRemoteBuilds
      <> Map.keysSet completedRemoteBuilds
      <> Map.keysSet completedUploads
      <> Map.keysSet completedDownloads
  l host = Set.size . Map.findWithDefault mempty host
  c num = memptyIfTrue (num == 0)
  cs b num a = c num (b <> show num <> a)
printBuilds
  :: UTCTime
  -> Map Host (Set (Derivation, UTCTime))
  -> Set (Derivation, UTCTime)
  -> Text
printBuilds now remoteBuilds localBuilds =
  unlines
    .  ("Currently building:" :)
    .  fmap printBuild
    .  sortOn snd
    $  remoteLabels
    <> localLabels
 where
  remoteLabels = Map.foldMapWithKey
    (\host builds ->
      (\(x, t) -> ((name $ toStorePath x) <> " on " <> toText host, t))
        <$> toList builds
    )
    remoteBuilds
  localLabels = (\(x, t) -> (name $ toStorePath x, t)) <$> toList localBuilds
  printBuild (p, t) = "  " <> p <> " (running for " <> timeDiff now t <> ")"

timeDiff :: UTCTime -> UTCTime -> Text
timeDiff larger smaller = toText
  $ formatTime defaultTimeLocale "%02H:%02M:%02S" (diffUTCTime larger smaller)
