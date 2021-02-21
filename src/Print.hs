module Print where

import Relude
import Prelude ()

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime)

import Parser (Derivation (toStorePath), Host, StorePath (name))
import Table
import Update

vertical, verticalSlim, lowerleft, upperleft, horizontal, down, up, clock, running, done, goal, warning, todo, leftT, cellBorder, tablePadding, average, emptyCell, skipCell :: Text
vertical = "┃"
verticalSlim = "│"
lowerleft = "┗"
upperleft = "┏"
leftT = "┣"
horizontal = "━"
down = "⬇"
up = "⬆"
clock = "⏲"
running = "▶"
done = "✔"
todo = "⏳"
warning = "⚠"
goal = "🏁"
average = "∅"
cellBorder = " " <> verticalSlim <> " "
tablePadding = vertical <> "    "
emptyCell = "     "
skipCell = emptyCell <> cellBorder

showCond :: Monoid m => Bool -> m -> m
showCond = memptyIfFalse

stateToText :: UTCTime -> BuildState -> Text
stateToText now buildState@BuildState{outstandingBuilds, outstandingDownloads, plannedCopies, runningRemoteBuilds, runningLocalBuilds, completedLocalBuilds, completedDownloads, completedUploads, startTime, completedRemoteBuilds, failedLocalBuilds, failedRemoteBuilds, lastPlannedBuild}
  | totalBuilds + plannedCopies + numFailedBuilds == 0 = ""
  | otherwise = builds <> failedBuilds <> table
 where
  builds =
    showCond
      (runningBuilds > 0)
      $ prependLines
        (upperleft <> horizontal)
        (vertical <> " ")
        (vertical <> " ")
        (printBuilds now runningRemoteBuilds runningLocalBuilds)
        <> "\n"
  failedBuilds =
    showCond
      (numFailedBuilds > 0)
      $ prependLines
        ((if runningBuilds > 0 then leftT else upperleft) <> horizontal)
        (vertical <> " ")
        (vertical <> " ")
        (printFailedBuilds failedRemoteBuilds failedLocalBuilds)
        <> "\n"
  table =
    prependLines
      ((if numFailedBuilds + runningBuilds > 0 then leftT else upperleft) <> stimes (3 :: Int) horizontal <> " ")
      (vertical <> "    ")
      (lowerleft <> horizontal <> " 𝚺 ")
      $ printAlignedSep innerTable
  innerTable = fromMaybe (one (text "")) (nonEmpty headers) :| tableRows
  headers =
    (cells 3 <$> optHeader showBuilds "Builds")
      <> (cells 2 <$> optHeader showDownloads "Downloads")
      <> optHeader showUploads "Uploads"
      <> optHeader showHosts "Host"
  optHeader cond = showCond cond . one . bold . header :: Text -> [Entry]
  tableRows =
    showCond
      showHosts
      (printHosts buildState showBuilds showDownloads showUploads)
      <> maybeToList (nonEmpty lastRow)
  lastRow =
    showCond
      showBuilds
      [ nonZeroBold runningBuilds (yellow (label running (disp runningBuilds)))
      , nonZeroBold numCompletedBuilds (green (label done (disp numCompletedBuilds)))
      , nonZeroBold numOutstandingBuilds (blue (label todo (disp numOutstandingBuilds)))
      ]
      <> showCond
        showDownloads
        [ nonZeroBold downloadsDone (green (label down (disp downloadsDone)))
        , nonZeroBold numOutstandingDownloads . blue . label todo . disp $ numOutstandingDownloads
        ]
      <> showCond showUploads [text ""]
      <> (one . bold . header $ maybe "" (\build -> "🏁" <> (name . toStorePath) build <> " ") lastPlannedBuild <> clock <> " " <> timeDiff now startTime)
  showHosts = numHosts > 0
  showBuilds = totalBuilds > 0
  showDownloads = downloadsDone + length outstandingDownloads > 0
  showUploads = countPaths completedUploads > 0
  numFailedBuilds = Set.size failedLocalBuilds + countPaths failedRemoteBuilds
  numOutstandingDownloads = Set.size outstandingDownloads
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

printHosts :: BuildState -> Bool -> Bool -> Bool -> [NonEmpty Entry]
printHosts BuildState{runningRemoteBuilds, runningLocalBuilds, completedLocalBuilds, completedDownloads, completedUploads, completedRemoteBuilds} showBuilds showDownloads showUploads =
  mapMaybe nonEmpty $
    ( showCond
        showBuilds
        [ nonZeroShowBold numRunningLocalBuilds (yellow (label running (disp numRunningLocalBuilds)))
        , nonZeroShowBold numCompletedLocalBuilds (green (label done (disp numCompletedLocalBuilds)))
        , dummy
        ]
        <> showCond showDownloads [dummy, dummy]
        <> showCond showUploads [dummy]
        <> one (header "local")
    ) :
    remoteLabels
 where
  numRunningLocalBuilds = Set.size runningLocalBuilds
  numCompletedLocalBuilds = Set.size completedLocalBuilds
  labelForHost h =
    showCond
      showBuilds
      [ nonZeroShowBold runningBuilds (yellow (label running (disp runningBuilds)))
      , nonZeroShowBold doneBuilds (green (label done (disp doneBuilds)))
      , dummy
      ]
      <> showCond
        showDownloads
        [nonZeroShowBold downloads (green (label down (disp downloads))), dummy]
      <> showCond
        showUploads
        [nonZeroShowBold uploads (green (label up (disp uploads)))]
      <> one (magenta (header (toText h)))
   where
    uploads = l h completedUploads
    downloads = l h completedDownloads
    runningBuilds = l h runningRemoteBuilds
    doneBuilds = l h completedRemoteBuilds
  remoteLabels = labelForHost <$> hosts
  hosts =
    sort
      . toList
      $ Map.keysSet runningRemoteBuilds
        <> Map.keysSet completedRemoteBuilds
        <> Map.keysSet completedUploads
        <> Map.keysSet completedDownloads
  l host = Set.size . Map.findWithDefault mempty host

nonZeroShowBold :: Int -> Entry -> Entry
nonZeroShowBold num = if num > 0 then bold else const dummy
nonZeroBold :: Int -> Entry -> Entry
nonZeroBold num = if num > 0 then bold else id

printBuilds ::
  UTCTime ->
  Map Host (Set (Derivation, (UTCTime, Maybe Int))) ->
  Set (Derivation, (UTCTime, Maybe Int)) ->
  NonEmpty Text
printBuilds now remoteBuilds localBuilds =
  printAligned . (one (cells 3 (header " Currently building:")) :|)
    . fmap printBuild
    . reverse
    . sortOn snd
    $ remoteLabels
      <> localLabels
 where
  remoteLabels =
    Map.foldMapWithKey
      (\host builds -> first (remoteLabel host) <$> toList builds)
      remoteBuilds
  localLabels = first localLabel <$> toList localBuilds
  printBuild (toList -> p, (t, l)) =
    yellow (text running)
      :| (p <> [header (clock <> " " <> timeDiff now t <> maybe "" (\x -> " (" <> average <> timeDiffSeconds x <> ")") l)])
printFailedBuilds ::
  Map Host (Set (Derivation, Int)) ->
  Set (Derivation, Int) ->
  NonEmpty Text
printFailedBuilds remoteBuilds localBuilds =
  printAligned . (one (cells 3 (red (header " Failed builds:"))) :|)
    . fmap printBuild
    $ remoteLabels
      <> localLabels
 where
  remoteLabels =
    Map.foldMapWithKey
      (\host builds -> first (remoteLabel host) <$> toList builds)
      remoteBuilds
  localLabels = first localLabel <$> toList localBuilds
  printBuild (toList -> p, diff) = red (text warning) :| p <> one (text ("after " <> clock <> " " <> timeDiffSeconds diff))

remoteLabel :: ToText a => a -> Derivation -> NonEmpty Entry
remoteLabel host build = (cyan . text . name . toStorePath $ build) :| [text "on", magenta . text . toText $ host]
localLabel :: Derivation -> NonEmpty Entry
localLabel = one . cyan . text . name . toStorePath

timeDiff :: UTCTime -> UTCTime -> Text
timeDiff larger smaller =
  toText $
    formatTime defaultTimeLocale "%02H:%02M:%02S" (diffUTCTime larger smaller)

timeDiffSeconds :: Int -> Text
timeDiffSeconds seconds =
  toText $ formatTime defaultTimeLocale "%02H:%02M:%02S" (fromIntegral seconds :: NominalDiffTime)
