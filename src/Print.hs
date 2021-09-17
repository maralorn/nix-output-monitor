module Print where

import Relude
import Prelude ()

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime)

import Parser (Derivation (toStorePath), Host (Localhost), StorePath (name))
import Table
import Update

vertical, verticalSlim, lowerleft, upperleft, horizontal, down, up, clock, running, done, bigsum, goal, warning, todo, leftT, cellBorder, tablePadding, average, emptyCell, skipCell :: Text
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
bigsum = "∑"
cellBorder = " " <> verticalSlim <> " "
tablePadding = vertical <> "    "
emptyCell = "     "
skipCell = emptyCell <> cellBorder

showCond :: Monoid m => Bool -> m -> m
showCond = memptyIfFalse

stateToText :: UTCTime -> BuildState -> Text
stateToText now buildState@BuildState{..}
  | not inputReceived = clock <> " " <> timeDiff now startTime <> showCond (diffUTCTime now startTime > 15) " nom hasn‘t detected any input. Have you redirected nix-build stderr into nom? (See the README for details.)"
  | totalBuilds + plannedCopies + numFailedBuilds == 0 = ""
  | otherwise = runningBuildsDisplay <> failedBuildsDisplay <> table
 where
  runningBuildsDisplay =
    showCond
      (numRunningBuilds > 0)
      $ prependLines
        (upperleft <> horizontal)
        (vertical <> " ")
        (vertical <> " ")
        (printBuilds now runningBuilds)
        <> "\n"
  failedBuildsDisplay =
    showCond
      (numFailedBuilds > 0)
      $ prependLines
        ((if numRunningBuilds > 0 then leftT else upperleft) <> horizontal)
        (vertical <> " ")
        (vertical <> " ")
        (printFailedBuilds failedBuilds)
        <> "\n"
  table =
    prependLines
      ((if numFailedBuilds + numRunningBuilds > 0 then leftT else upperleft) <> stimes (3 :: Int) horizontal <> " ")
      (vertical <> "    ")
      (lowerleft <> horizontal <> " " <> bigsum <> " ")
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
      [ nonZeroBold numRunningBuilds (yellow (label running (disp numRunningBuilds)))
      , nonZeroBold numCompletedBuilds (green (label done (disp numCompletedBuilds)))
      , nonZeroBold numOutstandingBuilds (blue (label todo (disp numOutstandingBuilds)))
      ]
      <> showCond
        showDownloads
        [ nonZeroBold downloadsDone (green (label down (disp downloadsDone)))
        , nonZeroBold numOutstandingDownloads . blue . label todo . disp $ numOutstandingDownloads
        ]
      <> showCond showUploads [text ""]
      <> (one . bold . lastBuildColor . header $ lastBuildText <> clock <> " " <> timeDiff now startTime)
  lastBuildIcon drv
    | drv `Set.member` outstandingBuilds = (id, todo)
  lastBuildIcon drv
    | drv `Set.member` completedBuildsSet = (green, goal)
  lastBuildIcon drv
    | setAny ((== drv) . fst) runningBuildsSet = (id, running)
  lastBuildIcon _ = (red, warning)
  (lastBuildColor, lastBuildText) = lastPlannedBuild & maybe (id, "") \build ->
     let (c, i) = lastBuildIcon build in (c, i <> " " <> (name . toStorePath) build <> " ")


  showHosts = numHosts > 0
  showBuilds = totalBuilds > 0
  showDownloads = downloadsDone + length outstandingDownloads > 0
  showUploads = countPaths completedUploads > 0
  numFailedBuilds = Set.size failedBuildsSet
  numOutstandingDownloads = Set.size outstandingDownloads
  numHosts =
     Set.size (Set.filter (/= Localhost) (Map.keysSet runningBuilds <> Map.keysSet completedBuilds <> Map.keysSet completedUploads))
  numRunningBuilds = Set.size runningBuildsSet
  failedBuildsSet = collapseMultimap failedBuilds
  completedBuildsSet = collapseMultimap completedBuilds
  runningBuildsSet = collapseMultimap runningBuilds
  numCompletedBuilds = Set.size completedBuildsSet
  numOutstandingBuilds = length outstandingBuilds
  totalBuilds = numOutstandingBuilds + numRunningBuilds + numCompletedBuilds
  downloadsDone = countPaths completedDownloads

setAny :: (a -> Bool) -> Set a -> Bool
setAny pred' = Set.foldl' (\y x -> pred' x || y) False

printHosts :: BuildState -> Bool -> Bool -> Bool -> [NonEmpty Entry]
printHosts BuildState{runningBuilds, completedBuilds, completedDownloads, completedUploads} showBuilds showDownloads showUploads =
  mapMaybe nonEmpty $ labelForHost <$> hosts
 where
  labelForHost h =
    showCond
      showBuilds
      [ nonZeroShowBold numRunningBuilds (yellow (label running (disp numRunningBuilds)))
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
    numRunningBuilds = l h runningBuilds
    doneBuilds = l h completedBuilds
  hosts =
    sort
      . toList
      $ Map.keysSet runningBuilds
        <> Map.keysSet completedBuilds
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
  NonEmpty Text
printBuilds now builds =
  printAligned . (one (cells 3 (bold (header " Currently building:"))) :|)
    . fmap printBuild
    . reverse
    . sortOn snd
    $
    Map.foldMapWithKey
      (\host hostBuilds -> first (hostLabel host) <$> toList hostBuilds)
      builds
 where
  printBuild (toList -> p, (t, l)) =
    yellow (text running)
      :| (p <> [header (clock <> " " <> timeDiff now t <> maybe "" (\x -> " (" <> average <> timeDiffSeconds x <> ")") l)])

printFailedBuilds ::
  Map Host (Set (Derivation, Int, Int)) ->
  NonEmpty Text
printFailedBuilds builds =
  printAligned . (one (cells 3 (bold (header " Failed builds:"))) :|)
    . fmap printBuild
    $ Map.foldMapWithKey
      (\host hostBuilds -> (\(a, b, c) -> (hostLabel host a, b, c)) <$> toList hostBuilds)
      builds
 where
  printBuild (toList -> p, diff, code) = yellow (text warning) :| p <> [red (text ("failed with exit code " <> show code)), text ("after " <> clock <> " " <> timeDiffSeconds diff)]

hostLabel :: Host -> Derivation -> NonEmpty Entry
hostLabel Localhost build = one . cyan . text . name . toStorePath $ build
hostLabel host build = (cyan . text . name . toStorePath $ build) :| [text "on", magenta . text . toText $ host]

timeDiff :: UTCTime -> UTCTime -> Text
timeDiff larger smaller =
  toText $
    formatTime defaultTimeLocale "%02H:%02M:%02S" (diffUTCTime larger smaller)

timeDiffSeconds :: Int -> Text
timeDiffSeconds seconds =
  toText $ formatTime defaultTimeLocale "%02H:%02M:%02S" (fromIntegral seconds :: NominalDiffTime)
