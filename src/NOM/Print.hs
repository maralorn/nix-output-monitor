module NOM.Print (stateToText) where

import Relude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime)

import NOM.Parser (Derivation (toStorePath), Host (Localhost), StorePath (name))
import NOM.Print.Table (Entry, blue, bold, cells, cyan, disp, dummy, green, header, label, magenta, markup, markups, prependLines, printAlignedSep, red, text, yellow)
import NOM.Print.Tree (showForest)
import NOM.State (Build (MkBuild), BuildState (..), BuildStatus (Building, Failed))
import NOM.Update (collapseMultimap, countPaths)
import NOM.Util ((.>), (<.>>))
import NOM.State.Tree (Tree)

vertical, lowerleft, upperleft, horizontal, down, up, clock, running, done, bigsum, goal, warning, todo, leftT, average :: Text
vertical = "┃"
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

showCond :: Monoid m => Bool -> m -> m
showCond = memptyIfFalse

stateToText :: UTCTime -> BuildState -> Text
stateToText now buildState@BuildState{..}
  | not inputReceived = time <> showCond (diffUTCTime now startTime > 15) " nom hasn‘t detected any input. Have you redirected nix-build stderr into nom? (See the README for details.)"
  | totalBuilds + plannedCopies + numFailedBuilds == 0 = time
  | otherwise =
    buildsDisplay
      <> table
      <> unlines errors
 where
  buildsDisplay =
    showCond
      (numRunningBuilds + numFailedBuilds > 0)
      $ prependLines
        (upperleft <> horizontal)
        (vertical <> " ")
        (vertical <> " ")
        (printBuilds now buildForest)
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
      <> (one . bold . lastBuildColor . header $ lastBuildText <> time)
  lastBuildIcon drv
    | drv `Set.member` outstandingBuilds = (id, todo)
  lastBuildIcon drv
    | drv `Set.member` completedBuildsSet = (green, goal)
  lastBuildIcon drv
    | setAny ((== drv) . fst) runningBuildsSet = (id, running)
  lastBuildIcon _ = (red, warning)
  (lastBuildColor, lastBuildText) =
    lastPlannedBuild & maybe (id, "") \build ->
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
  time = clock <> " " <> timeDiff now startTime

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
  [Tree Derivation Build] -> 
  NonEmpty Text
printBuilds now forest = markup bold " Build Graph: " :| maybe [] (toList . showForest) (nonEmpty textForest)
 where
  textForest = bimap (name . toStorePath) printBuild <$> forest
  printBuild = \case
    MkBuild host drv (Building t l) ->
      unwords $
        [ markup yellow running
        , hostMarkup host drv
        , clock
        , timeDiff now t
        ]
          <> maybe [] (\x -> ["(" <> average <> timeDiffSeconds x <> ")"]) l
    MkBuild host drv (Failed dur code) ->
      unwords
        [ markup yellow warning
        , hostMarkup host drv
        , markups [red, bold] (unwords ["failed with exit code", show code, "after", clock, timeDiffSeconds dur])
        ]

hostMarkup :: Host -> Derivation -> Text
hostMarkup Localhost build = markups [cyan, bold] (name . toStorePath $ build)
hostMarkup host build = hostMarkup Localhost build <> " on " <> markup magenta (toText host)

timeFormat :: String
timeFormat = "%02H:%02M:%02S"

timeDiff :: UTCTime -> UTCTime -> Text
timeDiff =
  diffUTCTime
    <.>> formatTime defaultTimeLocale timeFormat
    .> toText

timeDiffSeconds :: Int -> Text
timeDiffSeconds =
  fromIntegral
    .> formatTime @NominalDiffTime defaultTimeLocale timeFormat
    .> toText
