{-# LANGUAGE DataKinds #-}
module NOM.Print (stateToText) where

import Relude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime)

import Data.Tree (Forest)
import NOM.Parser (Derivation (toStorePath), Host (Localhost), StorePath (name))
import NOM.Print.Table (Entry, blue, bold, cells, cyan, disp, dummy, green, header, label, magenta, markup, markups, prependLines, printAlignedSep, red, text, yellow)
import NOM.Print.Tree (showForest)
import NOM.State (BuildForest, BuildState (..), BuildStatus (Building, Built, Failed), DerivationNode (DerivationNode, derivation), StorePathNode (StorePathNode, path))
import NOM.State.Tree (replaceDuplicates, trimForest)
import NOM.Update (collapseMultimap, countPaths)
import NOM.Util ((.>), (<.>>))
import System.Console.Terminal.Size (Window)
import qualified Data.Tree as Tree
import qualified System.Console.Terminal.Size as Window

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

stateToText :: Maybe (Window Int) -> UTCTime -> BuildState -> Text
stateToText maybeWindow now buildState@BuildState{..}
  | not inputReceived = time <> showCond (diffUTCTime now startTime > 15) " nom hasn‘t detected any input. Have you redirected nix-build stderr into nom? (See the README for details.)"
  | not anythingGoingOn = time
  | otherwise =
    buildsDisplay
      <> table
      <> unlines errors
 where
  anythingGoingOn = totalBuilds + downloadsDone + numOutstandingDownloads + numFailedBuilds > 0
  buildsDisplay =
    showCond
      anythingGoingOn
      $ prependLines
        (upperleft <> horizontal)
        (vertical <> " ")
        (vertical <> " ")
        (printBuilds maybeWindow now buildForest)
        <> "\n"
  table =
    prependLines
      (leftT <> stimes (3 :: Int) horizontal <> " ")
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
  Maybe (Window Int) ->
  UTCTime ->
  BuildForest ->
  NonEmpty Text
printBuilds maybeWindow now forest = markup bold " Build Graph: " :| showForest textForest
 where
  maxRows = maybe maxBound Window.height maybeWindow
  withLinks = replaceDuplicates mkLink forest
  forestToPrint = if length (foldMap Tree.flatten withLinks) > maxRows `div` 2 then trimForest isLeft withLinks else withLinks
  textForest :: Forest Text
  textForest = fmap (either (either printDerivation printStorePath) printLink) <$> forestToPrint
  mkLink = bimap derivation path
  printLink :: Either Derivation StorePath -> Text
  printLink = (markup blue . (<> markup bold " ↴")) . name . either toStorePath id
  printDerivation :: DerivationNode -> Text
  printDerivation (DerivationNode derivation status) = case status of
    Nothing -> markup blue . (todo <>) . name . toStorePath $ derivation
    Just (host, buildStatus) -> case buildStatus of
      Building t l ->
        unwords $
          [ markup yellow running
          , hostMarkup host derivation
          , clock
          , timeDiff now t
          ]
            <> maybe [] (\x -> ["(" <> average <> timeDiffSeconds x <> ")"]) l
      Failed dur code _at ->
        unwords
          [ markup yellow warning
          , hostMarkup host derivation
          , markups [red, bold] (unwords ["failed with exit code", show code, "after", clock, timeDiffSeconds dur])
          ]
      Built dur _at ->
        unwords
          [ markup green done
          , hostMarkup host derivation
          , clock
          , timeDiffSeconds dur
          ]
  printStorePath :: StorePathNode -> Text
  printStorePath (StorePathNode path _ _) = name path

--printBuild = \case

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
