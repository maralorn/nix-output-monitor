{-# LANGUAGE DataKinds #-}

module NOM.Print (stateToText) where

import Relude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Tree (Forest)
import qualified Data.Tree as Tree

-- optics
import Optics (has, preview, summing, united, (%), _2, _Just, _Left, _Nothing, _Right)

-- generic-optics
import Data.Generics.Product (typed)
import Data.Generics.Sum (_Ctor)

-- terminal-size
import System.Console.Terminal.Size (Window)
import qualified System.Console.Terminal.Size as Window

import NOM.Parser (Derivation (toStorePath), Host (Localhost), StorePath (name))
import NOM.Print.Table (Entry, blue, bold, cells, cyan, disp, dummy, green, grey, header, label, magenta, markup, markups, prependLines, printAlignedSep, red, text, yellow)
import NOM.Print.Tree (showForest)
import NOM.State (BuildState (..), BuildStatus (Building, Built, Failed), DerivationNode (DerivationNode), LinkTreeNode, StorePathNode (StorePathNode), Summary, SummaryForest)
import NOM.State.Tree (collapseForestN, mapTwigsAndLeafs)
import NOM.Util (collapseMultimap, countPaths, (.>), (<.>>), (<|>>), (|>))

lb, vertical, lowerleft, upperleft, horizontal, down, up, clock, running, done, bigsum, warning, todo, leftT, average :: Text
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
average = "∅"
bigsum = "∑"
lb = "▓"

showCond :: Monoid m => Bool -> m -> m
showCond = memptyIfFalse

stateToText :: Maybe (Window Int) -> UTCTime -> BuildState -> Text
stateToText maybeWindow now buildState@BuildState{..}
  | not inputReceived = time <> showCond (diffUTCTime now startTime > 15) (markup grey " nom hasn‘t detected any input. Have you redirected nix-build stderr into nom? (See the README for details.)")
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
        (printBuilds maybeWindow now cachedShowForest)
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
      <> (one . bold . header $ time)

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

targetRatio :: Int
targetRatio = 3

type ElisionTreeNode = (Maybe LinkTreeNode, Summary)

possibleElisions :: [LinkTreeNode -> Bool]
possibleElisions =
  [ has (_Right % _Right)
  , has (_Right % _Left)
  , has (_Left % _Left % typed % _Just % _2 % _Ctor @"Built")
  , has (_Left % _Left % typed % _Nothing)
  ]

shrinkForestBy :: Int -> SummaryForest -> Forest ElisionTreeNode
shrinkForestBy linesToElide = fmap (fmap (first Just)) .> go possibleElisions linesToElide
 where
  go :: [LinkTreeNode -> Bool] -> Int -> Forest ElisionTreeNode -> Forest ElisionTreeNode
  go [] _ f = f
  go (nextElision : moreElisions) n f
    | n <= 0 = f
    | nAfter <= 0 = forest''
    | otherwise = go moreElisions' nAfter forest''
   where
    (nAfter, forest'') = collapseForestN nextElision n f
    moreElisions' = moreElisions <|>> \e x -> e x || nextElision x

printBuilds ::
  Maybe (Window Int) ->
  UTCTime ->
  SummaryForest ->
  NonEmpty Text
printBuilds maybeWindow now =
  shrinkForestToScreenSize
    .> fmap (mapTwigsAndLeafs (printSummariesTree False) (printSummariesTree True))
    .> showForest
    .> (markup bold " Dependency Graph: " :|)
 where
  maxRows :: Int
  maxRows = maybe maxBound Window.height maybeWindow `div` targetRatio
  shrinkForestToScreenSize :: SummaryForest -> Forest ElisionTreeNode
  shrinkForestToScreenSize forest = shrinkForestBy (length (foldMap Tree.flatten forest) - maxRows) forest
  printSummariesTree :: Bool -> ElisionTreeNode -> Text
  printSummariesTree isLeaf = (uncurry . flip) \summaries ->
    let count = length summaries
     in maybe
          (markup grey (show count <> " more"))
          ( either
              (either printDerivation printStorePath .> (<> markup grey (showCond isLeaf printAndMore count)))
              (printLink (count - 1))
          )
          .> (<> " " <> showSummaries summaries)
  showSummaries :: Summary -> Text
  showSummaries summaries =
    bar red (has (_Just % _2 % _Ctor @"Failed"))
      <> bar green (has (_Just % _2 % _Ctor @"Built"))
      <> bar yellow (has (_Just % _2 % _Ctor @"Building"))
      <> bar blue (has _Nothing) -- Waiting
      <> memptyIfTrue
        (null storePathStates)
        ( "("
            <> down
            <> printNum (has (_Ctor @"Downloaded"))
            <> "/"
            <> printNum (has (summing (_Ctor @"DownloadPlanned") (_Ctor @"Downloaded" % united)))
            <> ")"
        )
   where
    buildStates = toList summaries |> mapMaybe (preview (_Left % typed))
    storePathStates = toList summaries |> mapMaybe (preview (_Right % typed)) .> (fmap (toList @NonEmpty) .> join)
    printNum p = storePathStates |> filter p .> length .> show
    bar color p =
      buildStates
        |> filter p
        .> length
        .> (`stimesMonoid` lb)
        .> markup color
  --bar color p = markup color $ case () of
  --_ | count <= 2 -> stimesMonoid count lb
  --_ | count > 2, count < 10 -> lb <> show count <> stimesMonoid (count - 2) lb
  --_ -> lb <> show count <> stimesMonoid (count - 3) lb
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

printLink :: Int -> Either Derivation StorePath -> Text
printLink num =
  either toStorePath id
    .> name
    .> (<> markup grey (printAndMore num <> " ↴"))

printAndMore :: Int -> Text
printAndMore num = showCond (num > 0) (" and " <> show num <> " more")

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
