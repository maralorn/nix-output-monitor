module NOM.Print (stateToText) where

import Relude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Tree (Forest)
import qualified Data.Tree as Tree

-- optics
import Optics (has, preview, summing, (%), _2, _Just, _Left, _Nothing, _Right)

-- generic-optics
import Data.Generics.Product (typed)
import Data.Generics.Sum (_Ctor)

-- terminal-size
import System.Console.Terminal.Size (Window)
import qualified System.Console.Terminal.Size as Window

import Data.List (partition)
import qualified Data.Text as Text
import NOM.Parser (Derivation (toStorePath), Host (Localhost), StorePath (name))
import NOM.Print.Table (Entry, blue, bold, cells, cyan, disp, dummy, green, grey, header, label, magenta, markup, markups, prependLines, printAlignedSep, red, text, yellow)
import NOM.Print.Tree (showForest)
import NOM.State (BuildForest, BuildState (..), BuildStatus (..), DerivationNode (..), Link, LinkTreeNode, StorePathNode (..), StorePathState (..), Summary, SummaryForest)
import NOM.State.Tree (aggregateTree, collapseForestN, mapRootsTwigsAndLeafs, replaceDuplicates, sortForest)
import NOM.Update (SortOrder (SLink), mkOrder, nodeOrder)
import NOM.Util (collapseMultimap, countPaths, (.>), (<.>>), (<<.>>>), (<|>>), (|>))

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

stateToText :: BuildState -> Maybe (Window Int) -> UTCTime -> Text
stateToText buildState@BuildState{..} maybeWindow now
  | not inputReceived = time <> showCond (diffUTCTime now startTime > 15) (markup grey " nom hasn‘t detected any input. Have you redirected nix-build stderr into nom? (See the README for details.)")
  | not anythingGoingOn = time
  | otherwise = buildsDisplay <> table <> unlines errors
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
  , has (_Left % _Right)
  , has (_Left % _Left % typed % _Nothing)
  ]

shrinkForestBy :: Int -> SummaryForest -> Forest ElisionTreeNode
shrinkForestBy linesToElide = fmap (fmap (first Just)) .> go possibleElisions linesToElide
 where
  go :: [LinkTreeNode -> Bool] -> Int -> Forest ElisionTreeNode -> Forest ElisionTreeNode
  go [] _ forest = forest
  go (nextElision : moreElisions) n forest
    | n <= 0 = forest
    | nAfter <= 0 = forest''
    | otherwise = go moreElisions' nAfter forest''
   where
    (nAfter, forest'') = collapseForestN (\x -> x |> nextElision .> bool Nothing (Just (either one (const mempty) x))) n forest
    moreElisions' = moreElisions <|>> \e x -> e x || nextElision x

replaceLinksInForest :: Forest (Either DerivationNode StorePathNode, Summary) -> Forest (LinkTreeNode, Summary)
replaceLinksInForest = replaceDuplicates mkLink <<.>>> either (first Left) (first Right)

mkLink :: (Either DerivationNode StorePathNode, Summary) -> (Either Derivation StorePath, Summary)
mkLink (node, summary) = (bimap derivation path node, Set.insert node summary)

linkNodeOrder :: LinkTreeNode -> SortOrder
linkNodeOrder = either nodeOrder (const SLink)

printBuilds ::
  Maybe (Window Int) ->
  UTCTime ->
  BuildForest ->
  NonEmpty Text
printBuilds maybeWindow now =
  fmap (aggregateTree one)
    .> replaceLinksInForest
    .> sortForest (fmap fst .> mkOrder linkNodeOrder)
    .> shrinkForestToScreenSize
    .> fmap (mapRootsTwigsAndLeafs (printSummariesNode True) (printSummariesNode False) (printSummariesNode True))
    .> showForest
    .> (markup bold " Dependency Graph:" :|)
 where
  maxRows :: Int
  maxRows = maybe maxBound Window.height maybeWindow `div` targetRatio
  shrinkForestToScreenSize :: SummaryForest -> Forest ElisionTreeNode
  shrinkForestToScreenSize forest = shrinkForestBy (length (foldMap Tree.flatten forest) - maxRows) forest
  printSummariesNode :: Bool -> ElisionTreeNode -> Text
  printSummariesNode isLeaf = (uncurry . flip) \summary' ->
    let summary = showCond isLeaf (showSummary summary')
     in maybe
          summary
          ( either
              (either printDerivation printStorePath .> (<> showCond (isLeaf && not (Text.null summary)) (markup grey " & " <> summary)))
              printLink
          )

  showSummary :: Summary -> Text
  showSummary summaries =
    [ [totalBar | not (Text.null totalBar)]
    , memptyIfTrue
        (null downloads)
        [ markup
            cyan
            ( down
                <> show fullDownloads
                <> showCond (length downloads > fullDownloads) ("/" <> show (length downloads))
            )
        ]
    , memptyIfTrue
        (null uploads)
        [markup magenta (up <> show (length uploads))]
    ]
      |> join .> unwords
   where
    totalBar =
      bar red (has (_Just % _2 % _Ctor @"Failed"))
        <> bar green (has (_Just % _2 % _Ctor @"Built"))
        <> bar yellow (has (_Just % _2 % _Ctor @"Building"))
        <> bar blue (has _Nothing) -- Waiting
    buildStates = toList summaries |> mapMaybe (preview (_Left % typed))
    storePathStates = toList summaries |> mapMaybe (preview (_Right % typed)) .> (fmap (toList @NonEmpty) .> join)
    (uploads, downloads) = partition (has (summing (_Ctor @"Uploading") (_Ctor @"Uploaded"))) storePathStates
    fullDownloads = length (filter (has (_Ctor @"Downloaded")) downloads)
    countStates p =       buildStates        |> filter p        .> length
    bar color (countStates -> c)
      | c == 0 = ""
      | c <= 10 = stimesMonoid c lb |> markup color
      | otherwise = ("▓▓▓┄" <> show c <> "┄▓▓▓") |> markup color
  printDerivation :: DerivationNode -> Text
  printDerivation (DerivationNode (toStorePath .> name -> name) status) = case status of
    Nothing -> markup blue (todo <> " " <> name)
    Just (host, buildStatus) -> case buildStatus of
      Building t l ->
        unwords $
          [markups [yellow, bold] (running <> " " <> name)]
            <> hostMarkup host
            <> [clock, timeDiff now t]
            <> maybe [] (\x -> ["(" <> average <> timeDiffSeconds x <> ")"]) l
      Failed dur code _at ->
        unwords $
          [markups [red, bold] (warning <> " " <> name)]
            <> hostMarkup host
            <> [markups [red, bold] (unwords ["failed with exit code", show code, "after", clock, timeDiffSeconds dur])]
      Built dur _at ->
        unwords $
          [markup green (done <> " " <> name)]
            <> hostMarkup host
            <> [markup grey (clock <> " " <> timeDiffSeconds dur)]

printStorePath :: StorePathNode -> Text
printStorePath (StorePathNode path _ states) = foldMap (printStorePathState .> (<> " ")) states <> markup color (name path)
 where
  color = case last states of
    DownloadPlanned -> cyan
    (Downloading _) -> cyan
    (Downloaded _) -> cyan
    (Uploading _) -> magenta
    (Uploaded _) -> magenta

printStorePathState :: StorePathState -> Text
printStorePathState = \case
  DownloadPlanned -> markup cyan down <> markup blue todo
  (Downloading _) -> markup cyan down <> markup yellow running
  (Uploading _) -> markup magenta up <> markup yellow running
  (Downloaded _) -> markup cyan down <> markup green done
  (Uploaded _) -> markup magenta up <> markup green done

printLink :: Link -> Text
printLink link =
  link
    |> either toStorePath id
    .> name
    .> (<> " ↴")
    .> markup grey

hostMarkup :: Host -> [Text]
hostMarkup Localhost = mempty
hostMarkup host = ["on " <> markup magenta (toText host)]

timeDiff :: UTCTime -> UTCTime -> Text
timeDiff =
  diffUTCTime
    <.>> printDuration
    .> toText

printDuration :: NominalDiffTime -> Text
printDuration diff
  | diff < 60 = p "%Ss"
  | diff < 60 * 60 = p "%Mm%Ss"
  | otherwise = p "%Hh%Mm%Ss"
 where
  p x = diff |> formatTime defaultTimeLocale x .> toText

timeDiffSeconds :: Int -> Text
timeDiffSeconds = fromIntegral .> printDuration
