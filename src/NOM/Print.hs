{-# LANGUAGE DataKinds #-}

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
import NOM.State (BuildState (..), BuildStatus (..), DerivationNode (..), Link, LinkTreeNode, StorePathNode (..), StorePathState (..), Summary, SummaryForest)
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
  , has (_Left % _Right)
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
    .> (markup bold " Dependency Graph:" :|)
 where
  maxRows :: Int
  maxRows = maybe maxBound Window.height maybeWindow `div` targetRatio
  shrinkForestToScreenSize :: SummaryForest -> Forest ElisionTreeNode
  shrinkForestToScreenSize forest = shrinkForestBy (length (foldMap Tree.flatten forest) - maxRows) forest
  printSummariesTree :: Bool -> ElisionTreeNode -> Text
  printSummariesTree isLeaf = (uncurry . flip) \summary ->
    maybe
      (markup grey (printMore summary))
      ( either
          (either printDerivation printStorePath .> (<> markup grey (showCond isLeaf (printAndMore summary))))
          (printLink summary)
      )
      .> (<> " " <> showSummary summary)
  showSummary :: Summary -> Text
  showSummary summaries =
    [ [totalBar | not (Text.null totalBar)]
    , memptyIfTrue
        (null downloads)
        [ markup
            cyan
            ( "("
                <> down
                <> show fullDownloads
                <> showCond (length downloads > fullDownloads) ("/" <> show (length downloads))
                <> ")"
            )
        ]
    , memptyIfTrue
        (null uploads)
        [ markup
            magenta
            ( "("
                <> up
                <> show (length uploads)
                <> ")"
            )
        ]
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
    bar color p =
      buildStates
        |> filter p
        .> length
        .> (`stimesMonoid` lb)
        .> markup color
  printDerivation :: DerivationNode -> Text
  printDerivation (DerivationNode (toStorePath .> name -> name) status) = case status of
    Nothing -> markup blue (todo <> " " <> name)
    Just (host, buildStatus) -> case buildStatus of
      Building t l ->
        unwords $
          [ markups [yellow, bold] (running <> " " <> name)
          , hostMarkup host
          , clock
          , timeDiff now t
          ]
            <> maybe [] (\x -> ["(" <> average <> timeDiffSeconds x <> ")"]) l
      Failed dur code _at ->
        unwords
          [ markups [red, bold] (warning <> " " <> name)
          , hostMarkup host
          , markups [red, bold] (unwords ["failed with exit code", show code, "after", clock, timeDiffSeconds dur])
          ]
      Built dur _at ->
        unwords
          [ markup green (done <> " " <> name)
          , hostMarkup host
          , clock
          , timeDiffSeconds dur
          ]

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

printLink :: Summary -> Link -> Text
printLink summary link =
  link
    |> either toStorePath id
    .> name
    .> (<> markup grey (printAndMore s <> " ↴"))
 where
  s = summary |> toList .> filter ((link /=) . bimap derivation path) .> fromList

printAndMore :: Summary -> Text
printAndMore summary = showCond (not (null summary)) (" & " <> printMore summary)

printMore :: Summary -> Text
printMore summary = showCond (not (null summary)) (show (length summary) <> " more")

hostMarkup :: Host -> Text
hostMarkup Localhost = ""
hostMarkup host = "on " <> markup magenta (toText host)

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
