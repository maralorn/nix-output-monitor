module NOM.Print (stateToText) where

import Relude

import Data.List.NonEmpty.Extra (appendr)
import Data.MemoTrie (memo)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, ZonedTime, defaultTimeLocale, diffUTCTime, formatTime, zonedTimeToUTC)
import Data.Tree (Forest, Tree (Node))
import Data.IntMap qualified as IntMap
import Optics (itoList, view, _2)

-- terminal-size
import System.Console.Terminal.Size (Window)
import System.Console.Terminal.Size qualified as Window

import NOM.Builds (Derivation (..), FailType (..), Host (..), StorePath (..))
import NOM.Print.Table (Entry, blue, bold, cells, disp, dummy, green, grey, header, label, magenta, markup, markups, prependLines, printAlignedSep, red, text, yellow)
import NOM.Print.Tree (showForest)
import NOM.State (BuildInfo (..), BuildStatus (..), DependencySummary (..), DerivationId, DerivationInfo (..), DerivationSet, NOMState, NOMV1State (..), ProcessState (..), getDerivationInfos, getStorePathInfos, StorePathInfo (..))
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.State.Sorting (SortKey, sortKey, summaryIncludingRoot)
import NOM.State.Tree (mapRootsTwigsAndLeafs)
import NOM.Parser.JSON (ActivityId(..))
import NOM.Util ((.>), (<.>>), (<|>>), (|>))
import Data.Map.Strict qualified as Map

textRep, vertical, lowerleft, upperleft, horizontal, down, up, clock, running, done, bigsum, warning, todo, leftT, average :: Text
textRep = fromString [toEnum 0xFE0E]
vertical = "┃"
lowerleft = "┗"
upperleft = "┏"
leftT = "┣"
horizontal = "━"
down = "⬇" <> textRep
up = "⬆" <> textRep
clock = "⏱" <> textRep
running = "▶" <> textRep
done = "✔" <> textRep
todo = "⏳︎︎" <> textRep
warning = "⚠" <> textRep
average = "∅"
bigsum = "∑"

showCond :: Monoid m => Bool -> m -> m
showCond = memptyIfFalse

targetRatio, defaultTreeMax :: Int
targetRatio = 3
defaultTreeMax = 20

stateToText :: NOMV1State -> Maybe (Window Int) -> ZonedTime -> Text
stateToText buildState@MkNOMV1State{..} = fmap Window.height .> memo printWithSize
 where
  printWithSize :: Maybe Int -> ZonedTime -> Text
  printWithSize maybeWindow = printWithTime
   where
    printWithTime :: ZonedTime -> Text
    printWithTime
      | processState == JustStarted = \now -> time now <> showCond (diffUTCTime (zonedTimeToUTC now) startTime > 15) (markup grey " nom hasn‘t detected any input. Have you redirected nix-build stderr into nom? (See the README for details.)")
      | not anythingGoingOn = time
      | showBuildGraph = \now -> buildsDisplay now <> table (time now)
      | otherwise = time .> table
    maxHeight = case maybeWindow of
      Just limit -> limit `div` targetRatio
      Nothing -> defaultTreeMax
    buildsDisplay now =
      prependLines
        (upperleft <> horizontal)
        (vertical <> " ")
        (vertical <> " ")
        (printBuilds buildState maxHeight (zonedTimeToUTC now))
        <> "\n"
  runTime now = timeDiff (zonedTimeToUTC now) startTime
  time
    | processState == Finished = \now -> finishMarkup (" at " <> toText (formatTime defaultTimeLocale "%H:%M:%S" now) <> " after " <> runTime now)
    | otherwise = \now -> clock <> " " <> runTime now
  MkDependencySummary{..} = fullSummary
  runningBuilds' = runningBuilds <|>> (.host)
  completedBuilds' = completedBuilds <|>> (.host)
  numFailedBuilds = CMap.size failedBuilds
  anythingGoingOn = fullSummary /= mempty
  showBuildGraph = not (Seq.null forestRoots)
  table time' =
    prependLines
      ((if showBuildGraph then leftT else upperleft) <> stimes (3 :: Int) horizontal <> " ")
      (vertical <> "    ")
      (lowerleft <> horizontal <> " " <> bigsum <> " ")
      $ printAlignedSep (innerTable `appendr` one (lastRow time'))
  innerTable :: [NonEmpty Entry]
  innerTable = fromMaybe (one (text "")) (nonEmpty headers) : showCond showHosts printHosts
  headers =
    (cells 3 <$> optHeader showBuilds "Builds")
      <> (cells 3 <$> optHeader showDownloads "Downloads")
      <> (cells 2 <$> optHeader showUploads "Uploads")
      <> optHeader showHosts "Host"
  optHeader cond = showCond cond . one . bold . header :: Text -> [Entry]
  partial_last_row =
    showCond
      showBuilds
      [ nonZeroBold numRunningBuilds (yellow (label running (disp numRunningBuilds)))
      , nonZeroBold numCompletedBuilds (green (label done (disp numCompletedBuilds)))
      , nonZeroBold numPlannedBuilds (blue (label todo (disp numPlannedBuilds)))
      ]
      <> showCond
        showDownloads
        [ nonZeroBold downloadsRunning (yellow (label down (disp downloadsRunning)))
        , nonZeroBold downloadsDone (green (label down (disp downloadsDone)))
        , nonZeroBold numPlannedDownloads . blue . label todo . disp $ numPlannedDownloads
        ]
      <> showCond showUploads [
        nonZeroBold uploadsRunning (yellow (label up (disp uploadsRunning))),
        nonZeroBold uploadsDone (green (label up (disp uploadsDone)))
        ]
  lastRow time' = partial_last_row `appendr` one (bold (header time'))

  showHosts = numHosts > 0
  showBuilds = totalBuilds > 0
  showDownloads = downloadsDone + CSet.size plannedDownloads > 0
  showUploads = CMap.size completedUploads > 0
  numPlannedDownloads = CSet.size plannedDownloads
  numHosts =
    Set.size (Set.filter (/= Localhost) (foldMap one runningBuilds' <> foldMap one completedBuilds' <> foldMap one completedUploads))
  numRunningBuilds = CMap.size runningBuilds
  numCompletedBuilds = CMap.size completedBuilds
  numPlannedBuilds = CSet.size plannedBuilds
  totalBuilds = numPlannedBuilds + numRunningBuilds + numCompletedBuilds
  downloadsDone = CMap.size completedDownloads
  downloadsRunning = CMap.size runningDownloads
  uploadsRunning = CMap.size runningUploads
  uploadsDone = CMap.size completedUploads
  finishMarkup 
    | numFailedBuilds > 0 = ((warning <> " Exited after " <> show numFailedBuilds <> " build failures") <>) .> markup red
    | not (null nixErrors) = ((warning <> " Exited with " <> show (length nixErrors) <>  " errors reported by nix") <>) .> markup red
    | otherwise = ("Finished" <>) .> markup green
  printHosts :: [NonEmpty Entry]
  printHosts =
    mapMaybe nonEmpty $ labelForHost <$> hosts
   where
    labelForHost :: Host -> [Entry]
    labelForHost h =
      showCond
        showBuilds
        [ nonZeroShowBold numRunningBuildsOnHost (yellow (label running (disp numRunningBuildsOnHost)))
        , nonZeroShowBold doneBuilds (green (label done (disp doneBuilds)))
        , dummy
        ]
        <> showCond
          showDownloads
          [ nonZeroShowBold downloadsRunning' (yellow (label down (disp downloadsRunning')))
           , nonZeroShowBold downloads (green (label down (disp downloads))), dummy]
        <> showCond
          showUploads
          [nonZeroShowBold uploadsRunning' (yellow (label up (disp uploadsRunning'))),
          nonZeroShowBold uploads (green (label up (disp uploads)))]
        <> one (magenta (header (toText h)))
     where
      uploads = l h completedUploads
      uploadsRunning' = l h runningUploads
      downloads = l h completedDownloads
      downloadsRunning' = l h runningDownloads
      numRunningBuildsOnHost = l h runningBuilds'
      doneBuilds = l h completedBuilds'
    hosts =
      sort . toList @Set $
        foldMap (foldMap one) [runningBuilds', completedBuilds'] <> foldMap (foldMap one) [completedUploads, completedDownloads]
    l :: (Host -> CMap.CacheIdMap b Host -> Int)
    l host = CMap.size . CMap.filter (host ==)

nonZeroShowBold :: Int -> Entry -> Entry
nonZeroShowBold num = if num > 0 then bold else const dummy

nonZeroBold :: Int -> Entry -> Entry
nonZeroBold num = if num > 0 then bold else id

data TreeLocation = Root | Twig | Leaf deriving stock (Eq)

printBuilds ::
  NOMV1State ->
  Int ->
  UTCTime ->
  NonEmpty Text
printBuilds nomState@MkNOMV1State{..} maxHeight = printBuildsWithTime
 where
  printBuildsWithTime :: UTCTime -> NonEmpty Text
  printBuildsWithTime now = preparedPrintForest |> fmap (fmap (now |>)) .> showForest .> (graphHeader :|)
  num_raw_roots = length forestRoots
  num_roots = length preparedPrintForest
  graphTitle = markup bold "Dependency Graph"
  graphHeader = " " <> graphHeaderInner <> ":"
  graphHeaderInner
    | num_raw_roots <= 1 = graphTitle
    | num_raw_roots == num_roots = unwords [graphTitle, "with", show num_roots, "roots"]
    | otherwise = unwords [graphTitle, "showing", show num_roots, "of", show num_raw_roots, "roots"]
  preparedPrintForest :: Forest (UTCTime -> Text)
  preparedPrintForest = buildForest <|>> mapRootsTwigsAndLeafs (printTreeNode Root) (printTreeNode Twig) (printTreeNode Leaf)
  printTreeNode :: TreeLocation -> DerivationInfo -> UTCTime -> Text
  printTreeNode location drvInfo =
      let summary = showSummary drvInfo.dependencySummary
       in \now -> printDerivation drvInfo now <> showCond (location == Leaf && not (Text.null summary)) (markup grey " waiting for " <> summary)

  buildForest :: Forest DerivationInfo
  buildForest = evalState (goBuildForest forestRoots) mempty

  goBuildForest :: Seq DerivationId -> State DerivationSet (Forest DerivationInfo)
  goBuildForest = \case
    (thisDrv Seq.:<| restDrvs) -> do
      seen_ids <- get
      let mkNode
            | not (CSet.member thisDrv seen_ids) && CSet.member thisDrv derivationsToShow = do
              let drvInfo = get' (getDerivationInfos thisDrv)
                  childs = children thisDrv
              modify (CSet.insert thisDrv)
              subforest <- goBuildForest childs
              pure (Node drvInfo subforest :)
            | otherwise = pure id
      prepend_node <- mkNode
      goBuildForest restDrvs <|>> prepend_node
    _ -> pure []
  derivationsToShow :: DerivationSet
  derivationsToShow =
    let should_be_shown (index, (can_be_hidden, _, _)) = not can_be_hidden || index < maxHeight
        (_, sorted_set) = execState (goDerivationsToShow forestRoots) mempty
     in sorted_set
          |> Set.toAscList
          .> itoList
          .> takeWhile should_be_shown
          .> fmap (\(_, (_, _, drvId)) -> drvId)
          .> CSet.fromFoldable

  children :: DerivationId -> Seq DerivationId
  children drv_id = get' (getDerivationInfos drv_id) |> (.inputDerivations) <.>> fst

  goDerivationsToShow ::
    Seq DerivationId ->
    State
      ( DerivationSet -- seenIds
      , Set
          ( Bool -- is allowed to be hidden,
          , SortKey
          , DerivationId
          )
      )
      ()
  goDerivationsToShow = \case
    (thisDrv Seq.:<| restDrvs) -> do
      (seen_ids, sorted_set) <- get
      let sort_key = sortKey nomState thisDrv
          summary@MkDependencySummary{..} = get' (summaryIncludingRoot thisDrv)
          runningTransfers = CMap.keysSet runningDownloads <> CMap.keysSet runningUploads
          nodesOfRunningTransfers = flip foldMap (CSet.toList runningTransfers) \path -> 
            let infos = get' (getStorePathInfos path)
            in infos.inputFor <> CSet.fromFoldable infos.producer
          may_hide = CSet.isSubsetOf (nodesOfRunningTransfers <> CMap.keysSet failedBuilds <> CMap.keysSet runningBuilds) seen_ids
          new_seen_ids = CSet.insert thisDrv seen_ids
          new_sorted_set = Set.insert (may_hide, sort_key, thisDrv) sorted_set
          show_this_node =
            summary /= mempty
              && not (CSet.member thisDrv seen_ids)
              && ( not may_hide
                    || Set.size sorted_set < maxHeight
                    || sort_key < view _2 (Set.elemAt (maxHeight - 1) sorted_set)
                 )
      when show_this_node $ put (new_seen_ids, new_sorted_set) >> goDerivationsToShow (children thisDrv)
      goDerivationsToShow restDrvs
    _ -> pass

  get' :: NOMState b -> b
  get' procedure = evalState procedure nomState

  showSummary :: DependencySummary -> Text
  showSummary MkDependencySummary{..} =
    [ 
      memptyIfTrue (CMap.null failedBuilds) 
        [markup red $ show (CMap.size failedBuilds) <> " failed"],
      memptyIfTrue (CMap.null runningBuilds) 
        [markup yellow $ show (CMap.size runningBuilds) <> " building"],
      memptyIfTrue (CSet.null plannedBuilds) 
        [markup blue $ show (CSet.size plannedBuilds) <> " waiting builds"],
      memptyIfTrue (CMap.null runningUploads) 
        [markup magenta $ show (CMap.size runningUploads) <> " uploading"],
      memptyIfTrue (CMap.null runningDownloads) 
        [markup yellow $ show (CMap.size runningDownloads) <> " downloads"],
      memptyIfTrue (CSet.null plannedDownloads) 
        [markup blue $ show (CSet.size plannedDownloads) <> " waiting downloads"]
    ]
      |> join .> unwords

  printDerivation :: DerivationInfo -> UTCTime -> Text
  printDerivation drvInfo = case drvInfo.buildStatus of
    Unknown -> const if CSet.fromFoldable (Map.elems drvInfo.outputs) `CSet.intersection` CMap.keysSet drvInfo.dependencySummary.runningDownloads /= mempty then markups [bold, yellow] (down <> " " <> drvName) else drvName
    Planned -> const (markup blue (todo <> " " <> drvName))
    Building buildInfo -> let
        phaseList = case phaseMay buildInfo.end of
          Nothing -> []
          Just phase -> [markup bold ("(" <> phase <> ")")]
      in \now ->
      unwords $
        [markups [yellow, bold] (running <> " " <> drvName)]
          <> hostMarkup buildInfo.host
          <> phaseList
          <> [clock, timeDiff now buildInfo.start]
          <> maybe [] (\x -> ["(" <> average <> timeDiffSeconds x <> ")"]) buildInfo.estimate
    Failed buildInfo ->
      let
         (endTime, failType, activityId) = buildInfo.end
         phaseInfo = case phaseMay activityId of
           Nothing -> []
           Just phase -> ["in",phase]
      in
      const $
        unwords $
          [markups [red, bold] (warning <> " " <> drvName)]
            <> hostMarkup buildInfo.host
            <> [markups [red, bold] (unwords $ ["failed with", printFailType failType, "after", clock, timeDiff endTime buildInfo.start] <> phaseInfo)]
            
    Built buildInfo ->
      const $
        unwords $
          [markup green (done <> " " <> drvName)]
            <> hostMarkup buildInfo.host
            <> [markup grey (clock <> " " <> timeDiff buildInfo.end buildInfo.start)]
   where
    phaseMay activityId' = do 
          activityId <- activityId'
          (_, phase, _) <- IntMap.lookup activityId.value nomState.activities
          phase
    drvName = drvInfo.name.storePath.name

printFailType :: FailType -> Text
printFailType = \case
  ExitCode i -> "exit code " <> show i
  HashMismatch -> "hash mismatch"

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
