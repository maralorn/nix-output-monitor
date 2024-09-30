module NOM.Print (stateToText, showCode, Config (..)) where

import Data.Foldable qualified as Unsafe
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty.Extra (appendr)
import Data.Map.Strict qualified as Map
import Data.MemoTrie (memo)
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Strict qualified as Strict
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, ZonedTime, defaultTimeLocale, formatTime)
import Data.Tree (Forest, Tree (Node))
import GHC.Records (HasField)
import NOM.Builds (Derivation (..), FailType (..), Host (..), StorePath (..))
import NOM.NixMessage.JSON (ActivityId (..))
import NOM.Print.Table (Entry, blue, bold, cells, dummy, green, grey, header, label, magenta, markup, markups, prependLines, printAlignedSep, red, text, yellow)
import NOM.Print.Tree (showForest)
import NOM.State (
  ActivityStatus (..),
  BuildFail (..),
  BuildInfo (..),
  BuildStatus (..),
  DependencySummary (..),
  DerivationId,
  DerivationInfo (..),
  DerivationSet,
  InputDerivation (..),
  NOMState,
  NOMV1State (..),
  ProgressState (..),
  StorePathId,
  StorePathInfo (..),
  StorePathMap,
  StorePathSet,
  TransferInfo (..),
  getDerivationInfos,
  getStorePathInfos,
  inputStorePaths,
 )
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.State.Sorting (SortKey, sortKey, summaryIncludingRoot)
import NOM.State.Tree (mapRootsTwigsAndLeafs)
import NOM.Update (appendDifferingPlatform)
import Optics (itoList, view, _2)
import Relude
import System.Console.ANSI (SGR (Reset), setSGRCode)
import System.Console.Terminal.Size (Window)
import System.Console.Terminal.Size qualified as Window
import Text.Printf (printf)

showCode :: Text -> [String]
showCode = map (printf "%02X" . fromEnum) . toString

vertical, lowerleft, upperleft, horizontal, down, up, clock, running, done, bigsum, warning, todo, leftT, average :: Text

-- | U+2503 BOX DRAWINGS HEAVY VERTICAL
vertical = "┃"

-- | U+2517 BOX DRAWINGS HEAVY UP AND RIGHT
lowerleft = "┗"

-- | U+250F BOX DRAWINGS HEAVY DOWN AND RIGHT
upperleft = "┏"

-- | U+2523 BOX DRAWINGS HEAVY VERTICAL AND RIGHT
leftT = "┣"

-- | U+2501 BOX DRAWINGS HEAVY HORIZONTAL
horizontal = "━"

-- | U+2193 DOWNWARDS ARROW
down = "↓"

-- | U+2191 UPWARDS ARROW
up = "↑"

-- | U+23F1 STOPWATCH
clock = "⏱"

-- | U+EB70 TRIANGLE RIGHT
-- running = ""
running = "\xeb70"

-- | U+2714 HEAVY CHECK MARK
done = "✔"

-- | U+23F8 DOUBLE VERTICAL BAR
todo = "⏸"

-- | U+26A0 WARNING SIGN
warning = "⚠"

-- | U+2205 EMPTY SET
average = "∅"

-- | U+2211 N-ARY SUMMATION
bigsum = "∑"

showCond :: (Monoid m) => Bool -> m -> m
showCond = memptyIfFalse

targetRatio, defaultTreeMax :: Int
targetRatio = 3 -- We divide by this, don‘t set this to zero.
defaultTreeMax = 20

data Config = MkConfig
  { silent :: Bool
  , piping :: Bool
  }

printSections :: NonEmpty Text -> Text
printSections = (upperleft <>) . Text.intercalate (toText (setSGRCode [Reset]) <> "\n" <> leftT) . toList

-- printInterestingActivities :: Maybe Text -> IntMap InterestingActivity -> (ZonedTime, Double) -> Text
-- printInterestingActivities message activities (_, now) =
--   prependLines
--     ""
--     (vertical <> " ")
--     (vertical <> " ")
--     (horizontal <> markup bold " Build Planning:" :| maybeToList message <> (IntMap.elems activities <&> \activity -> unwords (activity.text : ifTimeDiffRelevant now activity.start id)))

printTraces :: Seq Text -> Int -> Text
printTraces traces maxHeight =
  prependLines
    ""
    (vertical <> " ")
    (vertical <> " ")
    (horizontal <> markup (bold . yellow) (" " <> show (length interesting_traces) <> " Traces: ") :| (lines =<< filtered_traces))
 where
  interesting_traces = toList traces
  compact_traces = sum (length . lines <$> interesting_traces) > maxHeight
  filtered_traces = (if compact_traces then map compactError else id) interesting_traces

printErrors :: Seq Text -> Int -> Text
printErrors errors maxHeight =
  prependLines
    ""
    (vertical <> " ")
    (vertical <> " ")
    (horizontal <> markup (bold . red) (" " <> show (length interesting_errors) <> " Errors: ") :| (lines =<< filtered_errors))
 where
  interesting_errors =
    reverse
      . filter (not . Text.isInfixOf "dependencies of derivation")
      $ toList errors
  compact_errors = sum (length . lines <$> interesting_errors) > maxHeight
  filtered_errors = (if compact_errors then map compactError else id) interesting_errors

compactError :: Text -> Text
compactError = fst . Text.breakOn "\n       last 10 log lines:"

stateToText :: Config -> NOMV1State -> Maybe (Window Int) -> (ZonedTime, Double) -> Text
stateToText config buildState@MkNOMV1State{..} = memo printWithSize . fmap Window.height
 where
  printWithSize :: Maybe Int -> (ZonedTime, Double) -> Text
  printWithSize maybeWindow = printWithTime
   where
    printWithTime :: (ZonedTime, Double) -> Text
    printWithTime
      | progressState == JustStarted && config.piping = \nows@(_, now) -> markup bold (time nows <> showCond (now - startTime > 15) (markup grey " nom hasn‘t detected any input. Have you redirected nix-build stderr into nom? (See -h and the README for details.)"))
      | progressState == Finished && config.silent = const ""
      | Just sections' <- nonEmpty sections = \now -> printSections . fmap ($ now) $ sections' <> one (table . time)
      | fullSummary /= mempty = printSections . one . table . time
      | config.silent = const ""
      | otherwise = markup bold . time
    sections =
      fmap snd
        . filter fst
        $ [
            -- (not (IntMap.null interestingActivities) || isJust evalMessage, printInterestingActivities evalMessage interestingActivities)
            (not (Seq.null nixErrors), const errorDisplay)
          , (not (Seq.null nixTraces), const traceDisplay)
          , (not (Seq.null forestRoots), buildsDisplay . snd)
          ]
    maxHeight = case maybeWindow of
      Just limit -> limit `div` targetRatio -- targetRatio is hardcoded to be bigger than zero.
      Nothing -> defaultTreeMax
    buildsDisplay now =
      prependLines
        horizontal
        (vertical <> " ")
        (vertical <> " ")
        (printBuilds buildState hostNums maxHeight now)
    errorDisplay = printErrors nixErrors maxHeight
    traceDisplay = printTraces nixTraces maxHeight
  -- evalMessage = case evaluationState.lastFileName of
  --   Strict.Just file_name -> Just ("Evaluated " <> show (evaluationState.count) <> " files, last one was '" <> file_name <> "'")
  --   Strict.Nothing -> Nothing
  runTime now = timeDiff now startTime
  time
    | progressState == Finished = \(nowClock, now) -> finishMarkup (" at " <> toText (formatTime defaultTimeLocale "%H:%M:%S" nowClock) <> " after " <> runTime now)
    | otherwise = \(_, now) -> clock <> " " <> runTime now
  MkDependencySummary{..} = fullSummary
  runningBuilds' = (.host) <$> runningBuilds
  completedBuilds' = (.host) <$> completedBuilds
  failedBuilds' = (.host) <$> failedBuilds
  numFailedBuilds = CMap.size failedBuilds
  table time' =
    prependLines
      (stimes (3 :: Int) horizontal <> " ")
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
      [ yellow $ nonZeroBold running numRunningBuilds
      , green $ nonZeroBold done numCompletedBuilds
      , blue $ nonZeroBold todo numPlannedBuilds
      ]
      <> showCond
        showDownloads
        [ yellow $ nonZeroBold down downloadsRunning
        , green $ nonZeroBold down downloadsDone
        , blue $ nonZeroBold todo numPlannedDownloads
        ]
      <> showCond
        showUploads
        [ yellow $ nonZeroBold up uploadsRunning
        , green $ nonZeroBold up uploadsDone
        ]
  lastRow time' = partial_last_row `appendr` one (bold (header time'))

  showHosts = Set.size hosts > 1
  manyHosts = Set.size buildHosts > 1 || Set.size hosts > 2 -- We only need number labels on hosts if we are using remote builders or more then one transfer peer (normally a substitution cache).
  hostNums = zip (toList hosts) [0 ..]
  showBuilds = totalBuilds > 0
  showDownloads = downloadsDone + downloadsRunning + numPlannedDownloads > 0
  showUploads = uploadsDone + uploadsRunning > 0
  numPlannedDownloads = CSet.size plannedDownloads
  buildHosts = one Localhost <> foldMap (foldMap one) [runningBuilds', completedBuilds', failedBuilds']
  hosts = buildHosts <> foldMap (foldMap (one . (.host))) [completedUploads, completedDownloads]
  numRunningBuilds = CMap.size runningBuilds
  numCompletedBuilds = CMap.size completedBuilds
  numPlannedBuilds = CSet.size plannedBuilds
  totalBuilds = numPlannedBuilds + numRunningBuilds + numCompletedBuilds
  downloadsDone = CMap.size completedDownloads
  downloadsRunning = CMap.size runningDownloads
  uploadsRunning = CMap.size runningUploads
  uploadsDone = CMap.size completedUploads
  finishMarkup
    | numFailedBuilds > 0 = markup red . ((warning <> " Exited after " <> show numFailedBuilds <> " build failures") <>)
    | not (null nixErrors) = markup red . ((warning <> " Exited with " <> show (length nixErrors) <> " errors reported by nix") <>)
    | not (null nixTraces) = markup yellow . ((warning <> " Exited with " <> show (length nixTraces) <> " traces reported by nix") <>)
    | otherwise = markup green . ("Finished" <>)
  printHosts :: [NonEmpty Entry]
  printHosts =
    mapMaybe (nonEmpty . labelForHost) hostNums
   where
    labelForHost :: (Host, Int) -> [Entry]
    labelForHost (host, index) =
      showCond
        showBuilds
        [ yellow $ nonZeroShowBold running numRunningBuildsOnHost
        , green $ nonZeroShowBold done doneBuilds
        , dummy
        ]
        <> showCond
          showDownloads
          [ yellow $ nonZeroShowBold down downloadsRunning'
          , green $ nonZeroShowBold down downloads
          , dummy
          ]
        <> showCond
          showUploads
          [ yellow $ nonZeroShowBold up uploadsRunning'
          , green $ nonZeroShowBold up uploads
          ]
        <> one (magenta . header $ (if index > 0 && manyHosts then "[" <> show index <> "]: " else "") <> toText host)
     where
      uploads = action_count_for_host host completedUploads
      uploadsRunning' = action_count_for_host host runningUploads
      downloads = action_count_for_host host completedDownloads
      downloadsRunning' = action_count_for_host host runningDownloads
      numRunningBuildsOnHost = action_count_for_host host runningBuilds
      doneBuilds = action_count_for_host host completedBuilds
    action_count_for_host :: (HasField "host" a Host) => Host -> CMap.CacheIdMap b a -> Int
    action_count_for_host host = CMap.size . CMap.filter (\x -> host == x.host)

nonZeroShowBold :: Text -> Int -> Entry
nonZeroShowBold label' num = if num > 0 then label label' $ text (markup bold (show num)) else dummy

nonZeroBold :: Text -> Int -> Entry
nonZeroBold label' num = label label' $ text (markup (if num > 0 then bold else id) (show num))

data TreeLocation = Root | Twig | Leaf deriving stock (Eq)

ifTimeDiffRelevant :: Double -> Double -> ([Text] -> [Text]) -> [Text]
ifTimeDiffRelevant to from = ifTimeDurRelevant $ realToFrac (to - from)

ifTimeDurRelevant :: NominalDiffTime -> ([Text] -> [Text]) -> [Text]
ifTimeDurRelevant dur mod' = memptyIfFalse (dur > 1) (mod' [clock, printDuration dur])

printBuilds ::
  NOMV1State ->
  [(Host, Int)] ->
  Int ->
  Double ->
  NonEmpty Text
printBuilds nomState@MkNOMV1State{..} hostNums maxHeight = printBuildsWithTime
 where
  hostLabel :: Bool -> Host -> Text
  hostLabel color host = (if color then markup magenta else id) $ maybe (toText host) (("[" <>) . (<> "]") . show) (List.lookup host hostNums)
  printBuildsWithTime :: Double -> NonEmpty Text
  printBuildsWithTime now = (graphHeader :|) $ showForest $ fmap (fmap ($ now)) preparedPrintForest
  num_raw_roots = length forestRoots
  num_roots = length preparedPrintForest
  graphTitle = markup bold "Dependency Graph"
  graphHeader = " " <> graphHeaderInner <> ":"
  graphHeaderInner
    | num_raw_roots <= 1 = graphTitle
    | num_raw_roots == num_roots = unwords [graphTitle, "with", show num_roots, "roots"]
    | otherwise = unwords [graphTitle, "showing", show num_roots, "of", show num_raw_roots, "roots"]
  preparedPrintForest :: Forest (Double -> Text)
  preparedPrintForest = mapRootsTwigsAndLeafs (printTreeNode Root) (printTreeNode Twig) (printTreeNode Leaf) <$> buildForest
  printTreeNode :: TreeLocation -> DerivationInfo -> Double -> Text
  printTreeNode location drvInfo =
    let summary = showSummary drvInfo.dependencySummary
        (planned, display_drv) = printDerivation drvInfo (get' (inputStorePaths drvInfo))
        displayed_summary = showCond (location == Leaf && planned && not (Text.null summary)) (markup grey " waiting for " <> summary)
     in \now -> display_drv now <> displayed_summary

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
      prepend_node <$> goBuildForest restDrvs
    _ -> pure []
  derivationsToShow :: DerivationSet
  derivationsToShow =
    let should_be_shown (index, (can_be_hidden, _, _)) = not can_be_hidden || index < maxHeight
        (_, sorted_set) = execState (goDerivationsToShow forestRoots) mempty
     in CSet.fromFoldable
          $ fmap (\(_, (_, _, drvId)) -> drvId)
          $ takeWhile should_be_shown
          $ itoList
          $ Set.toAscList sorted_set

  children :: DerivationId -> Seq DerivationId
  children drv_id = fmap (.derivation) $ (.inputDerivations) $ get' $ getDerivationInfos drv_id

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
          show_this_node =
            maxHeight
              > 0
              && summary
              /= mempty
              && not (CSet.member thisDrv seen_ids)
              && ( not may_hide
                    || Set.size sorted_set
                    < maxHeight
                    || sort_key
                    < view _2 (Set.elemAt (maxHeight - 1) sorted_set)
                 )
          new_seen_ids = CSet.insert thisDrv seen_ids
          new_sorted_set = Set.insert (may_hide, sort_key, thisDrv) sorted_set
      when show_this_node $ put (new_seen_ids, new_sorted_set) >> goDerivationsToShow (children thisDrv)
      goDerivationsToShow restDrvs
    _ -> pass

  get' :: NOMState b -> b
  get' procedure = evalState procedure nomState

  showSummary :: DependencySummary -> Text
  showSummary MkDependencySummary{..} =
    unwords
      $ join
        [ memptyIfTrue
            (CMap.null failedBuilds)
            [markup red $ show (CMap.size failedBuilds) <> " " <> warning]
        , memptyIfTrue
            (CMap.null runningBuilds)
            [markup yellow $ show (CMap.size runningBuilds) <> " " <> running]
        , memptyIfTrue
            (CSet.null plannedBuilds)
            [markup blue $ show (CSet.size plannedBuilds) <> " " <> todo]
        , memptyIfTrue
            (CMap.null runningUploads)
            [markup yellow $ show (CMap.size runningUploads) <> " " <> up]
        , memptyIfTrue
            (CMap.null runningDownloads)
            [markup yellow $ show (CMap.size runningDownloads) <> " " <> down]
        , memptyIfTrue
            (CSet.null plannedDownloads)
            [markup blue $ show (CSet.size plannedDownloads) <> " " <> down <> " " <> todo]
        ]

  hostMarkup :: Bool -> Host -> [Text]
  hostMarkup _ Localhost = mempty
  hostMarkup color host = ["on", hostLabel color host]

  print_hosts :: Bool -> Text -> [Host] -> [Text]
  print_hosts color direction_label hosts
    | null hosts || length hostNums <= 2 = []
    | otherwise = direction_label : (hostLabel color <$> hosts)
  print_hosts_down color = print_hosts color "from"
  print_hosts_up color = print_hosts color "to"

  printDerivation :: DerivationInfo -> Map Text StorePathId -> (Bool, Double -> Text)
  printDerivation drvInfo _input_store_paths = do
    let store_paths_in :: StorePathSet -> Bool
        store_paths_in some_set = not $ Map.null $ Map.filter (`CSet.member` some_set) drvInfo.outputs
        store_paths_in_map :: StorePathMap (TransferInfo a) -> [TransferInfo a]
        store_paths_in_map info_map = toList $ Map.mapMaybe (`CMap.lookup` info_map) drvInfo.outputs
        hosts :: [TransferInfo a] -> [Host]
        hosts = toList . Set.fromList . fmap (.host)
        earliest_start :: [TransferInfo a] -> Double
        earliest_start = Unsafe.minimum . fmap (.start)
        build_sum :: [TransferInfo (Strict.Maybe Double)] -> NominalDiffTime
        build_sum = sum . fmap (\transfer_info -> realToFrac $ Strict.maybe 0 (transfer_info.start -) transfer_info.end)
        phaseMay activityId' = do
          activityId <- Strict.toLazy activityId'
          activity_status <- IntMap.lookup activityId.value nomState.activities
          Strict.toLazy $ activity_status.phase
        drvName = appendDifferingPlatform nomState drvInfo drvInfo.name.storePath.name
        downloadingOutputs = store_paths_in_map drvInfo.dependencySummary.runningDownloads
        uploadingOutputs = store_paths_in_map drvInfo.dependencySummary.runningUploads
        plannedDownloads = store_paths_in drvInfo.dependencySummary.plannedDownloads
        downloadedOutputs = store_paths_in_map drvInfo.dependencySummary.completedDownloads
        uploadedOutputs = store_paths_in_map drvInfo.dependencySummary.completedUploads
     in -- This code for printing info about every output proved to be to verbose. Keeping it in case we want something like that later on, maybe as an option.
        -- store_path_info_list =
        --  ((\(name, infos) now -> markups [bold, yellow] (running <> " " <> name <> " " <> down) <> " " <> markup magenta (disambiguate_transfer_host infos.host) <> clock <> " " <> timeDiff now infos.start) <$> store_paths_in_map drvInfo.dependencySummary.runningDownloads)
        --    <> ((\(name, infos) now -> markups [bold, yellow] (running <> " " <> name <> " " <> up) <> " " <> markup magenta (disambiguate_transfer_host infos.host) <> clock <> " " <> timeDiff now infos.start) <$> store_paths_in_map drvInfo.dependencySummary.runningUploads)
        --    <> ((\name -> const $ markup blue (todo <> name <> " " <> down)) <$> store_paths_in drvInfo.dependencySummary.plannedDownloads)
        --    <> ((\(name, infos) -> const $ markups [bold, green] (done <> " ") <> markup green (name <> " " <> down <> " ") <> markup magenta (disambiguate_transfer_host infos.host) <> markup grey (maybe "" (\end -> clock <> " " <> timeDiff end infos.start) infos.end)) <$> store_paths_in_map drvInfo.dependencySummary.completedDownloads)
        --    <> ((\(name, infos) -> const $ markups [bold, green] (done <> " ") <> markup green (name <> " " <> up <> " ") <> markup magenta (disambiguate_transfer_host infos.host) <> markup grey (maybe "" (\end -> clock <> " " <> timeDiff end infos.start) infos.end)) <$> store_paths_in_map drvInfo.dependencySummary.completedUploads)
        -- store_path_infos = if null store_path_info_list then const "" else \now -> " (" <> Text.intercalate ", " (($ now) <$> store_path_info_list) <> ")"

        case drvInfo.buildStatus of
          _
            | not $ null downloadingOutputs ->
                ( False
                , \now ->
                    unwords
                      $ markups [bold, yellow] (down <> " " <> running <> " " <> drvName)
                      : ( print_hosts_down True (hosts downloadingOutputs)
                            <> ifTimeDiffRelevant now (earliest_start downloadingOutputs) id
                        )
                )
            | not $ null uploadingOutputs ->
                ( False
                , \now ->
                    unwords
                      $ markups [bold, yellow] (up <> " " <> running <> " " <> drvName)
                      : ( print_hosts_up True (hosts uploadingOutputs)
                            <> ifTimeDiffRelevant now (earliest_start uploadingOutputs) id
                        )
                )
          Unknown
            | plannedDownloads -> (True, const $ markup blue (down <> " " <> todo <> " " <> drvName))
            | not $ null downloadedOutputs ->
                ( False
                , const
                    $ unwords
                    $ markup green (down <> " " <> done <> " " <> drvName)
                    : fmap
                      (markup grey)
                      ( print_hosts_down False (hosts downloadedOutputs)
                          <> ifTimeDurRelevant (build_sum downloadedOutputs) id
                      )
                )
            | not $ null uploadedOutputs ->
                ( False
                , const
                    $ unwords
                    $ markup green (up <> " " <> done <> " " <> drvName)
                    : fmap
                      (markup grey)
                      ( print_hosts_up False (hosts uploadedOutputs)
                          <> ifTimeDurRelevant (build_sum uploadedOutputs) id
                      )
                )
            | otherwise -> (False, const drvName)
          Planned -> (True, const $ markup blue (todo <> " " <> drvName))
          Building buildInfo ->
            let phaseList = case phaseMay buildInfo.activityId of
                  Nothing -> []
                  Just phase -> [markup bold ("(" <> phase <> ")")]
                before_time =
                  [markups [yellow, bold] (running <> " " <> drvName)]
                    <> hostMarkup True buildInfo.host
                    <> phaseList
                after_time = Strict.maybe [] (\x -> ["(" <> average <> " " <> timeDiffSeconds x <> ")"]) buildInfo.estimate
             in (False, \now -> unwords $ before_time <> ifTimeDiffRelevant now buildInfo.start (<> after_time))
          Failed buildInfo ->
            let MkBuildFail endTime failType = buildInfo.end
                phaseInfo = case phaseMay buildInfo.activityId of
                  Nothing -> []
                  Just phase -> ["in", phase]
             in ( False
                , const
                    . markups [red, bold]
                    . unwords
                    $ [warning, drvName]
                    <> hostMarkup False buildInfo.host
                    <> ["failed with", printFailType failType, "after", clock, timeDiff endTime buildInfo.start]
                    <> phaseInfo
                )
          Built buildInfo ->
            ( False
            , const
                $ markup green (done <> " " <> drvName)
                <> " "
                <> ( markup grey
                      . unwords
                      $ ( hostMarkup False buildInfo.host
                            <> ifTimeDiffRelevant buildInfo.end buildInfo.start id
                        )
                   )
            )

printFailType :: FailType -> Text
printFailType = \case
  ExitCode i -> "exit code " <> show i
  HashMismatch -> "hash mismatch"

timeDiff :: Double -> Double -> Text
timeDiff x =
  printDuration . realToFrac . (x -)

minute :: NominalDiffTime
minute = 60

hour :: NominalDiffTime
hour = 60 * minute

day :: NominalDiffTime
day = 24 * hour

printDuration :: NominalDiffTime -> Text
printDuration diff
  | diff < minute = p "%Ss"
  | diff < hour = p "%Mm%Ss"
  | diff < day = p "%Hh%Mm%Ss"
  | otherwise = p "%dd%Hh%Mm%Ss"
 where
  p x = toText $ formatTime defaultTimeLocale x diff

timeDiffSeconds :: Int -> Text
timeDiffSeconds = printDuration . fromIntegral
