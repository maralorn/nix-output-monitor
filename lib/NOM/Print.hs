module NOM.Print (stateToText, showCode, Config (..)) where

import Relude

import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty.Extra (appendr)
import Data.Map.Strict qualified as Map
import Data.MemoTrie (memo)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, ZonedTime, defaultTimeLocale, formatTime)
import Data.Tree (Forest, Tree (Node))
import Optics (itoList, view, _2)
import Streamly.Internal.Data.Time.Units (AbsTime, diffAbsTime)

import System.Console.ANSI (SGR (Reset), setSGRCode)

-- terminal-size
import System.Console.Terminal.Size (Window)
import System.Console.Terminal.Size qualified as Window

import Data.Foldable qualified as Unsafe
import GHC.Records (HasField)
import NOM.Builds (Derivation (..), FailType (..), Host (..), StorePath (..))
import NOM.NixMessage.JSON (ActivityId (..))
import NOM.Print.Table (Entry, blue, bold, cells, disp, dummy, green, grey, header, label, magenta, markup, markups, prependLines, printAlignedSep, red, text, yellow)
import NOM.Print.Tree (showForest)
import NOM.State (BuildInfo (..), BuildStatus (..), DependencySummary (..), DerivationId, DerivationInfo (..), DerivationSet, NOMState, NOMV1State (..), ProcessState (..), StorePathId, StorePathInfo (..), StorePathMap, StorePathSet, TransferInfo (..), getDerivationInfos, getStorePathInfos, inputStorePaths)
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.State.Sorting (SortKey, sortKey, summaryIncludingRoot)
import NOM.State.Tree (mapRootsTwigsAndLeafs)
import NOM.Update (appendDifferingPlatform)
import NOM.Util (diffTime, relTimeToSeconds)
import Text.Printf (printf)

showCode :: Text -> [String]
showCode = map (printf "%02X" . fromEnum) . toString

textRep, vertical, lowerleft, upperleft, horizontal, down, up, clock, running, done, bigsum, warning, todo, leftT, average :: Text
textRep = fromString [toEnum 0xFE0E]
vertical = "┃"
lowerleft = "┗"
upperleft = "┏"
leftT = "┣"
horizontal = "━"
-- >>> showCode down
-- ["2193","FE0E"]
down = "↓" <> textRep
-- >>> showCode up
-- ["2191","FE0E"]
up = "↑" <> textRep
-- >>> showCode clock
-- ["23F1","FE0E"]
clock = "⏱" <> textRep
-- >>> showCode running
-- ["25B6","FE0E"]
running = "⏵" <> textRep
-- >>> showCode done
-- ["2714","FE0E"]
done = "✔" <> textRep
-- >>> showCode todo
-- ["23F3","FE0E"]
todo = "⏳" <> textRep
-- >>> showCode warning
-- ["26A0","FE0E"]
warning = "⚠" <> textRep
-- >>> showCode average
-- ["2205","FE0E"]
average = "∅" <> textRep
-- >>> showCode bigsum
-- ["2211","FE0E"]
bigsum = "∑" <> textRep

showCond :: Monoid m => Bool -> m -> m
showCond = memptyIfFalse

targetRatio, defaultTreeMax :: Int
targetRatio = 3
defaultTreeMax = 20

data Config = MkConfig
  { silent :: Bool
  , piping :: Bool
  }

stateToText :: Config -> NOMV1State -> Maybe (Window Int) -> (ZonedTime, AbsTime) -> Text
stateToText config buildState@MkNOMV1State{..} = memo printWithSize . fmap Window.height
 where
  printWithSize :: Maybe Int -> (ZonedTime, AbsTime) -> Text
  printWithSize maybeWindow = printWithTime
   where
    printWithTime :: (ZonedTime, AbsTime) -> Text
    printWithTime
      | processState == JustStarted && config.piping = \nows@(_, now) -> time nows <> showCond (diffTime now startTime > 15) (markup grey " nom hasn‘t detected any input. Have you redirected nix-build stderr into nom? (See -h and the README for details.)")
      | processState == Finished && config.silent = const ""
      | showBuildGraph = \nows@(_, now) -> buildsDisplay now <> table (time nows)
      | not anythingGoingOn = if config.silent then const "" else time
      | otherwise = table . time
    maxHeight = case maybeWindow of
      Just limit -> limit `div` targetRatio
      Nothing -> defaultTreeMax
    buildsDisplay now =
      prependLines
        (toText (setSGRCode [Reset]) <> upperleft <> horizontal)
        (vertical <> " ")
        (vertical <> " ")
        (printBuilds buildState hostNums maxHeight now)
        <> "\n"
  runTime now = timeDiff now startTime
  time
    | processState == Finished = \(nowClock, now) -> finishMarkup (" at " <> toText (formatTime defaultTimeLocale "%H:%M:%S" nowClock) <> " after " <> runTime now)
    | otherwise = \(_, now) -> clock <> " " <> runTime now
  MkDependencySummary{..} = fullSummary
  runningBuilds' = (.host) <$> runningBuilds
  completedBuilds' = (.host) <$> completedBuilds
  failedBuilds' = (.host) <$> failedBuilds
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
    action_count_for_host :: HasField "host" a Host => Host -> CMap.CacheIdMap b a -> Int
    action_count_for_host host = CMap.size . CMap.filter (\x -> host == x.host)

nonZeroShowBold :: Text -> Int -> Entry
nonZeroShowBold label' num = if num > 0 then label label' $ text (markup bold (show num)) else dummy

nonZeroBold :: Text -> Int -> Entry
nonZeroBold label' num = label label' $ text (markup (if num > 0 then bold else id) (show num))

data TreeLocation = Root | Twig | Leaf deriving stock (Eq)

printBuilds ::
  NOMV1State ->
  [(Host, Int)] ->
  Int ->
  AbsTime ->
  NonEmpty Text
printBuilds nomState@MkNOMV1State{..} hostNums maxHeight = printBuildsWithTime
 where
  hostLabel host = markup magenta $ maybe (toText host) (("[" <>) . (<> "]") . show) (List.lookup host hostNums)
  disambiguate_transfer_host = if length hostNums > 2 then (<> " ") . hostLabel else const ""
  printBuildsWithTime :: AbsTime -> NonEmpty Text
  printBuildsWithTime now = (graphHeader :|) $ showForest $ fmap (fmap ($ now)) preparedPrintForest
  num_raw_roots = length forestRoots
  num_roots = length preparedPrintForest
  graphTitle = markup bold "Dependency Graph"
  graphHeader = " " <> graphHeaderInner <> ":"
  graphHeaderInner
    | num_raw_roots <= 1 = graphTitle
    | num_raw_roots == num_roots = unwords [graphTitle, "with", show num_roots, "roots"]
    | otherwise = unwords [graphTitle, "showing", show num_roots, "of", show num_raw_roots, "roots"]
  preparedPrintForest :: Forest (AbsTime -> Text)
  preparedPrintForest = mapRootsTwigsAndLeafs (printTreeNode Root) (printTreeNode Twig) (printTreeNode Leaf) <$> buildForest
  printTreeNode :: TreeLocation -> DerivationInfo -> AbsTime -> Text
  printTreeNode location drvInfo =
    let ~summary = showSummary drvInfo.dependencySummary
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
     in CSet.fromFoldable $
          fmap (\(_, (_, _, drvId)) -> drvId) $
            takeWhile should_be_shown $
              itoList $
                Set.toAscList sorted_set

  children :: DerivationId -> Seq DerivationId
  children drv_id = fmap fst $ (.inputDerivations) $ get' $ getDerivationInfos drv_id

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
      let ~sort_key = sortKey nomState thisDrv
          summary@MkDependencySummary{..} = get' (summaryIncludingRoot thisDrv)
          ~runningTransfers = CMap.keysSet runningDownloads <> CMap.keysSet runningUploads
          ~nodesOfRunningTransfers = flip foldMap (CSet.toList runningTransfers) \path ->
            let infos = get' (getStorePathInfos path)
             in infos.inputFor <> CSet.fromFoldable infos.producer
          ~may_hide = CSet.isSubsetOf (nodesOfRunningTransfers <> CMap.keysSet failedBuilds <> CMap.keysSet runningBuilds) seen_ids
          ~show_this_node =
            summary /= mempty
              && not (CSet.member thisDrv seen_ids)
              && ( not may_hide
                    || Set.size sorted_set < maxHeight
                    || sort_key < view _2 (Set.elemAt (maxHeight - 1) sorted_set)
                 )
          ~new_seen_ids = CSet.insert thisDrv seen_ids
          ~new_sorted_set = Set.insert (may_hide, sort_key, thisDrv) sorted_set
      when show_this_node $ put (new_seen_ids, new_sorted_set) >> goDerivationsToShow (children thisDrv)
      goDerivationsToShow restDrvs
    _ -> pass

  get' :: NOMState b -> b
  get' procedure = evalState procedure nomState

  showSummary :: DependencySummary -> Text
  showSummary MkDependencySummary{..} =
    unwords $
      join
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

  hostMarkup :: Host -> [Text]
  hostMarkup Localhost = mempty
  hostMarkup host = ["on " <> hostLabel host]

  printDerivation :: DerivationInfo -> Map Text StorePathId -> (Bool, AbsTime -> Text)
  printDerivation drvInfo _input_store_paths = do
    let store_paths_in :: StorePathSet -> Bool
        store_paths_in some_set = not $ Map.null $ Map.filter (`CSet.member` some_set) drvInfo.outputs
        store_paths_in_map :: StorePathMap (TransferInfo a) -> [TransferInfo a]
        store_paths_in_map info_map = toList $ Map.mapMaybe (`CMap.lookup` info_map) drvInfo.outputs
        hosts :: [TransferInfo a] -> [Host]
        hosts = toList . Set.fromList . fmap (.host)
        earliest_start :: [TransferInfo a] -> AbsTime
        earliest_start = Unsafe.minimum . fmap (.start)
        build_sum :: [TransferInfo (Maybe AbsTime)] -> NominalDiffTime
        build_sum = relTimeToSeconds . sum . fmap (\transfer_info -> maybe 0 (diffAbsTime transfer_info.start) transfer_info.end)
        phaseMay activityId' = do
          activityId <- activityId'
          (_, phase, _) <- IntMap.lookup activityId.value nomState.activities
          phase
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
                    markups [bold, yellow] (down <> " " <> running <> " " <> drvName)
                      <> unwords
                        ( (markup magenta . disambiguate_transfer_host <$> hosts downloadingOutputs)
                            <> (let age = relTimeToSeconds $ diffAbsTime now (earliest_start downloadingOutputs) in if age > 1 then [clock, printDuration age] else [])
                        )
                )
            | not $ null uploadingOutputs ->
                ( False
                , \now ->
                    markups [bold, yellow] (up <> " " <> running <> " " <> drvName)
                      <> unwords
                        ( (markup magenta . disambiguate_transfer_host <$> hosts uploadingOutputs)
                            <> (let age = relTimeToSeconds $ diffAbsTime now (earliest_start uploadingOutputs) in if age > 1 then [clock, printDuration age] else [])
                        )
                )
          Unknown
            | plannedDownloads -> (True, const $ markup blue (down <> " " <> todo <> " " <> drvName))
            | not $ null downloadedOutputs ->
                ( False
                , const $
                    markup green (down <> " " <> done <> " " <> drvName)
                      <> unwords
                        ( (markup magenta . disambiguate_transfer_host <$> hosts downloadedOutputs)
                            <> (let age = build_sum downloadedOutputs in ([markup grey $ clock <> " " <> printDuration age | age > 1]))
                        )
                )
            | not $ null uploadedOutputs ->
                ( False
                , const $
                    markup green (up <> " " <> done <> " " <> drvName)
                      <> unwords
                        ( (markup magenta . disambiguate_transfer_host <$> hosts uploadedOutputs)
                            <> (let age = build_sum uploadedOutputs in ([markup grey $ clock <> " " <> printDuration age | age > 1]))
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
                    <> hostMarkup buildInfo.host
                    <> phaseList
                after_time = maybe [] (\x -> ["(" <> average <> " " <> timeDiffSeconds x <> ")"]) buildInfo.estimate
             in (False, \now -> unwords $ before_time <> [clock, timeDiff now buildInfo.start] <> after_time)
          Failed buildInfo ->
            let (endTime, failType) = buildInfo.end
                phaseInfo = case phaseMay buildInfo.activityId of
                  Nothing -> []
                  Just phase -> ["in", phase]
             in ( False
                , const $
                    unwords $
                      [markups [red, bold] (warning <> " " <> drvName)]
                        <> hostMarkup buildInfo.host
                        <> [markups [red, bold] (unwords $ ["failed with", printFailType failType, "after", clock, timeDiff endTime buildInfo.start] <> phaseInfo)]
                )
          Built buildInfo ->
            ( False
            , const $
                unwords $
                  [markup green (done <> " " <> drvName)]
                    <> hostMarkup buildInfo.host
                    <> [markup grey (clock <> " " <> timeDiff buildInfo.end buildInfo.start)]
            )

printFailType :: FailType -> Text
printFailType = \case
  ExitCode i -> "exit code " <> show i
  HashMismatch -> "hash mismatch"

timeDiff :: AbsTime -> AbsTime -> Text
timeDiff x =
  printDuration . relTimeToSeconds . diffAbsTime x

printDuration :: NominalDiffTime -> Text
printDuration diff
  | diff < 60 = p "%Ss"
  | diff < 60 * 60 = p "%Mm%Ss"
  | otherwise = p "%Hh%Mm%Ss"
 where
  p x = toText $ formatTime defaultTimeLocale x diff

timeDiffSeconds :: Int -> Text
timeDiffSeconds = printDuration . fromIntegral
