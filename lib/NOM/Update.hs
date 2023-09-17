module NOM.Update (updateStateNixJSONMessage, updateStateNixOldStyleMessage, maintainState, detectLocalFinishedBuilds, appendDifferingPlatform) where

import Control.Monad.Trans.Writer.CPS qualified as CPS
import Data.ByteString.Char8 qualified as ByteString
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Strict qualified as Strict
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import NOM.Builds (Derivation (..), FailType, Host (..), StorePath (..), parseDerivation, parseIndentedStoreObject, parseStorePath)
import NOM.Error (NOMError)
import NOM.NixMessage.JSON (Activity, ActivityId, ActivityResult (..), MessageAction (..), NixJSONMessage (..), ResultAction (..), StartAction (..), StopAction (..), Verbosity (..))
import NOM.NixMessage.JSON qualified as JSON
import NOM.NixMessage.OldStyle (NixOldStyleMessage)
import NOM.NixMessage.OldStyle qualified as OldStyleMessage
import NOM.Parser qualified as Parser
import NOM.Print.Table (blue, markup)
import NOM.State (
  ActivityStatus (..),
  BuildFail (..),
  BuildInfo (..),
  BuildStatus (..),
  DependencySummary,
  DerivationId,
  DerivationInfo (..),
  DerivationMap,
  DerivationSet,
  EvalInfo (..),
  InputDerivation (..),
  NOMState,
  NOMStateT,
  NOMV1State (..),
  OutputName (Out),
  ProgressState (..),
  RunningBuildInfo,
  StorePathId,
  StorePathState (..),
  TransferInfo (..),
  clearDerivationIdFromSummary,
  clearStorePathsFromSummary,
  derivationToAnyOutPath,
  getDerivationId,
  getDerivationInfos,
  getRunningBuildsByHost,
  getStorePathId,
  getStorePathInfos,
  outPathToDerivation,
  parseOutputName,
  updateSummaryForDerivation,
  updateSummaryForStorePath,
 )
import NOM.State qualified as State
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.State.Sorting (sortDepsOfSet, sortKey)
import NOM.StreamParser (stripANSICodes)
import NOM.Update.Monad (
  BuildReportMap,
  MonadCacheBuildReports (..),
  MonadCheckStorePath (..),
  MonadNow (..),
  MonadReadDerivation (..),
  UpdateMonad,
 )
import NOM.Util (foldMapEndo, parseOneText)
import Nix.Derivation qualified as Nix
import Optics (gconstructor, gfield, has, preview, (%), (%~), (.~))
import Relude
import System.Console.ANSI (SGR (Reset), setSGRCode)

type ProcessingT m a = (UpdateMonad m) => NOMStateT (CPS.WriterT [Either NOMError ByteString] m) a

getReportName :: DerivationInfo -> Text
getReportName drv = case drv.pname of
  Strict.Just pname -> pname
  Strict.Nothing -> Text.dropWhileEnd (`Set.member` fromList ".1234567890-") drv.name.storePath.name

maintainState :: Double -> NOMV1State -> NOMV1State
maintainState now = execState $ do
  currentState <- get
  unless (CSet.null currentState.touchedIds) $ do
    sortDepsOfSet currentState.touchedIds
    modify' (gfield @"forestRoots" %~ Seq.sortOn (sortKey currentState))
    modify' (gfield @"touchedIds" .~ mempty)
  when (Strict.isJust currentState.evaluationState.lastFileName && currentState.evaluationState.at <= now - 5 && currentState.fullSummary /= mempty) do
    modify' (gfield @"evaluationState" %~ \old_state -> old_state{lastFileName = Strict.Nothing})

minTimeBetweenPollingNixStore :: NominalDiffTime
minTimeBetweenPollingNixStore = 0.2 -- in seconds

{-# INLINE updateStateNixJSONMessage #-}
updateStateNixJSONMessage :: forall m. (UpdateMonad m) => NixJSONMessage -> NOMV1State -> m (([NOMError], ByteString), Maybe NOMV1State)
updateStateNixJSONMessage input inputState =
  do
    ((hasChanged, msgs), outputState) <-
      runStateT (CPS.runWriterT (processJsonMessage input)) inputState
    let retval = if hasChanged then Just outputState else Nothing
        errors = lefts msgs
    pure ((errors, ByteString.unlines (rights msgs)), retval)

updateStateNixOldStyleMessage :: forall m. (UpdateMonad m) => (Maybe NixOldStyleMessage, ByteString) -> (Maybe Double, NOMV1State) -> m (([NOMError], ByteString), (Maybe Double, Maybe NOMV1State))
updateStateNixOldStyleMessage (result, input) (inputAccessTime, inputState) = do
  now <- getNow

  let processing = maybe noChange (\result' -> processResult result') result
      (outputAccessTime, check)
        | maybe True ((>= minTimeBetweenPollingNixStore) . realToFrac . (now -)) inputAccessTime = (Just now, detectLocalFinishedBuilds)
        | otherwise = (inputAccessTime, pure False)
  ((hasChanged, msgs), outputState) <-
    runStateT
      ( CPS.runWriterT
          ( or
              <$> sequence
                [ -- Update the state if any changes where parsed.
                  processing
                , -- Check if any local builds have finished, because nix-build would not tell us.
                  -- If we haven‘t done so in the last minTimeBetweenPollingNixStore seconds.
                  check
                ]
          )
      )
      inputState
  -- If any of the update steps returned true, return the new state, otherwise return Nothing.
  let retval = (outputAccessTime, if hasChanged then Just outputState else Nothing)
      errors = lefts msgs
  pure ((errors, input <> ByteString.unlines (rights msgs)), retval)

derivationIsCompleted :: (UpdateMonad m) => DerivationId -> NOMStateT m Bool
derivationIsCompleted drvId =
  derivationToAnyOutPath drvId >>= \case
    Nothing -> pure False -- Derivation has no "out" output.
    Just path -> storePathExists path

detectLocalFinishedBuilds :: ProcessingT m Bool
detectLocalFinishedBuilds = do
  runningLocalBuilds <- CMap.toList <$> getRunningBuildsByHost Localhost
  newCompletedOutputs <- filterM (\(x, _) -> derivationIsCompleted x) runningLocalBuilds
  let anyBuildsFinished = not (null newCompletedOutputs)
  when anyBuildsFinished (finishBuilds Localhost newCompletedOutputs)
  pure anyBuildsFinished

data ProgressUpdate
  = PlanDownloads (Set StorePath)
  | PlanBuilds (Set Derivation)
  | Evaluation
  | Query
  | Other
  | Irrelevant
  deriving stock (Show)

withChange :: ProgressUpdate -> ProcessingT m b -> ProcessingT m Bool
withChange update action = do
  current_state <- gets (.progressState)
  new_progress_state <- case (current_state, update) of
    (Planning store_paths derivations, PlanDownloads new_store_paths) -> pure (Planning (new_store_paths <> store_paths) derivations)
    (Planning store_paths derivations, PlanBuilds new_derivations) -> pure (Planning store_paths (new_derivations <> derivations))
    (_, PlanDownloads store_paths) -> pure (Planning store_paths mempty)
    (_, PlanBuilds derivations) -> pure (Planning mempty derivations)
    (Planning store_paths derivations, Other) -> do
      let store_paths_list = toList store_paths
          derivations_list = toList derivations
      producers <- lookupProducers store_paths_list
      void $ lookupDerivations (derivations_list <> producers)
      planned_build_ids <- forM (toList derivations) (\derivation -> getDerivationId derivation)
      planBuilds (fromList planned_build_ids)
      planned_download_ids <- forM store_paths_list (\x -> getStorePathId x)
      planDownloads (fromList planned_download_ids)
      pure Realising
    (_, Evaluation) -> pure Evaluating
    (Evaluating, Query) -> pure QueryingSubstituers
    _ -> pure current_state
  let change = case (current_state, update) of
        (JustStarted, _) -> True
        (_, Irrelevant) -> False
        _ -> True
  when (change && new_progress_state /= current_state) do
    modify' (gfield @"progressState" .~ new_progress_state)
  void action
  pure change

noChange :: ProcessingT m Bool
noChange = withChange Irrelevant pass

processResult :: (UpdateMonad m) => NixOldStyleMessage -> ProcessingT m Bool
processResult result = do
  now <- getNow
  case result of
    OldStyleMessage.Uploading path host -> withChange Other do
      pathId <- lookupStorePath path
      uploaded host pathId now
    OldStyleMessage.Downloading path host -> withChange Other do
      pathId <- lookupStorePath path
      downloaded host pathId now
      finishBuildByPathId host pathId
    OldStyleMessage.PlanCopies _ -> noChange
    OldStyleMessage.Build drvName host -> withChange Other do
      building host drvName now Nothing
    OldStyleMessage.PlanBuilds planned_builds _lastBuild -> withChange (PlanBuilds planned_builds) pass
    OldStyleMessage.PlanDownloads _download _unpacked planned_downloads -> withChange (PlanDownloads planned_downloads) pass
    OldStyleMessage.Checking drvName -> withChange Other do
      building Localhost drvName now Nothing
    OldStyleMessage.Failed drv code -> withChange Other do
      drvId <- lookupDerivation drv
      failedBuild now drvId code

processJsonMessage :: (UpdateMonad m) => NixJSONMessage -> ProcessingT m Bool
processJsonMessage = \case
  Message MkMessageAction{message, level} | level <= Info && level > Error -> do
    let message' = encodeUtf8 message
    CPS.tell [Right message']
    case parseIndentedStoreObject message of
      Just (Right download) -> withChange (PlanDownloads (one download)) pass
      Just (Left build) -> withChange (PlanBuilds (one build)) pass
      _ -> noChange
  Message MkMessageAction{message, level = Error}
    | stripped <- stripANSICodes message
    , Text.isPrefixOf "error:" stripped ->
        withChange Other do
          errors <- gets (.nixErrors)
          unless (any (Text.isInfixOf (Text.drop 7 stripped) . stripANSICodes) errors) do
            modify' (gfield @"nixErrors" %~ (<> (message Seq.<| mempty)))
            CPS.tell [Right (encodeUtf8 message)]
          whenJust
            (snd <$> parseOneText Parser.oldStyleParser (stripped <> "\n"))
            (\old_style_parse_result -> void $ processResult old_style_parse_result)
  Message MkMessageAction{message} | Just suffix <- Text.stripPrefix "evaluating file '" message -> withChange Evaluation do
    let file_name = Text.dropEnd 1 suffix
    now <- getNow
    modify' (gfield @"evaluationState" %~ \old -> old{count = old.count + 1, lastFileName = Strict.Just file_name, at = now})
  Result MkResultAction{result = BuildLogLine line, id = id'} ->
    do
      nomState <- get
      prefix <- activityPrefix ((.activity) <$> IntMap.lookup id'.value nomState.activities)
      CPS.tell [Right (encodeUtf8 (prefix <> line))]
      noChange
  Result MkResultAction{result = SetPhase phase, id = id'} ->
    withChange Other $ modify' (gfield @"activities" %~ IntMap.adjust (gfield @"phase" .~ Strict.Just phase) id'.value)
  Result MkResultAction{result = Progress progress, id = id'} ->
    withChange Other $ modify' (gfield @"activities" %~ IntMap.adjust (gfield @"progress" .~ Strict.Just progress) id'.value)
  Start startAction@MkStartAction{id = id'} -> do
    prefix <- activityPrefix $ Just startAction.activity
    when (not (Text.null startAction.text) && startAction.level <= Info) $ CPS.tell [Right . encodeUtf8 $ prefix <> startAction.text]
    changed <- case startAction.activity of
      JSON.Build drvName host -> withChange Other do
        now <- getNow
        building host drvName now (Just id')
      JSON.CopyPath path from Localhost -> withChange Other do
        now <- getNow
        pathId <- lookupStorePath path
        downloading from pathId now
      JSON.CopyPath path Localhost to -> withChange Other do
        now <- getNow
        pathId <- lookupStorePath path
        uploading to pathId now
      JSON.Unknown | Text.isPrefixOf "querying info" startAction.text -> withChange Query pass
      JSON.QueryPathInfo{} -> withChange Query pass
      _ -> noChange -- tell [Right (encodeUtf8 (markup yellow "unused activity: " <> show startAction.id <> " " <> show startAction.activity))]
    when changed $ modify' (gfield @"activities" %~ IntMap.insert id'.value (MkActivityStatus startAction.activity Strict.Nothing Strict.Nothing))
    pure changed
  Stop MkStopAction{id = id'} -> do
    activity <- gets (\s -> IntMap.lookup id'.value s.activities)
    case activity of
      Just (MkActivityStatus{activity = JSON.CopyPath path from Localhost}) -> withChange Other do
        now <- getNow
        pathId <- lookupStorePath path
        downloaded from pathId now
      Just (MkActivityStatus{activity = JSON.CopyPath path Localhost to}) -> withChange Other do
        now <- getNow
        pathId <- lookupStorePath path
        uploaded to pathId now
      Just (MkActivityStatus{activity = JSON.Build drv host}) -> do
        drvId <- lookupDerivation drv
        isCompleted <- derivationIsCompleted drvId
        if isCompleted then withChange Other $ finishBuildByDrvId host drvId else noChange
      _ -> noChange
  Plain msg -> CPS.tell [Right msg] >> noChange
  ParseError err -> CPS.tell [Left err] >> noChange
  Result _other_result -> noChange
  Message _other_message -> noChange

-- tell [Right (encodeUtf8 (markup yellow "unused message: " <> show _other))]

appendDifferingPlatform :: NOMV1State -> DerivationInfo -> Text -> Text
appendDifferingPlatform nomState drvInfo = case (nomState.buildPlatform, drvInfo.platform) of
  (Strict.Just p1, Strict.Just p2) | p1 /= p2 -> (<> "-" <> p2)
  _ -> id

activityPrefix :: Maybe Activity -> ProcessingT m Text
activityPrefix activities = case activities of
  Just (JSON.Build derivation _) -> do
    drvInfo <- lookupDerivationInfos derivation
    nomState <- get
    pure $ toText (setSGRCode [Reset]) <> markup blue (appendDifferingPlatform nomState drvInfo (getReportName drvInfo) <> "> ")
  _ -> pure ""

movingAverage :: Double
movingAverage = 0.5

reportFinishingBuilds :: (MonadCacheBuildReports m, MonadNow m) => Host -> NonEmpty (DerivationInfo, Double) -> m BuildReportMap
reportFinishingBuilds host builds = do
  now <- getNow
  updateBuildReports (modifyBuildReports host (timeDiffInt now <<$>> builds))

-- | time difference in seconds rounded down
timeDiffInt :: Double -> Double -> Int
timeDiffInt = fmap floor . (-)

finishBuilds :: Host -> [(DerivationId, BuildInfo ())] -> ProcessingT m ()
finishBuilds host builds = do
  derivationsWithNames <- forM builds \(drvId, buildInfo) ->
    (,buildInfo.start) <$> getDerivationInfos drvId
  ( \case
      Nothing -> pass
      Just finishedBuilds -> do
        newBuildReports <- reportFinishingBuilds host finishedBuilds
        modify' (gfield @"buildReports" .~ newBuildReports)
    )
    $ nonEmpty derivationsWithNames
  now <- getNow
  forM_ builds \(drv, info) -> updateDerivationState drv (const (Built (info $> now)))

modifyBuildReports :: Host -> NonEmpty (DerivationInfo, Int) -> BuildReportMap -> BuildReportMap
modifyBuildReports host = foldMapEndo (uncurry insertBuildReport)
 where
  insertBuildReport name =
    Map.insertWith
      (\new old -> floor (movingAverage * fromIntegral new + (1 - movingAverage) * fromIntegral old))
      (host, getReportName name)

failedBuild :: Double -> DerivationId -> FailType -> NOMState ()
failedBuild now drv code = updateDerivationState drv update
 where
  update = \case
    Built a -> State.Failed (a $> MkBuildFail now code)
    Building a -> State.Failed (a $> MkBuildFail now code)
    x -> x

lookupStorePath :: StorePath -> ProcessingT m StorePathId
lookupStorePath !path = do
  lookupProducers (one path) >>= (\derivations -> void $ lookupDerivations (toList derivations))
  getStorePathId path

lookupProducers :: [StorePath] -> ProcessingT m [Derivation]
lookupProducers paths = do
  storePathIds <- gets (.storePathIds)
  let missing_paths =
        paths
          & filter (\path -> not $ Map.member path storePathIds)
  getProducers missing_paths

lookupDerivations :: [Derivation] -> ProcessingT m [DerivationId]
lookupDerivations derivations = mapM (\derivation -> lookupDerivation derivation) derivations

lookupDerivation :: Derivation -> ProcessingT m DerivationId
lookupDerivation drv = do
  drvId <- getDerivationId drv
  isCached <- gets (maybe False (.cached) . CMap.lookup drvId . (.derivationInfos))
  unless isCached $
    getDerivation drv >>= \case
      Left err -> CPS.tell [Left err]
      Right parsedDrv -> insertDerivation parsedDrv drvId
  pure drvId

lookupDerivationInfos :: Derivation -> ProcessingT m DerivationInfo
lookupDerivationInfos drvName = do
  drvId <- lookupDerivation drvName
  getDerivationInfos drvId

forceList :: [a] -> [a]
forceList [] = []
forceList (!a : rest) = a : forced_rest
 where
  !forced_rest = forceList rest

insertDerivation :: Nix.Derivation FilePath Text -> DerivationId -> ProcessingT m ()
insertDerivation nix_derivation drvId = do
  -- We need to be really careful in this function. The Nix.Derivation keeps the
  -- read-in derivation file in memory. When using Texts from it we must make
  -- sure we destroy sharing with the original file, so that it can be garbage
  -- collected.
  let !unshared_outputs =
        nix_derivation.outputs
          & Map.mapKeys (parseOutputName . Text.copy)
          & Map.mapMaybe (parseStorePath . toText . Nix.path)
      !unshared_inputSrcs =
        nix_derivation.inputSrcs
          & toList
          & mapMaybe (parseStorePath . toText)
          & Set.fromList
      unshared_inputDrvs :: [(Derivation, Set OutputName)]
      !unshared_inputDrvs =
        nix_derivation.inputDrvs
          & Map.toList
          & mapMaybe
            ( \(drvPath, outputs_of_input) ->
                let !unshared_outputs_of_input = Set.map (parseOutputName . Text.copy) outputs_of_input
                 in (,unshared_outputs_of_input) <$> parseDerivation (toText drvPath)
            )
          & forceList
      !unshared_platform = Strict.Just (Text.copy nix_derivation.platform)
      !unshared_pname = Strict.toStrict (Text.copy <$> Map.lookup "pname" nix_derivation.env)

  outputs <-
    unshared_outputs & Map.traverseWithKey \_ pathName -> do
      pathId <- getStorePathId pathName
      modify' (gfield @"storePathInfos" %~ CMap.adjust (gfield @"producer" .~ Strict.Just drvId) pathId)
      pure pathId
  inputSources <-
    unshared_inputSrcs & flip foldlM mempty \acc pathName -> do
      pathId <- getStorePathId pathName
      modify' (gfield @"storePathInfos" %~ CMap.adjust (gfield @"inputFor" %~ CSet.insert drvId) pathId)
      pure $ CSet.insert pathId acc
  inputDerivationsList <-
    unshared_inputDrvs & mapM \(dep_name, outputs_of_input) -> do
      depId <- getDerivationId dep_name
      modify' (gfield @"derivationInfos" %~ CMap.adjust (gfield @"derivationParents" %~ CSet.insert drvId) depId)
      modify' (gfield @"forestRoots" %~ Seq.filter (/= depId))
      pure (MkInputDerivation{derivation = depId, outputs = outputs_of_input})
  let inputDerivations = Seq.fromList inputDerivationsList
  modify'
    ( gfield @"derivationInfos"
        %~ CMap.adjust
          ( \derivation_info ->
              derivation_info
                { outputs
                , inputSources
                , inputDerivations
                , cached = True
                , platform = unshared_platform
                , pname = unshared_pname
                }
          )
          drvId
    )
  noParents <- CSet.null . (.derivationParents) <$> getDerivationInfos drvId
  when noParents $ modify' (gfield @"forestRoots" %~ (drvId Seq.<|))

planBuilds :: Set DerivationId -> NOMState ()
planBuilds drvIds = forM_ drvIds \drvId ->
  updateDerivationState drvId (const Planned)

planDownloads :: Set StorePathId -> NOMState ()
planDownloads pathIds = forM_ pathIds \pathId ->
  insertStorePathState pathId DownloadPlanned Nothing

finishBuildByDrvId :: Host -> DerivationId -> ProcessingT m ()
finishBuildByDrvId host drvId = do
  buildInfoMay <- getBuildInfoIfRunning drvId
  whenJust buildInfoMay \buildInfo -> finishBuilds host [(drvId, buildInfo)]

finishBuildByPathId :: Host -> StorePathId -> ProcessingT m ()
finishBuildByPathId host pathId = do
  drvIdMay <- outPathToDerivation pathId
  whenJust drvIdMay (\x -> finishBuildByDrvId host x)

downloading :: Host -> StorePathId -> Double -> NOMState ()
downloading host pathId start = insertStorePathState pathId (State.Downloading MkTransferInfo{host, start, end = ()}) Nothing

getBuildInfoIfRunning :: DerivationId -> NOMState (Maybe RunningBuildInfo)
getBuildInfoIfRunning drvId =
  runMaybeT $ do
    drvInfos <- MaybeT (gets (CMap.lookup drvId . (.derivationInfos)))
    MaybeT (pure ((() <$) <$> preview (gfield @"buildStatus" % gconstructor @"Building") drvInfos))

downloaded :: Host -> StorePathId -> Double -> NOMState ()
downloaded host pathId end = insertStorePathState pathId (Downloaded MkTransferInfo{host, start = end, end = Strict.Nothing}) $ Just \case
  State.Downloading transfer_info | transfer_info.host == host -> Downloaded (transfer_info $> Strict.Just end)
  other -> other

uploading :: Host -> StorePathId -> Double -> NOMState ()
uploading host pathId start =
  insertStorePathState pathId (State.Uploading MkTransferInfo{host, start, end = ()}) Nothing

uploaded :: Host -> StorePathId -> Double -> NOMState ()
uploaded host pathId end =
  insertStorePathState pathId (Uploaded MkTransferInfo{host, start = end, end = Strict.Nothing}) $ Just \case
    State.Uploading transfer_info | transfer_info.host == host -> Uploaded (transfer_info $> Strict.Just end)
    other -> other

building :: Host -> Derivation -> Double -> Maybe ActivityId -> ProcessingT m ()
building host drvName now activityId = do
  reportName <- getReportName <$> lookupDerivationInfos drvName
  lastNeeded <- Map.lookup (host, reportName) . (.buildReports) <$> get
  drvId <- lookupDerivation drvName
  updateDerivationState drvId (const (Building (MkBuildInfo now host (Strict.toStrict lastNeeded) (Strict.toStrict activityId) ())))

updateDerivationState :: DerivationId -> (BuildStatus -> BuildStatus) -> NOMState ()
updateDerivationState drvId updateStatus = do
  -- Update derivationInfo for this Derivation
  derivation_infos <- getDerivationInfos drvId
  let oldStatus = derivation_infos.buildStatus
      newStatus = updateStatus oldStatus
  when (oldStatus /= newStatus) do
    modify' (gfield @"derivationInfos" %~ CMap.adjust (gfield @"buildStatus" .~ newStatus) drvId)
    let update_summary = updateSummaryForDerivation oldStatus newStatus drvId
        clear_summary = clearDerivationIdFromSummary oldStatus drvId

    -- Update summaries of all parents and sort them
    updateParents False update_summary clear_summary (derivation_infos.derivationParents)

    -- Update fullSummary
    modify' (gfield @"fullSummary" %~ update_summary)

updateParents :: Bool -> (DependencySummary -> DependencySummary) -> (DependencySummary -> DependencySummary) -> DerivationSet -> NOMState ()
updateParents force_direct update_func clear_func direct_parents = do
  relevant_parents <- (if force_direct then CSet.union direct_parents else id) <$> collect_parents True mempty direct_parents
  parents <- collect_parents False mempty direct_parents
  modify'
    ( gfield @"derivationInfos"
        %~ apply_to_all_summaries update_func relevant_parents
          . apply_to_all_summaries clear_func (CSet.difference parents relevant_parents)
    )
  modify' (gfield @"touchedIds" %~ CSet.union parents)
 where
  apply_to_all_summaries ::
    (DependencySummary -> DependencySummary) ->
    DerivationSet ->
    DerivationMap DerivationInfo ->
    DerivationMap DerivationInfo
  apply_to_all_summaries func = foldMapEndo (CMap.adjust (gfield @"dependencySummary" %~ func)) . CSet.toList
  collect_parents :: Bool -> DerivationSet -> DerivationSet -> NOMState DerivationSet
  collect_parents no_irrelevant collected_parents parents_to_scan = case CSet.maxView parents_to_scan of
    Nothing -> pure collected_parents
    Just (current_parent, rest_to_scan) -> do
      drv_infos <- getDerivationInfos current_parent
      transfer_states <- fold <$> forM (Map.lookup Out drv_infos.outputs) (fmap (.states) . \x -> getStorePathInfos x)
      let all_transfers_completed = all (\x -> has (gconstructor @"Downloaded") x || has (gconstructor @"Uploaded") x) transfer_states
          is_irrelevant = all_transfers_completed && has (gconstructor @"Unknown") drv_infos.buildStatus || has (gconstructor @"Built") drv_infos.buildStatus
          proceed = collect_parents no_irrelevant
      if is_irrelevant && no_irrelevant
        then proceed collected_parents rest_to_scan
        else proceed (CSet.insert current_parent collected_parents) (CSet.union (CSet.difference drv_infos.derivationParents collected_parents) rest_to_scan)

updateStorePathStates :: StorePathState -> Maybe (StorePathState -> StorePathState) -> Set StorePathState -> Set StorePathState
updateStorePathStates new_state update_state =
  Set.insert new_state
    . localFilter
    . ( case update_state of
          Just update_func -> Set.fromList . fmap update_func . Set.toList
          Nothing -> id
      )
 where
  localFilter = case new_state of
    DownloadPlanned -> id
    State.Downloading _ -> Set.filter (DownloadPlanned /=)
    Downloaded _ -> Set.filter (DownloadPlanned /=) -- We don‘t need to filter downloading state because that has already been handled by the update_state function
    State.Uploading _ -> id
    Uploaded _ -> id -- Analogous to downloaded

insertStorePathState :: StorePathId -> StorePathState -> Maybe (StorePathState -> StorePathState) -> NOMState ()
insertStorePathState storePathId new_store_path_state update_store_path_state = do
  -- Update storePathInfos for this Storepath
  store_path_info <- getStorePathInfos storePathId
  let oldStatus = store_path_info.states
      newStatus = updateStorePathStates new_store_path_state update_store_path_state oldStatus
  modify' (gfield @"storePathInfos" %~ CMap.adjust (gfield @"states" .~ newStatus) storePathId)

  let update_summary = updateSummaryForStorePath oldStatus newStatus storePathId
      clear_summary = clearStorePathsFromSummary oldStatus storePathId

  -- Update summaries of all parents
  updateParents True update_summary clear_summary (Strict.maybe id CSet.insert store_path_info.producer store_path_info.inputFor)

  -- Update fullSummary
  modify' (gfield @"fullSummary" %~ update_summary)
