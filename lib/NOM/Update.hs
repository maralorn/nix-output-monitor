module NOM.Update (updateStateNixJSONMessage, updateStateNixOldStyleMessage, maintainState, detectLocalFinishedBuilds, appendDifferingPlatform) where

import Control.Monad.Writer (MonadWriter (tell))
import Control.Monad.Writer.Strict (WriterT (runWriterT))
import Data.ByteString.Char8 qualified as ByteString
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
-- optics

import NOM.Builds (Derivation (..), FailType, Host (..), StorePath (..), parseDerivation, parseIndentedStoreObject, parseStorePath)
import NOM.Error (NOMError)
import NOM.IO.ParseStream.Attoparsec (parseOneText, stripANSICodes)
import NOM.NixMessage.JSON (Activity, ActivityId, ActivityResult (..), MessageAction (..), NixJSONMessage (..), ResultAction (..), StartAction (..), StopAction (..), Verbosity (..))
import NOM.NixMessage.JSON qualified as JSON
import NOM.NixMessage.OldStyle (NixOldStyleMessage)
import NOM.NixMessage.OldStyle qualified as OldStyleMessage
import NOM.Parser qualified as Parser
import NOM.Print.Table (blue, markup)
import NOM.State (
  BuildInfo (..),
  BuildStatus (..),
  DependencySummary,
  DerivationId,
  DerivationInfo (..),
  DerivationMap,
  DerivationSet,
  NOMState,
  NOMStateT,
  NOMV1State (..),
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
  updateSummaryForDerivation,
  updateSummaryForStorePath,
 )
import NOM.State qualified as State
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet
import NOM.State.Sorting (sortDepsOfSet, sortKey)
import NOM.Update.Monad (
  BuildReportMap,
  MonadCacheBuildReports (..),
  MonadCheckStorePath (..),
  MonadNow (..),
  MonadReadDerivation (..),
  UpdateMonad,
 )
import NOM.Util (diffTime, foldMapEndo, relTimeToSeconds)
import Nix.Derivation qualified as Nix
import Optics (gconstructor, gfield, has, preview, view, (%), (%~), (.~), (?~), _1, _2, _3)
import Relude
import Streamly.Internal.Data.Time.Units (AbsTime, diffAbsTime)
import System.Console.ANSI (SGR (Reset), setSGRCode)

type ProcessingT m a = UpdateMonad m ⇒ NOMStateT (WriterT [Either NOMError ByteString] m) a

getReportName ∷ DerivationInfo → Text
getReportName drv = case drv.pname of
  Just pname → pname
  Nothing → Text.dropWhileEnd (`Set.member` fromList ".1234567890-") drv.name.storePath.name

setInputReceived ∷ NOMState Bool
setInputReceived = do
  s ← get
  let change = s.progressState == JustStarted
  when change (put s{progressState = InputReceived})
  pure change

maintainState ∷ NOMV1State → NOMV1State
maintainState = execState $ do
  currentState@MkNOMV1State{touchedIds} ← get
  unless (CSet.null touchedIds) $ do
    sortDepsOfSet touchedIds
    modify (gfield @"forestRoots" %~ Seq.sortOn (sortKey currentState))
    modify (gfield @"touchedIds" .~ mempty)

minTimeBetweenPollingNixStore ∷ NominalDiffTime
minTimeBetweenPollingNixStore = 0.2 -- in seconds

{-# INLINE updateStateNixJSONMessage #-}
updateStateNixJSONMessage ∷ ∀ m. UpdateMonad m ⇒ Either NOMError NixJSONMessage → NOMV1State → m (([NOMError], ByteString), Maybe NOMV1State)
updateStateNixJSONMessage input inputState =
  {-# SCC "updateStateNixJSONMessage" #-}
  do
    let process =
          {-# SCC "matching_message" #-}
          case input of
            Left err → do
              tell [Left err]
              noChange
            Right jsonMessage → processJsonMessage jsonMessage
    ((hasChanged, msgs), !outputState) ← {-# SCC "run_state" #-} runStateT (runWriterT (({-# SCC "input_received" #-} setInputReceived) >> {-# SCC "processing" #-} process)) inputState
    let retval = if hasChanged then Just outputState else Nothing
        errors = lefts msgs
    {-# SCC "emitting_new_state" #-} pure ((errors, ByteString.unlines (rights msgs)), retval)

updateStateNixOldStyleMessage ∷ ∀ m. UpdateMonad m ⇒ (Maybe NixOldStyleMessage, ByteString) → (Maybe AbsTime, NOMV1State) → m (([NOMError], ByteString), (Maybe AbsTime, Maybe NOMV1State))
updateStateNixOldStyleMessage (result, input) (inputAccessTime, inputState) = do
  now ← getNow

  let processing = case result of
        Just result' → processResult result'
        Nothing → pure False
      (outputAccessTime, check)
        | maybe True ((>= minTimeBetweenPollingNixStore) . diffTime now) inputAccessTime = (Just now, detectLocalFinishedBuilds)
        | otherwise = (inputAccessTime, pure False)
  ((!hasChanged, !msgs), outputState) ←
    runStateT
      ( runWriterT
          ( or
              <$> sequence
                [ -- First check if this is the first time that we receive input (for error messages).
                  setInputReceived
                , -- Update the state if any changes where parsed.
                  processing
                , -- Check if any local builds have finished, because nix-build would not tell us.
                  -- If we haven‘t done so in the last 200ms.
                  check
                ]
          )
      )
      inputState
  -- If any of the update steps returned true, return the new state, otherwise return Nothing.
  let retval = (outputAccessTime, if hasChanged then Just outputState else Nothing)
      errors = lefts msgs
  pure ((errors, input <> ByteString.unlines (rights msgs)), retval)

derivationIsCompleted ∷ UpdateMonad m ⇒ DerivationId → NOMStateT m Bool
derivationIsCompleted drvId =
  derivationToAnyOutPath drvId >>= \case
    Nothing → pure False -- Derivation has no "out" output.
    Just path → storePathExists path

detectLocalFinishedBuilds ∷ ProcessingT m Bool
detectLocalFinishedBuilds = do
  runningLocalBuilds ← CMap.toList <$> getRunningBuildsByHost Localhost -- .> traceShowId
  newCompletedOutputs ← filterM (\(x, _) → derivationIsCompleted x) runningLocalBuilds
  let anyBuildsFinished = not (null newCompletedOutputs)
  when anyBuildsFinished (finishBuilds Localhost newCompletedOutputs)
  pure anyBuildsFinished

withChange ∷ Functor f ⇒ f b → f Bool
withChange = (True <$)

noChange ∷ Applicative f ⇒ f Bool
noChange = pure False

processResult ∷ UpdateMonad m ⇒ NixOldStyleMessage → ProcessingT m Bool
processResult result = do
  now ← getNow
  case result of
    OldStyleMessage.Uploading path host → withChange do
      pathId ← getStorePathId path
      uploaded host pathId now
    OldStyleMessage.Downloading path host → withChange do
      pathId ← getStorePathId path
      downloaded host pathId now
      finishBuildByPathId host pathId
    OldStyleMessage.PlanCopies _ → noChange
    OldStyleMessage.Build drvName host → withChange do
      building host drvName now Nothing
    OldStyleMessage.PlanBuilds plannedBuilds _lastBuild → withChange do
      plannedDrvIds ← forM (toList plannedBuilds) \drv →
        lookupDerivation drv
      planBuilds (fromList plannedDrvIds)
    OldStyleMessage.PlanDownloads _download _unpacked plannedDownloads → withChange do
      plannedDownloadIds ← forM (toList plannedDownloads) \path →
        getStorePathId path
      planDownloads (fromList plannedDownloadIds)
    OldStyleMessage.Checking drvName → withChange do
      building Localhost drvName now Nothing
    OldStyleMessage.Failed drv code → withChange do
      drvId ← lookupDerivation drv
      failedBuild now drvId code

processJsonMessage ∷ UpdateMonad m ⇒ NixJSONMessage → ProcessingT m Bool
processJsonMessage = \case
  Message MkMessageAction{message, level} | level <= Info && level > Error → do
    let message' = encodeUtf8 message
    tell [Right message']
    case parseIndentedStoreObject message of
      Just (Right download) →
        {-# SCC "plan_download" #-}
        withChange do
          plannedDownloadId ← getStorePathId download
          planDownloads $ one plannedDownloadId
      Just (Left build) →
        {-# SCC "plan_build" #-}
        withChange do
          plannedDrvId ← lookupDerivation build
          planBuilds (one plannedDrvId)
      _ → noChange
  Message MkMessageAction{message, level = Error}
    | stripped ← stripANSICodes message
    , Text.isPrefixOf "error:" stripped →
        {-# SCC "pass_through_error" #-}
        withChange do
          errors ← gets (.nixErrors)
          unless (stripped `elem` errors) do
            modify' (gfield @"nixErrors" %~ (<> [stripped]))
            whenJust (parseOneText Parser.oldStyleParser message) \result →
              void (processResult result)
            tell [Right (encodeUtf8 message)]
    | stripped ← stripANSICodes message
    , Text.isPrefixOf "note:" stripped →
        {-# SCC "pass_through_note" #-}
        do
          tell [Right (encodeUtf8 message)]
          noChange
  Result MkResultAction{result = BuildLogLine line, id = id'} →
    {-# SCC "pass_through_build_line" #-}
    do
      nomState ← get
      prefix ← activityPrefix (view _1 <$> IntMap.lookup id'.value nomState.activities)
      tell [Right (encodeUtf8 (prefix <> line))]
      noChange
  Result MkResultAction{result = SetPhase phase, id = id'} →
    {-# SCC "updating_phase" #-} withChange $ modify' (gfield @"activities" %~ IntMap.adjust (_2 ?~ phase) id'.value)
  Result MkResultAction{result = Progress progress, id = id'} →
    {-# SCC "updating_progress" #-} withChange $ modify' (gfield @"activities" %~ IntMap.adjust (_3 ?~ progress) id'.value)
  Start startAction@MkStartAction{id = id'} →
    {-# SCC "starting_action" #-}
    withChange do
      prefix ← activityPrefix $ Just startAction.activity
      when (not (Text.null startAction.text) && startAction.level <= Info) $ tell [Right . encodeUtf8 $ prefix <> startAction.text]
      modify' (gfield @"activities" %~ IntMap.insert id'.value (startAction.activity, Nothing, Nothing))
      case startAction.activity of
        JSON.Build drvName host → do
          now ← getNow
          building host drvName now (Just id')
        JSON.CopyPath path from Localhost → do
          now ← getNow
          pathId ← getStorePathId path
          downloading from pathId now
        JSON.CopyPath path Localhost to → do
          now ← getNow
          pathId ← getStorePathId path
          uploading to pathId now
        _ → pass -- tell [Right (encodeUtf8 (markup yellow "unused activity: " <> show startAction.id <> " " <> show startAction.activity))]
  Stop MkStopAction{id = id'} →
    {-# SCC "stoping_action" #-}
    do
      activity ← gets (\s → IntMap.lookup id'.value s.activities)
      case activity of
        Just (JSON.CopyPath path from Localhost, _, _) → withChange do
          now ← getNow
          pathId ← getStorePathId path
          downloaded from pathId now
        Just (JSON.CopyPath path Localhost to, _, _) → withChange do
          now ← getNow
          pathId ← getStorePathId path
          uploaded to pathId now
        Just (JSON.Build drv host, _, _) → do
          drvId ← lookupDerivation drv
          isCompleted ← derivationIsCompleted drvId
          if isCompleted then withChange $ finishBuildByDrvId host drvId else noChange
        _ → noChange
  _other → do
    -- tell [Right (encodeUtf8 (markup yellow "unused message: " <> show _other))]
    noChange

appendDifferingPlatform ∷ NOMV1State → DerivationInfo → Text → Text
appendDifferingPlatform nomState drvInfo = case (nomState.buildPlatform, drvInfo.platform) of
  (Just p1, Just p2) | p1 /= p2 → (<> "-" <> p2)
  _ → id

activityPrefix ∷ Maybe Activity → ProcessingT m Text
activityPrefix activities = do
  case activities of
    Just (JSON.Build derivation _) → do
      drvInfo ← lookupDerivationInfos derivation
      nomState ← get
      pure $ toText (setSGRCode [Reset]) <> markup blue (appendDifferingPlatform nomState drvInfo (getReportName drvInfo) <> "> ")
    _ → pure ""

movingAverage ∷ Double
movingAverage = 0.5

reportFinishingBuilds ∷ (MonadCacheBuildReports m, MonadNow m) ⇒ Host → NonEmpty (DerivationInfo, AbsTime) → m BuildReportMap
reportFinishingBuilds host builds = do
  now ← getNow
  updateBuildReports (modifyBuildReports host (timeDiffInt now <<$>> builds))

-- | time difference in seconds rounded down
timeDiffInt ∷ AbsTime → AbsTime → Int
timeDiffInt = fmap (floor . relTimeToSeconds) . diffAbsTime

finishBuilds ∷ Host → [(DerivationId, BuildInfo ())] → ProcessingT m ()
finishBuilds host builds = do
  derivationsWithNames ← forM builds \(drvId, buildInfo) →
    (,buildInfo.start) <$> getDerivationInfos drvId
  ( \case
      Nothing → pass
      Just finishedBuilds → do
        newBuildReports ← reportFinishingBuilds host finishedBuilds
        modify (gfield @"buildReports" .~ newBuildReports)
    )
    $ nonEmpty derivationsWithNames
  now ← getNow
  forM_ builds \(drv, info) → updateDerivationState drv (const (Built (info $> now)))

modifyBuildReports ∷ Host → NonEmpty (DerivationInfo, Int) → BuildReportMap → BuildReportMap
modifyBuildReports host = foldMapEndo (uncurry insertBuildReport)
 where
  insertBuildReport name =
    Map.insertWith
      (\new old → floor (movingAverage * fromIntegral new + (1 - movingAverage) * fromIntegral old))
      (host, getReportName name)

failedBuild ∷ AbsTime → DerivationId → FailType → NOMState ()
failedBuild now drv code = updateDerivationState drv update
 where
  update = \case
    Built a → State.Failed (a $> (now, code))
    Building a → State.Failed (a $> (now, code))
    x → x

lookupDerivation ∷ Derivation → ProcessingT m DerivationId
lookupDerivation drv = do
  drvId ← getDerivationId drv
  isCached ← gets (maybe False (.cached) . CMap.lookup drvId . (.derivationInfos))
  unless isCached $
    getDerivation drv >>= \case
      Left err → tell [Left err]
      Right parsedDrv → insertDerivation parsedDrv drvId
  pure drvId

lookupDerivationInfos ∷ Derivation → ProcessingT m DerivationInfo
lookupDerivationInfos drvName = do
  drvId ← lookupDerivation drvName
  getDerivationInfos drvId

insertDerivation ∷ Nix.Derivation FilePath Text → DerivationId → ProcessingT m ()
insertDerivation derivation drvId = do
  outputs' ←
    derivation.outputs & Map.traverseMaybeWithKey \_ path → do
      parseStorePath (toText (Nix.path path)) & mapM \pathName → do
        pathId ← getStorePathId pathName
        modify (gfield @"storePathInfos" %~ CMap.adjust (gfield @"producer" ?~ drvId) pathId)
        pure pathId
  inputSources ←
    derivation.inputSrcs & flip foldlM mempty \acc path → do
      pathIdMay ←
        parseStorePath (toText path) & mapM \pathName → do
          pathId ← getStorePathId pathName
          modify (gfield @"storePathInfos" %~ CMap.adjust (gfield @"inputFor" %~ CSet.insert drvId) pathId)
          pure pathId
      pure $ maybe id CSet.insert pathIdMay acc
  inputDerivationsList ←
    derivation.inputDrvs & Map.toList & mapMaybeM \(drvPath, outs) → do
      depIdMay ←
        parseDerivation (toText drvPath) & mapM \depName → do
          depId ← lookupDerivation depName
          modify (gfield @"derivationInfos" %~ CMap.adjust (gfield @"derivationParents" %~ CSet.insert drvId) depId)
          modify (gfield @"forestRoots" %~ Seq.filter (/= depId))
          pure depId
      pure $ (,outs) <$> depIdMay
  let inputDerivations = Seq.fromList inputDerivationsList
  modify (gfield @"derivationInfos" %~ CMap.adjust (\i → i{outputs = outputs', inputSources, inputDerivations, cached = True, platform = Just derivation.platform, pname = Map.lookup "pname" derivation.env}) drvId)
  noParents ← CSet.null . (.derivationParents) <$> getDerivationInfos drvId
  when noParents $ modify (gfield @"forestRoots" %~ (drvId Seq.<|))

planBuilds ∷ Set DerivationId → NOMState ()
planBuilds drvIds = forM_ drvIds \drvId →
  updateDerivationState drvId (const Planned)

planDownloads ∷ Set StorePathId → NOMState ()
planDownloads pathIds = forM_ pathIds \pathId →
  insertStorePathState pathId DownloadPlanned Nothing

finishBuildByDrvId ∷ Host → DerivationId → ProcessingT m ()
finishBuildByDrvId host drvId = do
  buildInfoMay ← getBuildInfoIfRunning drvId
  whenJust buildInfoMay \buildInfo → finishBuilds host [(drvId, buildInfo)]

finishBuildByPathId ∷ Host → StorePathId → ProcessingT m ()
finishBuildByPathId host pathId = do
  drvIdMay ← outPathToDerivation pathId
  whenJust drvIdMay (\x → finishBuildByDrvId host x)

downloading ∷ Host → StorePathId → AbsTime → NOMState ()
downloading host pathId start = do
  insertStorePathState pathId (State.Downloading MkTransferInfo{host, start, end = ()}) Nothing

getBuildInfoIfRunning ∷ DerivationId → NOMState (Maybe RunningBuildInfo)
getBuildInfoIfRunning drvId =
  runMaybeT $ do
    drvInfos ← MaybeT (gets (CMap.lookup drvId . (.derivationInfos)))
    MaybeT (pure ((() <$) <$> preview (gfield @"buildStatus" % gconstructor @"Building") drvInfos))

downloaded ∷ Host → StorePathId → AbsTime → NOMState ()
downloaded host pathId end = do
  insertStorePathState pathId (Downloaded MkTransferInfo{host, start = end, end = Nothing}) $ Just \case
    State.Downloading transfer_info | transfer_info.host == host → Downloaded (transfer_info $> Just end)
    other → other

uploading ∷ Host → StorePathId → AbsTime → NOMState ()
uploading host pathId start =
  insertStorePathState pathId (State.Uploading MkTransferInfo{host, start, end = ()}) Nothing

uploaded ∷ Host → StorePathId → AbsTime → NOMState ()
uploaded host pathId end =
  insertStorePathState pathId (Uploaded MkTransferInfo{host, start = end, end = Nothing}) $ Just \case
    State.Uploading transfer_info | transfer_info.host == host → Uploaded (transfer_info $> Just end)
    other → other

building ∷ Host → Derivation → AbsTime → Maybe ActivityId → ProcessingT m ()
building host drvName now activityId = do
  reportName ← getReportName <$> lookupDerivationInfos drvName
  lastNeeded ← Map.lookup (host, reportName) . (.buildReports) <$> get
  drvId ← lookupDerivation drvName
  updateDerivationState drvId (const (Building (MkBuildInfo now host lastNeeded activityId ())))

updateDerivationState ∷ DerivationId → (BuildStatus → BuildStatus) → NOMState ()
updateDerivationState drvId updateStatus = do
  -- Update derivationInfo for this Derivation
  derivation_infos ← getDerivationInfos drvId
  let oldStatus = derivation_infos.buildStatus
      newStatus = updateStatus oldStatus
  when (oldStatus /= newStatus) do
    modify (gfield @"derivationInfos" %~ CMap.adjust (gfield @"buildStatus" .~ newStatus) drvId)
    let update_summary = updateSummaryForDerivation oldStatus newStatus drvId
        clear_summary = clearDerivationIdFromSummary oldStatus drvId

    -- Update summaries of all parents and sort them
    updateParents False update_summary clear_summary (derivation_infos.derivationParents)

    -- Update fullSummary
    modify (gfield @"fullSummary" %~ update_summary)

updateParents ∷ Bool → (DependencySummary → DependencySummary) → (DependencySummary → DependencySummary) → DerivationSet → NOMState ()
updateParents force_direct update_func clear_func direct_parents = do
  relevant_parents ← (if force_direct then CSet.union direct_parents else id) <$> collect_parents True mempty direct_parents
  parents ← collect_parents False mempty direct_parents
  modify
    ( gfield @"derivationInfos"
        %~ apply_to_all_summaries update_func relevant_parents
          . apply_to_all_summaries clear_func (CSet.difference parents relevant_parents)
    )
  modify (gfield @"touchedIds" %~ CSet.union parents)
 where
  apply_to_all_summaries ∷
    (DependencySummary → DependencySummary) →
    DerivationSet →
    DerivationMap DerivationInfo →
    DerivationMap DerivationInfo
  apply_to_all_summaries func = foldMapEndo (CMap.adjust (gfield @"dependencySummary" %~ func)) . CSet.toList
  collect_parents ∷ Bool → DerivationSet → DerivationSet → NOMState DerivationSet
  collect_parents no_irrelevant collected_parents parents_to_scan = case CSet.maxView parents_to_scan of
    Nothing → pure collected_parents
    Just (current_parent, rest_to_scan) → do
      drv_infos ← getDerivationInfos current_parent
      transfer_states ← fold <$> forM (Map.lookup "out" drv_infos.outputs) (fmap (.states) . \x → getStorePathInfos x)
      let all_transfers_completed = all (\x → has (gconstructor @"Downloaded") x || has (gconstructor @"Uploaded") x) transfer_states
          is_irrelevant = all_transfers_completed && has (gconstructor @"Unknown") drv_infos.buildStatus || has (gconstructor @"Built") drv_infos.buildStatus
          proceed = collect_parents no_irrelevant
      if is_irrelevant && no_irrelevant
        then proceed collected_parents rest_to_scan
        else proceed (CSet.insert current_parent collected_parents) (CSet.union (CSet.difference drv_infos.derivationParents collected_parents) rest_to_scan)

updateStorePathStates ∷ StorePathState → Maybe (StorePathState → StorePathState) → Set StorePathState → Set StorePathState
updateStorePathStates new_state update_state =
  Set.insert new_state
    . localFilter
    . ( case update_state of
          Just update_func → Set.fromList . fmap update_func . Set.toList
          Nothing → id
      )
 where
  localFilter = case new_state of
    DownloadPlanned → id
    State.Downloading _ → Set.filter (DownloadPlanned /=)
    Downloaded _ → Set.filter (DownloadPlanned /=) -- We don‘t need to filter downloading state because that has already been handled by the update_state function
    State.Uploading _ → id
    Uploaded _ → id -- Analogous to downloaded

insertStorePathState ∷ StorePathId → StorePathState → Maybe (StorePathState → StorePathState) → NOMState ()
insertStorePathState storePathId new_store_path_state update_store_path_state = do
  -- Update storePathInfos for this Storepath
  store_path_info ← getStorePathInfos storePathId
  let oldStatus = store_path_info.states
      newStatus = updateStorePathStates new_store_path_state update_store_path_state oldStatus
  modify (gfield @"storePathInfos" %~ CMap.adjust (gfield @"states" .~ newStatus) storePathId)

  let update_summary = updateSummaryForStorePath oldStatus newStatus storePathId
      clear_summary = clearStorePathsFromSummary oldStatus storePathId

  -- Update summaries of all parents
  updateParents True update_summary clear_summary (maybe id CSet.insert store_path_info.producer store_path_info.inputFor)

  -- Update fullSummary
  modify (gfield @"fullSummary" %~ update_summary)
