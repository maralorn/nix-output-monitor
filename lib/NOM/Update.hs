module NOM.Update (updateState, maintainState, detectLocalFinishedBuilds) where

import Relude

import Control.Monad.Writer (MonadWriter (tell))
import Control.Monad.Writer.Strict (WriterT (runWriterT))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

-- optics
import Data.Generics.Product (field, typed)
import Data.Generics.Sum (_As)
import Optics (preview, (%), (%~), (.~), (?~))

import Nix.Derivation qualified as Nix

import NOM.Builds (Derivation (..), FailType, Host (..), StorePath (..))
import NOM.Error (NOMError)
import NOM.Parser (ParseResult (..), parseDerivation, parseStorePath)
import NOM.Parser qualified as Parser
import NOM.State (
  BuildInfo (..),
  BuildStatus (..),
  DependencySummary,
  DerivationId,
  DerivationInfo (..),
  DerivationSet,
  NOMState,
  NOMStateT,
  NOMV1State (..),
  ProcessState (..),
  RunningBuildInfo,
  StorePathId,
  StorePathState (..),
  drv2out,
  getDerivationId,
  getDerivationInfos,
  getRunningBuildsByHost,
  getStorePathId,
  getStorePathInfos,
  lookupDerivationId,
  out2drv,
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
import NOM.Util (foldMapEndo, (.>), (<.>>), (<|>>), (|>))

getReportName :: Derivation -> Text
getReportName drv = Text.dropWhileEnd (`Set.member` fromList ".1234567890-") drv.storePath.name

setInputReceived :: NOMState Bool
setInputReceived = do
  s <- get
  let change = s.processState == JustStarted
  when change (put s{processState = InputReceived})
  pure change

maintainState :: NOMV1State -> NOMV1State
maintainState = execState $ do
  currentState@MkNOMV1State{touchedIds} <- get
  unless (CSet.null touchedIds) $ do
    sortDepsOfSet touchedIds
    modify (field @"forestRoots" %~ Seq.sortOn (sortKey currentState))
    modify (field @"touchedIds" .~ mempty)

minTimeBetweenPollingNixStore :: NominalDiffTime
minTimeBetweenPollingNixStore = 0.2 -- in seconds

updateState :: forall m. UpdateMonad m => (Maybe ParseResult, ByteString) -> (Maybe UTCTime, NOMV1State) -> m (([NOMError], ByteString), (Maybe UTCTime, Maybe NOMV1State))
updateState (result, input) (inputAccessTime, inputState) = do
  now <- getNow

  let (processing, printOutput) = case result of
        Just result'@(JSONMessage _) -> (processResult result', False)
        Just result' -> (processResult result', True)
        Nothing -> (pure False, True)
      (outputAccessTime, check)
        | maybe True (diffUTCTime now .> (>= minTimeBetweenPollingNixStore)) inputAccessTime = (Just now, detectLocalFinishedBuilds)
        | otherwise = (inputAccessTime, pure False)
  ((!hasChanged, !errors), outputState) <-
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
  deepseq retval (pure ((errors, if printOutput then input else ""), retval))

detectLocalFinishedBuilds :: UpdateMonad m => NOMStateT m Bool
detectLocalFinishedBuilds = do
  runningLocalBuilds <- getRunningBuildsByHost Localhost <|>> CMap.toList -- .> traceShowId
  let isCompleted :: UpdateMonad m => (DerivationId, b) -> NOMStateT m Bool
      isCompleted (drvId, _) =
        drv2out drvId >>= \case
          Nothing -> pure False -- Derivation has no "out" output.
          Just path -> storePathExists path
  newCompletedOutputs <- filterM (\x -> isCompleted x) runningLocalBuilds
  let anyBuildsFinished = not (null newCompletedOutputs)
  when anyBuildsFinished (finishBuilds Localhost newCompletedOutputs)
  pure anyBuildsFinished

processResult :: UpdateMonad m => ParseResult -> NOMStateT (WriterT [NOMError] m) Bool
processResult result = do
  let withChange :: Functor f => f b -> f Bool
      withChange = (True <$)
      noChange :: Applicative f => f Bool
      noChange = pure False
  now <- getNow
  case result of
    Parser.Uploading path host -> withChange do
      pathId <- getStorePathId path
      uploaded host pathId
    Parser.Downloading path host -> withChange do
      pathId <- getStorePathId path
      finishedRemoteBuild <- downloaded host pathId
      whenJust finishedRemoteBuild \build -> finishBuilds host [build]
    PlanCopies _ -> noChange
    Build drv host -> withChange do
      drvId <- lookupDerivation drv
      building host drvId now
    PlanBuilds plannedBuilds _lastBuild -> withChange do
      plannedDrvIds <- forM (toList plannedBuilds) \drv ->
        lookupDerivation drv
      planBuilds (fromList plannedDrvIds)
    PlanDownloads _download _unpacked plannedDownloads -> withChange do
      plannedDownloadIds <- forM (toList plannedDownloads) \path ->
        getStorePathId path
      planDownloads (fromList plannedDownloadIds)
    Checking drv -> withChange do
      drvId <- lookupDerivation drv
      building Localhost drvId now
    Parser.Failed drv code -> withChange do
      drvId <- lookupDerivation drv
      failedBuild now drvId code
    JSONMessage msg -> case msg of
      Left err -> withChange $ tell [err]
      Right _ -> withChange pass

movingAverage :: Double
movingAverage = 0.5

reportFinishingBuilds :: (MonadCacheBuildReports m, MonadNow m) => Host -> NonEmpty (Derivation, UTCTime) -> m BuildReportMap
reportFinishingBuilds host builds = do
  now <- getNow
  updateBuildReports (modifyBuildReports host (timeDiffInt now <<$>> builds))

timeDiffInt :: UTCTime -> UTCTime -> Int
timeDiffInt = diffUTCTime <.>> floor

finishBuilds :: (MonadCacheBuildReports m, MonadNow m) => Host -> [(DerivationId, BuildInfo ())] -> NOMStateT m ()
finishBuilds host builds = do
  derivationsWithNames <- forM builds \(drvId, buildInfo) ->
    lookupDerivationId drvId <|>> (,buildInfo.start)
  nonEmpty derivationsWithNames |> \case
    Nothing -> pass
    Just finishedBuilds -> do
      newBuildReports <- reportFinishingBuilds host finishedBuilds
      modify (field @"buildReports" .~ newBuildReports)
  now <- getNow
  forM_ builds \(drv, info) -> updateDerivationState drv (const (Built (info $> now)))

modifyBuildReports :: Host -> NonEmpty (Derivation, Int) -> BuildReportMap -> BuildReportMap
modifyBuildReports host = foldMapEndo (uncurry insertBuildReport)
 where
  insertBuildReport name =
    Map.insertWith
      (\new old -> floor (movingAverage * fromIntegral new + (1 - movingAverage) * fromIntegral old))
      (host, getReportName name)

failedBuild :: UTCTime -> DerivationId -> FailType -> NOMState ()
failedBuild now drv code = updateDerivationState drv update
 where
  update = \case
    Building a -> State.Failed (a $> (now, code))
    x -> x

lookupDerivation :: MonadReadDerivation m => Derivation -> NOMStateT (WriterT [NOMError] m) DerivationId
lookupDerivation drv = do
  drvId <- getDerivationId drv
  isCached <- gets ((.derivationInfos) .> CMap.lookup drvId .> maybe False (.cached))
  unless isCached $
    getDerivation drv >>= \case
      Left err -> tell [err]
      Right parsedDrv -> insertDerivation parsedDrv drvId
  pure drvId

insertDerivation :: MonadReadDerivation m => Nix.Derivation FilePath Text -> DerivationId -> NOMStateT (WriterT [NOMError] m) ()
insertDerivation Nix.Derivation{outputs, inputSrcs, inputDrvs} drvId = do
  outputs' <-
    outputs |> Map.traverseMaybeWithKey \_ path -> do
      parseStorePath (Nix.path path) |> mapM \pathName -> do
        pathId <- getStorePathId pathName
        modify (field @"storePathInfos" %~ CMap.adjust (field @"producer" ?~ drvId) pathId)
        pure pathId
  inputSources <-
    inputSrcs |> flip foldlM mempty \acc path -> do
      pathIdMay <-
        parseStorePath path |> mapM \pathName -> do
          pathId <- getStorePathId pathName
          modify (field @"storePathInfos" %~ CMap.adjust (field @"inputFor" %~ CSet.insert drvId) pathId)
          pure pathId
      pure $ maybe id CSet.insert pathIdMay acc
  inputDerivationsList <-
    inputDrvs |> Map.toList .> mapMaybeM \(drvPath, outs) -> do
      depIdMay <-
        parseDerivation drvPath |> mapM \depName -> do
          depId <- lookupDerivation depName
          modify (field @"derivationInfos" %~ CMap.adjust (field @"derivationParents" %~ CSet.insert drvId) depId)
          modify (field @"forestRoots" %~ Seq.filter (/= depId))
          pure depId
      pure $ depIdMay <|>> (,outs)
  let inputDerivations = Seq.fromList inputDerivationsList
  modify (field @"derivationInfos" %~ CMap.adjust (\i -> i{outputs = outputs', inputSources, inputDerivations, cached = True}) drvId)
  noParents <- getDerivationInfos drvId <|>> (.derivationParents) .> CSet.null
  when noParents $ modify (field @"forestRoots" %~ (drvId Seq.<|))

planBuilds :: Set DerivationId -> NOMState ()
planBuilds drvIds = forM_ drvIds \drvId ->
  updateDerivationState drvId (const Planned)

planDownloads :: Set StorePathId -> NOMState ()
planDownloads pathIds = forM_ pathIds \pathId ->
  insertStorePathState pathId DownloadPlanned

downloaded :: Host -> StorePathId -> NOMState (Maybe (DerivationId, RunningBuildInfo))
downloaded host pathId = do
  insertStorePathState pathId (Downloaded host)
  runMaybeT $ do
    drvId <- MaybeT (out2drv pathId)
    drvInfos <- MaybeT (gets ((.derivationInfos) .> CMap.lookup drvId))
    MaybeT (pure (preview (typed @BuildStatus % _As @"Building") drvInfos <|>> (drvId,)))

uploaded :: Host -> StorePathId -> NOMState ()
uploaded host pathId = insertStorePathState pathId (Uploaded host)

building :: Host -> DerivationId -> UTCTime -> NOMState ()
building host drv now = do
  drvName <- lookupDerivationId drv
  lastNeeded <- get <|>> (.buildReports) .> Map.lookup (host, getReportName drvName)
  updateDerivationState drv (const (Building (MkBuildInfo now host lastNeeded ())))

updateDerivationState :: DerivationId -> (BuildStatus -> BuildStatus) -> NOMState ()
updateDerivationState drvId updateStatus = do
  -- Update derivationInfo for this Derivation
  derivation_infos <- getDerivationInfos drvId
  let oldStatus = derivation_infos.buildStatus
      newStatus = updateStatus oldStatus
  modify (field @"derivationInfos" %~ CMap.adjust (field @"buildStatus" .~ newStatus) drvId)

  let update_summary = updateSummaryForDerivation oldStatus newStatus drvId

  -- Update summaries of all parents and sort them
  updateParents update_summary (derivation_infos.derivationParents)

  -- Update fullSummary
  modify (field @"fullSummary" %~ update_summary)

updateParents :: (DependencySummary -> DependencySummary) -> DerivationSet -> NOMState ()
updateParents update_func = go mempty
 where
  go :: DerivationSet -> DerivationSet -> NOMState ()
  go updated_parents parentsToUpdate = case CSet.maxView parentsToUpdate of
    Nothing -> modify (field @"touchedIds" %~ CSet.union updated_parents)
    Just (parentToUpdate, restToUpdate) -> do
      modify (field @"derivationInfos" %~ CMap.adjust (field @"dependencySummary" %~ update_func) parentToUpdate)
      next_parents <- getDerivationInfos parentToUpdate <|>> (.derivationParents)
      go (CSet.insert parentToUpdate updated_parents) (CSet.union (CSet.difference next_parents updated_parents) restToUpdate)

updateStorePathStates :: StorePathState -> Set StorePathState -> Set StorePathState
updateStorePathStates newState = localFilter .> Set.insert newState
 where
  localFilter = case newState of
    DownloadPlanned -> id
    State.Downloading _ -> Set.filter (DownloadPlanned /=)
    Downloaded h -> Set.filter (State.Downloading h /=) .> Set.filter (DownloadPlanned /=)
    State.Uploading _ -> id
    Uploaded h -> Set.filter (State.Uploading h /=)

insertStorePathState :: StorePathId -> StorePathState -> NOMState ()
insertStorePathState storePathId newStorePathState = do
  -- Update storePathInfos for this Storepath
  store_path_info <- getStorePathInfos storePathId
  let oldStatus = store_path_info.states
      newStatus = updateStorePathStates newStorePathState oldStatus
  modify (field @"storePathInfos" %~ CMap.adjust (field @"states" .~ newStatus) storePathId)

  let update_summary = updateSummaryForStorePath oldStatus newStatus storePathId

  -- Update summaries of all parents
  updateParents update_summary (maybe id CSet.insert store_path_info.producer store_path_info.inputFor)

  -- Update fullSummary
  modify (field @"fullSummary" %~ update_summary)
