module NOM.Update (updateState, maintainState, detectLocalFinishedBuilds) where

import Relude

import Control.Monad.Writer (MonadWriter (tell))
import Control.Monad.Writer.Strict (WriterT (runWriterT))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Data.ByteString.Char8 qualified as ByteString
import Data.IntMap qualified as IntMap

-- optics
import Data.Generics.Product (field, typed)
import Data.Generics.Sum (_As)
import Optics (preview, (%), (%~), (.~), (?~), _1, _2, view)

import Nix.Derivation qualified as Nix

import NOM.Builds (Derivation (..), FailType, Host (..), StorePath (..))
import NOM.Error (NOMError)
import NOM.Parser (ParseResult (..), parseDerivation, parseStorePath, updateParser)
import NOM.Parser.JSON(InternalJson(..), MessageAction (..), ResultAction (..), ActivityResult (..), StartAction (..), ActivityId(..), Verbosity (..), Activity, StopAction (..))
import NOM.Parser.JSON qualified as JSON
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
  TransferInfo(..),
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
import NOM.Print.Table (markup, blue)
import NOM.IO.ParseStream (parseOneText, stripANSICodes)
import System.Console.ANSI (setSGRCode, SGR (Reset))
import Data.Attoparsec.ByteString qualified as Attoparsec

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
        Just result'@(JsonMessage msg) -> (processResult result', Just msg)
        Just result' -> (processResult result', Nothing)
        Nothing -> (pure False, Nothing)
      (outputAccessTime, check)
        | maybe True (diffUTCTime now .> (>= minTimeBetweenPollingNixStore)) inputAccessTime = (Just now, detectLocalFinishedBuilds)
        | otherwise = (inputAccessTime, pure False)
  ((!hasChanged, !msgs), outputState) <-
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
      output = case printOutput of
         Just _ -> ""
         Nothing -> input
      errors = lefts msgs
  deepseq retval (pure ((errors, output <> ByteString.unlines (rights msgs)), retval))

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

withChange :: Functor f => f b -> f Bool
withChange = (True <$)

noChange :: Applicative f => f Bool
noChange = pure False

processResult :: UpdateMonad m => ParseResult -> NOMStateT (WriterT [Either NOMError ByteString] m) Bool
processResult result = do
  now <- getNow
  case result of
    Parser.Uploading path host -> withChange do
      pathId <- getStorePathId path
      uploaded host pathId now
    Parser.Downloading path host -> withChange do
      pathId <- getStorePathId path
      finishedRemoteBuild <- downloaded host pathId now
      whenJust finishedRemoteBuild \build -> finishBuilds host [build]
    PlanCopies _ -> noChange
    Build drvName host -> withChange do
      building host drvName now Nothing
    PlanBuilds plannedBuilds _lastBuild -> withChange do
      plannedDrvIds <- forM (toList plannedBuilds) \drv ->
        lookupDerivation drv
      planBuilds (fromList plannedDrvIds)
    PlanDownloads _download _unpacked plannedDownloads -> withChange do
      plannedDownloadIds <- forM (toList plannedDownloads) \path ->
        getStorePathId path
      planDownloads (fromList plannedDownloadIds)
    Checking drvName -> withChange do
      building Localhost drvName now Nothing
    Parser.Failed drv code -> withChange do
      drvId <- lookupDerivation drv
      failedBuild now drvId code
    JsonMessage msg -> case msg of
      Left err -> do
        tell [Left err]
        noChange
      Right jsonMessage -> processJsonMessage now jsonMessage

processJsonMessage :: UpdateMonad m => UTCTime -> InternalJson -> NOMStateT (WriterT [Either NOMError ByteString] m) Bool
processJsonMessage now = \case
  Message MkMessageAction {message,level} | level <= Info && level > Error -> do
    let message' = encodeUtf8 message
        parseResult = Attoparsec.parseOnly (Left <$> Parser.planBuildLine <|> Right <$> Parser.planDownloadLine) (message' <> "\n")
    tell [Right message']
    case parseResult of
      Right (Right download) -> withChange do
        plannedDownloadId <- getStorePathId download
        planDownloads $ one plannedDownloadId
      Right (Left build) -> withChange do
        plannedDrvId <- lookupDerivation build
        planBuilds (one plannedDrvId)
      _ -> noChange
  Message MkMessageAction {message,level = Error} | stripped <- stripANSICodes message, Text.isPrefixOf "error:" stripped -> withChange do
    errors <- gets (.nixErrors)
    unless (stripped `elem` errors) do
      modify' (field @"nixErrors" %~ (<> [stripped]))
      whenJust (parseOneText updateParser message) \result ->
        void (processResult result)
      tell [Right (encodeUtf8 message)]
  Result MkResultAction {result = BuildLogLine line, id=id'} -> do
    nomState <- get
    let prefix = activityPrefix (IntMap.lookup id'.value nomState.activities <|>> view _1)
    tell [Right (encodeUtf8 (prefix <> line))]
    noChange
  Result MkResultAction {result = SetPhase phase, id=id'} -> withChange do
    modify' (field @"activities" %~ IntMap.adjust (_2 ?~ phase) id'.value)
  Result MkResultAction {result = Progress _} ->
    noChange
    -- withChange $ modify' (field @"activities" %~ IntMap.adjust (_3 ?~ progress) id'.value)
  Start startAction@MkStartAction{id=id'} -> withChange do
    when (not (Text.null startAction.text) && startAction.level <= Info) $ tell [Right (encodeUtf8 (activityPrefix (Just startAction.activity) <> startAction.text))]
    modify' (field @"activities" %~ IntMap.insert id'.value (startAction.activity, Nothing,Nothing))
    case startAction.activity of
      JSON.Build drvName host _ _ -> do
        building host drvName now (Just id')
      JSON.CopyPath path from Localhost -> do
        pathId <- getStorePathId path
        finishedRemoteBuild <- downloading from pathId now
        whenJust finishedRemoteBuild \build -> finishBuilds from [build]
      JSON.CopyPath path Localhost to -> do
        pathId <- getStorePathId path
        uploading to pathId now
      _ -> pass --tell [Right (encodeUtf8 (markup yellow "unused activity: " <> show startAction.id <> " " <> show startAction.activity))]
  Stop MkStopAction{id=id'} -> do
    activity <- gets (\s -> IntMap.lookup id'.value s.activities)
    case activity of
      Just (JSON.CopyPath path from Localhost,_,_) -> withChange do
        pathId <- getStorePathId path
        void $ downloaded from pathId now
      Just (JSON.CopyPath path Localhost to,_,_) -> withChange do
        pathId <- getStorePathId path
        uploaded to pathId now
      _ -> noChange
  _other -> do
    --tell [Right (encodeUtf8 (markup yellow "unused message: " <> show _other))]
    noChange

activityPrefix :: Maybe Activity -> Text
activityPrefix = \case
                   Just (JSON.Build derivation _ _ _) -> toText (setSGRCode [Reset]) <> markup blue (getReportName derivation <> "> ")
                   _ -> ""

movingAverage :: Double
movingAverage = 0.5

reportFinishingBuilds :: (MonadCacheBuildReports m, MonadNow m) => Host -> NonEmpty (Derivation, UTCTime) -> m BuildReportMap
reportFinishingBuilds host builds = do
  now <- getNow
  updateBuildReports (modifyBuildReports host (timeDiffInt now <<$>> builds))

-- | time difference in seconds rounded down
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
    Building a -> State.Failed (a <|>> (now, code,))
    x -> x

lookupDerivation :: MonadReadDerivation m => Derivation -> NOMStateT (WriterT [Either NOMError ByteString] m) DerivationId
lookupDerivation drv = do
  drvId <- getDerivationId drv
  isCached <- gets ((.derivationInfos) .> CMap.lookup drvId .> maybe False (.cached))
  unless isCached $
    getDerivation drv >>= \case
      Left err -> tell [Left err]
      Right parsedDrv -> insertDerivation parsedDrv drvId
  pure drvId

insertDerivation :: MonadReadDerivation m => Nix.Derivation FilePath Text -> DerivationId -> NOMStateT (WriterT [Either NOMError ByteString] m) ()
insertDerivation derivation drvId = do
  outputs' <-
    derivation.outputs |> Map.traverseMaybeWithKey \_ path -> do
      parseStorePath (Nix.path path) |> mapM \pathName -> do
        pathId <- getStorePathId pathName
        modify (field @"storePathInfos" %~ CMap.adjust (field @"producer" ?~ drvId) pathId)
        pure pathId
  inputSources <-
    derivation.inputSrcs |> flip foldlM mempty \acc path -> do
      pathIdMay <-
        parseStorePath path |> mapM \pathName -> do
          pathId <- getStorePathId pathName
          modify (field @"storePathInfos" %~ CMap.adjust (field @"inputFor" %~ CSet.insert drvId) pathId)
          pure pathId
      pure $ maybe id CSet.insert pathIdMay acc
  inputDerivationsList <-
    derivation.inputDrvs |> Map.toList .> mapMaybeM \(drvPath, outs) -> do
      depIdMay <-
        parseDerivation drvPath |> mapM \depName -> do
          depId <- lookupDerivation depName
          modify (field @"derivationInfos" %~ CMap.adjust (field @"derivationParents" %~ CSet.insert drvId) depId)
          modify (field @"forestRoots" %~ Seq.filter (/= depId))
          pure depId
      pure $ depIdMay <|>> (,outs)
  let inputDerivations = Seq.fromList inputDerivationsList
  modify (field @"derivationInfos" %~ CMap.adjust (\i -> i{outputs = outputs', inputSources, inputDerivations, cached = True, platform = Just derivation.platform, pname = Map.lookup "pname" derivation.env}) drvId)
  noParents <- getDerivationInfos drvId <|>> (.derivationParents) .> CSet.null
  when noParents $ modify (field @"forestRoots" %~ (drvId Seq.<|))

planBuilds :: Set DerivationId -> NOMState ()
planBuilds drvIds = forM_ drvIds \drvId ->
  updateDerivationState drvId (const Planned)

planDownloads :: Set StorePathId -> NOMState ()
planDownloads pathIds = forM_ pathIds \pathId ->
  insertStorePathState pathId DownloadPlanned Nothing

downloading :: Host -> StorePathId -> UTCTime -> NOMState (Maybe (DerivationId, RunningBuildInfo))
downloading host pathId start = do
  insertStorePathState pathId (State.Downloading MkTransferInfo {host, duration = start}) Nothing
  runMaybeT $ do
    drvId <- MaybeT (out2drv pathId)
    drvInfos <- MaybeT (gets ((.derivationInfos) .> CMap.lookup drvId))
    MaybeT (pure (preview (typed @BuildStatus % _As @"Building") drvInfos <|>> (() <$) .> (drvId,)))

downloaded :: Host -> StorePathId -> UTCTime -> NOMState (Maybe (DerivationId, RunningBuildInfo))
downloaded host pathId end = do
  insertStorePathState pathId (Downloaded MkTransferInfo {host, duration = Nothing}) $ Just \case
    State.Downloading transfer_info | transfer_info.host == host -> Downloaded (transfer_info{duration = Just $ timeDiffInt end transfer_info.duration})
    other -> other
  runMaybeT $ do
    drvId <- MaybeT (out2drv pathId)
    drvInfos <- MaybeT (gets ((.derivationInfos) .> CMap.lookup drvId))
    MaybeT (pure (preview (typed @BuildStatus % _As @"Building") drvInfos <|>> (() <$) .> (drvId,)))

uploading :: Host -> StorePathId -> UTCTime -> NOMState ()
uploading host pathId start =
  insertStorePathState pathId (State.Uploading MkTransferInfo {host, duration = start}) Nothing
uploaded :: Host -> StorePathId -> UTCTime -> NOMState ()
uploaded host pathId end =
  insertStorePathState pathId (Uploaded MkTransferInfo {host, duration = Nothing}) $ Just \case
    State.Uploading transfer_info | transfer_info.host == host -> Uploaded (transfer_info{duration = Just $ timeDiffInt end transfer_info.duration})
    other -> other

building :: UpdateMonad m => Host -> Derivation -> UTCTime -> Maybe ActivityId -> NOMStateT (WriterT [Either NOMError ByteString] m) ()
building host drvName now activityId = do
  lastNeeded <- get <|>> (.buildReports) .> Map.lookup (host, getReportName drvName)
  drvId <- lookupDerivation drvName
  updateDerivationState drvId (const (Building (MkBuildInfo now host lastNeeded activityId)))

updateDerivationState :: DerivationId -> (BuildStatus -> BuildStatus) -> NOMState ()
updateDerivationState drvId updateStatus = do
  -- Update derivationInfo for this Derivation
  derivation_infos <- getDerivationInfos drvId
  let oldStatus = derivation_infos.buildStatus
      newStatus = updateStatus oldStatus
  when (oldStatus /= newStatus) do
    modify (field @"derivationInfos" %~ CMap.adjust (field @"buildStatus" .~ newStatus) drvId)
    let
      update_summary = updateSummaryForDerivation oldStatus newStatus drvId
      update_status
        | Building{} <- newStatus = \case
          Unknown -> Planned
          other -> other
        | otherwise = id

    -- Update summaries of all parents and sort them
    updateParents update_status update_summary (derivation_infos.derivationParents)

    -- Update fullSummary
    modify (field @"fullSummary" %~ update_summary)

updateParents :: (BuildStatus -> BuildStatus) -> (DependencySummary  -> DependencySummary ) -> DerivationSet -> NOMState ()
updateParents stateUpdate update_func = go mempty
 where
  go :: DerivationSet -> DerivationSet -> NOMState ()
  go updated_parents parentsToUpdate = case CSet.maxView parentsToUpdate of
    Nothing -> modify (field @"touchedIds" %~ CSet.union updated_parents)
    Just (parentToUpdate, restToUpdate) -> do
      updateDerivationState parentToUpdate stateUpdate
      modify (field @"derivationInfos" %~ CMap.adjust (field @"dependencySummary" %~ update_func) parentToUpdate)
      next_parents <- getDerivationInfos parentToUpdate <|>> (.derivationParents)
      go (CSet.insert parentToUpdate updated_parents) (CSet.union (CSet.difference next_parents updated_parents) restToUpdate)

updateStorePathStates :: StorePathState -> Maybe (StorePathState -> StorePathState) -> Set StorePathState -> Set StorePathState
updateStorePathStates new_state update_state = case update_state of
    Just update_func -> Set.toList .> fmap update_func .> Set.fromList
    Nothing -> id
    .> localFilter .> Set.insert new_state
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
  modify (field @"storePathInfos" %~ CMap.adjust (field @"states" .~ newStatus) storePathId)

  let update_summary = updateSummaryForStorePath oldStatus newStatus storePathId

  -- Update summaries of all parents
  updateParents id update_summary (maybe id CSet.insert store_path_info.producer store_path_info.inputFor)

  -- Update fullSummary
  modify (field @"fullSummary" %~ update_summary)
