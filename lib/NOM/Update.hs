module NOM.Update where

import Control.Monad.Except (liftEither)
import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Generics.Product (field, typed)
import Data.Generics.Sum (_As)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime)
import NOM.Parser (Derivation (..), Host (..), ParseResult (..), StorePath (..), FailType)
import qualified NOM.Parser as Parser
import NOM.State (BuildInfo (MkBuildInfo, buildStart), BuildStatus (..), DependencySummary, DerivationId, DerivationInfo (..), DerivationSet, NOMState, NOMStateT, NOMV1State (..), ProcessState (..), RunningBuildInfo, StorePathId, StorePathState (..), drv2out, getDerivationId, getDerivationInfos, getRunningBuildsByHost, getStorePathId, getStorePathInfos, lookupDerivationId, out2drv, reportError, storePathInputFor, storePathProducer, storePathStates, updateSummaryForDerivation, updateSummaryForStorePath)
import qualified NOM.State as State
import qualified NOM.State.CacheId.Map as CMap
import qualified NOM.State.CacheId.Set as CSet
import NOM.State.Sorting (sortKey, sortDepsOfSet)
import NOM.Update.Monad
  ( BuildReportMap,
    MonadCacheBuildReports (..),
    MonadCheckStorePath (..),
    MonadNow (..),
    MonadReadDerivation (..),
    UpdateMonad,
  )
import NOM.Util (foldMapEndo, hush, (.>), (<.>>), (<|>>), (|>))
import qualified Nix.Derivation as Nix
import Optics (preview, (%), (%~), (.~), (?~))
import Relude

getReportName :: Derivation -> Text
getReportName = Text.dropWhileEnd (`Set.member` fromList ".1234567890-") . name . toStorePath

setInputReceived :: NOMState Bool
setInputReceived = do
  s <- get
  let change = processState s == JustStarted
  when change (put s {processState = InputReceived})
  pure change

updateState :: UpdateMonad m => Maybe ParseResult -> (Maybe UTCTime, NOMV1State) -> m (Maybe UTCTime, Maybe NOMV1State)
updateState result (inputAccessTime, inputState) = do
  now <- getNow
  let (outputAccessTime, check) =
        if maybe True (diffUTCTime now .> (>= 0.2)) inputAccessTime
          then (Just now, detectLocalFinishedBuilds)
          else (inputAccessTime, pure False)
  (hasChanged, outputState) <-
    runStateT
      ( or
          <$> sequence
            [ -- First check if we this is the first time that we receive input (for error messages)
              setInputReceived,
              -- Update the state if any changes where parsed.
              maybe (pure False) processResult result,
              -- Check if any local builds have finished, because nix-build would not tell us.
              -- If we haven‘t done so in the last 200ms.
              check
            ]
      )
      inputState
  -- If any of the update steps returned true, return the new state, otherwise return Nothing.
  (if hasChanged then Just outputState else Nothing) |> (outputAccessTime,) .> pure

detectLocalFinishedBuilds :: UpdateMonad m => NOMStateT m Bool
detectLocalFinishedBuilds = do
  runningLocalBuilds <- getRunningBuildsByHost Localhost <|>> CMap.toList -- .> traceShowId
  let isCompleted (drvId, _) =
        drv2out drvId >>= \case
          Nothing -> pure False -- Derivation has no "out" output.
          Just path -> storePathExists path
  newCompletedOutputs <- filterM isCompleted runningLocalBuilds -- <|>> traceShowId
  let anyBuildsFinished = not (null newCompletedOutputs)
  when anyBuildsFinished (finishBuilds Localhost newCompletedOutputs)
  pure anyBuildsFinished

processResult :: UpdateMonad m => ParseResult -> NOMStateT m Bool
processResult result = do
  let withChange = (True <$)
      noChange = pure False
  now <- getNow
  case result of
    Parser.Uploading path host ->
      withChange $
        getStorePathId path >>= uploaded host
    Parser.Downloading path host ->
      withChange $
        getStorePathId path
          >>= downloaded host
          >>= maybeToList
          .> finishBuilds host
    PlanCopies _ -> noChange
    Build drv host -> withChange do
      lookupDerivation drv >>= flip (building host) now
    PlanBuilds plannedBuilds _lastBuild -> withChange do
      mapM lookupDerivation (toList plannedBuilds) >>= fromList .> planBuilds
    PlanDownloads _download _unpacked plannedDownloads ->
      withChange $
        mapM getStorePathId (toList plannedDownloads) >>= fromList .> planDownloads
    Checking drv ->
      withChange $
        lookupDerivation drv >>= flip (building Localhost) now
    Parser.Failed drv code ->
      withChange $
        lookupDerivation drv >>= flip (failedBuild now) code

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
    lookupDerivationId drvId <|>> (,buildStart buildInfo)
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

lookupDerivation :: MonadReadDerivation m => Derivation -> NOMStateT m DerivationId
lookupDerivation drv = do
  drvId <- getDerivationId drv
  isCached <- gets (derivationInfos .> CMap.lookup drvId .> maybe False cached)
  unless isCached $
    either reportError pure =<< runExceptT do
      parsedDerivation <- getDerivation drv >>= liftEither
      outputs <-
        Nix.outputs parsedDerivation |> Map.traverseMaybeWithKey \_ path -> do
          parseStorePath (Nix.path path) |> mapM \pathName -> do
            pathId <- getStorePathId pathName
            modify (field @"storePathInfos" %~ CMap.adjust (field @"storePathProducer" ?~ drvId) pathId)
            pure pathId
      inputSources <-
        Nix.inputSrcs parsedDerivation |> flip foldlM mempty \acc path -> do
          pathIdMay <-
            parseStorePath path |> mapM \pathName -> do
              pathId <- getStorePathId pathName
              modify (field @"storePathInfos" %~ CMap.adjust (field @"storePathInputFor" %~ CSet.insert drvId) pathId)
              pure pathId
          pure $ maybe id CSet.insert pathIdMay acc
      inputDerivationsList <-
        Nix.inputDrvs parsedDerivation |> Map.toList .> mapMaybeM \(drvPath, outs) -> do
          depIdMay <-
            parseDerivation drvPath |> mapM \depName -> do
              depId <- lookupDerivation depName
              modify (field @"derivationInfos" %~ CMap.adjust (field @"derivationParents" %~ CSet.insert drvId) depId)
              modify (field @"forestRoots" %~ Seq.filter (/= depId))
              pure depId
          pure $ depIdMay <|>> (,outs)
      let inputDerivations = Seq.fromList inputDerivationsList
      modify (field @"derivationInfos" %~ CMap.adjust (\i -> i {outputs, inputSources, inputDerivations, cached = True}) drvId)
      noParents <- getDerivationInfos drvId <|>> derivationParents .> CSet.null
      when noParents $ modify (field @"forestRoots" %~ (drvId Seq.<|))
  pure drvId

parseStorePath :: FilePath -> Maybe StorePath
parseStorePath = hush . parseOnly (Parser.storePath <* endOfInput) . fromString

parseDerivation :: FilePath -> Maybe Derivation
parseDerivation = hush . parseOnly (Parser.derivation <* endOfInput) . fromString

planBuilds :: Set DerivationId -> NOMState ()
planBuilds = mapM_ \drv -> updateDerivationState drv (const Planned)

planDownloads :: Set StorePathId -> NOMState ()
planDownloads = mapM_ (`insertStorePathState` DownloadPlanned)

downloaded :: Host -> StorePathId -> NOMState (Maybe (DerivationId, RunningBuildInfo))
downloaded host pathId = do
  insertStorePathState pathId (Downloaded host)
  runMaybeT $ do
    drvId <- MaybeT (out2drv pathId)
    drvInfos <- MaybeT (gets (derivationInfos .> CMap.lookup drvId))
    MaybeT (pure (preview (typed @BuildStatus % _As @"Building") drvInfos <|>> (drvId,)))

uploaded :: Host -> StorePathId -> NOMState ()
uploaded host = flip insertStorePathState (Uploaded host)

building :: Host -> DerivationId -> UTCTime -> NOMState ()
building host drv now = do
  drvName <- lookupDerivationId drv
  lastNeeded <- get <|>> buildReports .> Map.lookup (host, getReportName drvName)
  updateDerivationState drv (const (Building (MkBuildInfo now host lastNeeded ())))

updateDerivationState :: DerivationId -> (BuildStatus -> BuildStatus) -> NOMState ()
updateDerivationState drvId updateStatus = do
  -- Update derivationInfo for this Derivation
  derivation_infos <- getDerivationInfos drvId
  let oldStatus = buildStatus derivation_infos
      newStatus = updateStatus oldStatus
  modify (field @"derivationInfos" %~ CMap.adjust (field @"buildStatus" .~ newStatus) drvId)

  let update_summary = updateSummaryForDerivation oldStatus newStatus drvId

  -- Update summaries of all parents and sort them
  updateParents update_summary (derivationParents derivation_infos)

  -- Update fullSummary
  modify (field @"fullSummary" %~ update_summary)
  currentState <- get
  modify (field @"forestRoots" %~ Seq.sortOn (sortKey currentState))

updateParents :: (DependencySummary -> DependencySummary) -> DerivationSet -> NOMState ()
updateParents update_func = go mempty
  where
    go updated_parents parentsToUpdate = case CSet.maxView parentsToUpdate of
      Nothing -> sortDepsOfSet updated_parents
      Just (parentToUpdate, restToUpdate) -> do
        modify (field @"derivationInfos" %~ CMap.adjust (field @"dependencySummary" %~ update_func) parentToUpdate)
        next_parents <- getDerivationInfos parentToUpdate <|>> derivationParents
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
  store_path_infos <- getStorePathInfos storePathId
  let oldStatus = storePathStates store_path_infos
      newStatus = updateStorePathStates newStorePathState oldStatus
  modify (field @"storePathInfos" %~ CMap.adjust (field @"storePathStates" .~ newStatus) storePathId)

  let update_summary = updateSummaryForStorePath oldStatus newStatus storePathId

  -- Update summaries of all parents
  updateParents update_summary (maybe id CSet.insert (storePathProducer store_path_infos) (storePathInputFor store_path_infos))

  -- Update fullSummary
  modify (field @"fullSummary" %~ update_summary)
  currentState <- get
  modify (field @"forestRoots" %~ Seq.sortOn (sortKey currentState))