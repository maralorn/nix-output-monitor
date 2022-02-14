module NOM.Update where

import Relude

import Control.Monad.Except (liftEither)
import Data.Generics.Product (field, typed)
import Data.Generics.Sum (_As)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime)
import Optics (preview, (%), (%~), (.~), (?~))

import Data.Attoparsec.Text (endOfInput, parseOnly)

import qualified Nix.Derivation as Nix

import NOM.Parser (Derivation (..), Host (..), ParseResult (..), StorePath (..))
import qualified NOM.Parser as Parser
import NOM.State (BuildInfo (MkBuildInfo, buildStart), BuildStatus (..), DerivationId, DerivationInfo (..), DisplayState (OptionalNode), NOMState, NOMStateT, NOMV1State (..), ProcessState (..), RunningBuildInfo, StorePathId, StorePathState (..), drv2out, getDerivationId, getRunningBuildsByHost, getStorePathId, insertStorePathState, lookupDerivationId, out2drv, reportError, updateDerivationState)
import qualified NOM.State as State
import qualified NOM.State.CacheId.Map as CMap
import qualified NOM.State.CacheId.Set as CSet
import NOM.Update.Monad (
  BuildReportMap,
  MonadCacheBuildReports (..),
  MonadCheckStorePath (..),
  MonadNow (..),
  MonadReadDerivation (..),
  UpdateMonad,
 )
import NOM.Util (firstFF, foldEndo, forMaybeM, hush, (.>), (<.>>), (<|>>), (|>))

getReportName :: Derivation -> Text
getReportName = Text.dropWhileEnd (`Set.member` fromList ".1234567890-") . name . toStorePath

{-
data SortOrder
  = -- First the failed builds starting with the earliest failures
    SFailed UTCTime
  | -- Second the running builds starting with longest running
    -- For one build prefer the tree with the longest prefix for the highest probability of few permutations over time
    SBuilding UTCTime (Down Int)
  | SDownloading
  | SUploading
  | SWaiting
  | SDownloadWaiting
  | -- The longer a build is completed the less it matters
    SDone (Down UTCTime)
  | SDownloaded
  | SUploaded
  | -- Links are really not that interesting
    SLink
  deriving (Eq, Show, Ord)

mkOrder :: (a -> SortOrder) -> Tree a -> SortOrder
mkOrder order = go
 where
  go (Node label subForest) = minimum (order label : (increaseLevel . go <$> subForest))

increaseLevel :: SortOrder -> SortOrder
increaseLevel = \case
  SBuilding t i -> SBuilding t (i + 1)
  s -> s

nodeOrder :: BuildTreeNode -> SortOrder
nodeOrder = \case
  Left (DerivationNode _ (Just (_, Building start _))) -> SBuilding start 0
  Left (DerivationNode _ (Just (_, State.Failed _ _ at))) -> SFailed at
  Left (DerivationNode _ (Just (_, Built _ at))) -> SDone (Down at)
  Left (DerivationNode _ Nothing) -> SWaiting
  Right (StorePathNode _ _ state') -> state' |> fmap storePathOrder .> minimum

storePathOrder :: StorePathState -> SortOrder
storePathOrder = \case
  State.Downloaded _ -> SDownloaded
  State.Downloading _ -> SDownloading
  State.Uploaded _ -> SUploaded
  State.Uploading _ -> SUploading
  State.DownloadPlanned -> SDownloadWaiting
-}
setInputReceived :: NOMState Bool
setInputReceived = do
  s <- get
  let change = processState s == JustStarted
  when change (put s{processState = InputReceived})
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
      ( orM
          [ -- First check if we this is the first time that we receive input (for error messages)
            setInputReceived
          , -- Update the state if any changes where parsed.
            maybe (pure False) processResult result
          , -- Check if any local builds have finished, because nix-build would not tell us.
            -- If we haven‘t done so in the last 200ms.
            check
          ]
      )
      inputState
  -- If any of the update steps returned true, return the new state, otherwise return Nothing.
  (if hasChanged then Just outputState else Nothing) |> (outputAccessTime,) .> pure

detectLocalFinishedBuilds :: UpdateMonad m => NOMStateT m Bool
detectLocalFinishedBuilds = do
  runningLocalBuilds <- getRunningBuildsByHost Localhost <|>> CMap.toList
  newCompletedOutputs <- filterM (fst .> drv2out >=> maybe (pure False) storePathExists) runningLocalBuilds
  let anyBuildsFinished = null newCompletedOutputs
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
  let report =
        fmap (second buildStart) .> reportFinishingBuilds host
          >=> (field @"buildReports" .~) .> modify
  forMaybeM builds (firstFF lookupDerivationId)
    >>= nonEmpty .> maybe pass report
  now <- getNow
  forM_ builds \(drv, info) -> updateDerivationState drv (const (Built (info $> now)))

modifyBuildReports :: Host -> NonEmpty (Derivation, Int) -> BuildReportMap -> BuildReportMap
modifyBuildReports host = fmap (uncurry insertBuildReport) .> foldEndo
 where
  insertBuildReport name =
    Map.insertWith
      (\new old -> floor (movingAverage * fromIntegral new + (1 - movingAverage) * fromIntegral old))
      (host, getReportName name)

failedBuild :: UTCTime -> DerivationId -> Int -> NOMState ()
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
      inputDerivations <-
        Nix.inputDrvs parsedDerivation |> Map.toList .> mapMaybeM \(drvPath, outs) -> do
          depIdMay <-
            parseDerivation drvPath |> mapM \depName -> do
              depId <- lookupDerivation depName
              modify (field @"derivationInfos" %~ CMap.adjust (field @"derivationParents" %~ CSet.insert drvId) depId)
              pure depId
          pure $ depIdMay <|>> (,OptionalNode,outs)
      modify (field @"derivationInfos" %~ CMap.adjust (\i -> i{outputs, inputSources, inputDerivations, cached = True}) drvId)
  pure drvId

parseStorePath :: FilePath -> Maybe StorePath
parseStorePath = hush . parseOnly (Parser.storePath <* endOfInput) . fromString
parseDerivation :: FilePath -> Maybe Derivation
parseDerivation = hush . parseOnly (Parser.derivation <* endOfInput) . fromString

planBuilds :: Set DerivationId -> NOMState ()
planBuilds = mapM_ \drv -> updateDerivationState drv (const Planned)

{-
derivationUpdate :: NOMV1State -> Derivation -> Maybe (Host, BuildStatus) -> ForestUpdate BuildTreeNode
derivationUpdate buildState derivation' newState = do
  let def = Left @DerivationNode @StorePathNode (DerivationNode derivation' newState)
  ForestUpdate
    { isParent = flip (isDirectDependency buildState) def
    , isChild = isDirectDependency buildState def
    , match = matchDerivation derivation'
    , update = const def
    , def
    }

storePathUpdate :: NOMV1State -> StorePath -> StorePathState -> ForestUpdate BuildTreeNode
storePathUpdate buildState storePath newState = do
  let drv = Map.lookup storePath (outputToDerivation buildState)
      def = Right (StorePathNode storePath drv (one newState))
  ForestUpdate
    { isParent = flip (isDirectDependency buildState) def
    , isChild = isDirectDependency buildState def
    , match = matchStorePath storePath
    , update = second ((typed %~ insertStorePathState newState) .> (typed .~ drv))
    , def
    }
-}

{-
isDirectDependency :: NOMV1State -> BuildTreeNode -> BuildTreeNode -> Bool
isDirectDependency buildState parent' child' = fromMaybe False $ case (parent', child') of
  (Left (DerivationNode parent _), Left (DerivationNode child _)) -> drvsDep parent child
  (Left (DerivationNode parent _), Right child) -> drv2path parent child
  (Right (StorePathNode _ (Just parent) _), Right child) -> drv2path parent child
  (Right (StorePathNode _ Nothing _), Right _) -> Nothing
  (Right _, Left _) -> Nothing
 where
  getInfo parent = Map.lookup parent (derivationInfos buildState)
  drvsDep parent child = getInfo parent <|>> inputDrvs .> Map.keysSet .> Set.member child
  drv2path parent = \case
    (StorePathNode child Nothing _) -> getInfo parent <|>> inputSrcs .> Set.member child
    (StorePathNode _ (Just child) _) -> drvsDep parent child
matchDerivation :: Derivation -> BuildTreeNode -> Bool
matchDerivation derivation' = either ((derivation' ==) . derivation) (const False)
matchStorePath :: StorePath -> BuildTreeNode -> Bool
matchStorePath storePath = either (const False) ((storePath ==) . path)
-}

planDownloads :: Set StorePathId -> NOMState ()
planDownloads = mapM_ (insertStorePathState DownloadPlanned)

downloaded :: Host -> StorePathId -> NOMState (Maybe (DerivationId, RunningBuildInfo))
downloaded host pathId = do
  insertStorePathState (Downloaded host) pathId
  runMaybeT $ do
    drvId <- MaybeT (out2drv pathId)
    drvInfos <- MaybeT (gets (derivationInfos .> CMap.lookup drvId))
    MaybeT (pure (preview (typed @BuildStatus % _As @"Building") drvInfos <|>> (drvId,)))

uploaded :: Host -> StorePathId -> NOMState ()
uploaded host = insertStorePathState (Downloaded host)

building :: Host -> DerivationId -> UTCTime -> NOMState ()
building host drv now = do
  lastNeeded <- runMaybeT do
    drvName <- MaybeT (lookupDerivationId drv)
    MaybeT (gets (buildReports .> Map.lookup (host, getReportName drvName)))
  updateDerivationState drv (const (Building (MkBuildInfo now host lastNeeded ())))
