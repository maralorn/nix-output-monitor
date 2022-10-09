module NOM.State (
  ProcessState (..),
  RunningBuildInfo,
  StorePathId,
  StorePathState (..),
  StorePathInfo (..),
  StorePathSet,
  StorePathMap,
  BuildInfo (..),
  BuildStatus (..),
  DependencySummary (..),
  DerivationId,
  DerivationInfo (..),
  DerivationSet,
  DerivationMap,
  TransferInfo (..),
  NOMState,
  NOMV1State (..),
  getDerivationInfos,
  initalStateFromBuildPlatform,
  updateSummaryForStorePath,
  clearDerivationIdFromSummary,
  clearStorePathsFromSummary,
  getStorePathInfos,
  NOMStateT,
  getRunningBuilds,
  getRunningBuildsByHost,
  lookupStorePathId,
  lookupDerivationId,
  getStorePathId,
  getDerivationId,
  out2drv,
  drv2out,
  updateSummaryForDerivation,
) where

import Relude

import Data.Generics.Product (HasField (field))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Optics ((%~))

import NOM.Builds (Derivation (..), FailType, Host (..), StorePath (..))
import NOM.NixMessage.JSON (Activity, ActivityId, ActivityProgress)
import NOM.State.CacheId (CacheId)
import NOM.State.CacheId.Map (CacheIdMap)
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set (CacheIdSet)
import NOM.State.CacheId.Set qualified as CSet
import NOM.Update.Monad (
  BuildReportMap,
  MonadCacheBuildReports (getCachedBuildReports),
  MonadNow,
  getNow,
 )
import NOM.Util (foldMapEndo)

data StorePathState
  = DownloadPlanned
  | Downloading RunningTransferInfo
  | Uploading RunningTransferInfo
  | Downloaded CompletedTransferInfo
  | Uploaded CompletedTransferInfo
  deriving stock (Show, Eq, Ord, Generic)

data DerivationInfo = MkDerivationInfo
  { name :: Derivation
  , outputs :: Map Text StorePathId
  , inputDerivations :: Seq (DerivationId, Set Text)
  , inputSources :: StorePathSet
  , buildStatus :: BuildStatus
  , dependencySummary :: DependencySummary
  , cached :: Bool
  , derivationParents :: DerivationSet
  , pname :: Maybe Text
  , platform :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)

type StorePathId = CacheId StorePath

type DerivationId = CacheId Derivation

type StorePathMap = CacheIdMap StorePath

type DerivationMap = CacheIdMap Derivation

type StorePathSet = CacheIdSet StorePath

type DerivationSet = CacheIdSet Derivation

data StorePathInfo = MkStorePathInfo
  { name :: StorePath
  , states :: Set StorePathState
  , producer :: Maybe DerivationId
  , inputFor :: DerivationSet
  }
  deriving stock (Show, Eq, Ord, Generic)

type RunningBuildInfo = BuildInfo ()

type CompletedBuildInfo = BuildInfo UTCTime

type RunningTransferInfo = TransferInfo ()

type CompletedTransferInfo = TransferInfo (Maybe UTCTime)

type FailedBuildInfo = BuildInfo (UTCTime, FailType)

data DependencySummary = MkDependencySummary
  { plannedBuilds :: DerivationSet
  , runningBuilds :: DerivationMap RunningBuildInfo
  , completedBuilds :: DerivationMap CompletedBuildInfo
  , failedBuilds :: DerivationMap FailedBuildInfo
  , plannedDownloads :: StorePathSet
  , completedDownloads :: StorePathMap CompletedTransferInfo
  , completedUploads :: StorePathMap CompletedTransferInfo
  , runningDownloads :: StorePathMap RunningTransferInfo
  , runningUploads :: StorePathMap RunningTransferInfo
  }
  deriving stock (Show, Eq, Ord, Generic)

data NOMV1State = MkNOMV1State
  { derivationInfos :: DerivationMap DerivationInfo
  , storePathInfos :: StorePathMap StorePathInfo
  , fullSummary :: DependencySummary
  , forestRoots :: Seq DerivationId
  , buildReports :: BuildReportMap
  , startTime :: UTCTime
  , processState :: ProcessState
  , storePathIds :: Map StorePath StorePathId
  , derivationIds :: Map Derivation DerivationId
  , touchedIds :: DerivationSet
  , activities :: IntMap (Activity, Maybe Text, Maybe ActivityProgress)
  , nixErrors :: [Text]
  , buildPlatform :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)

data ProcessState = JustStarted | InputReceived | Finished
  deriving stock (Show, Eq, Ord, Generic)

data BuildStatus
  = Unknown
  | Planned
  | Building (BuildInfo ())
  | Failed (BuildInfo (UTCTime, FailType)) -- End
  | Built (BuildInfo UTCTime) -- End
  deriving stock (Show, Eq, Ord, Generic)

data BuildInfo a = MkBuildInfo
  { start :: UTCTime
  , host :: Host
  , estimate :: Maybe Int
  , activityId :: Maybe ActivityId
  , end :: a
  }
  deriving stock (Show, Eq, Ord, Generic, Functor)

data TransferInfo a = MkTransferInfo
  { host :: Host
  , start :: UTCTime
  , end :: a
  }
  deriving stock (Show, Eq, Ord, Generic, Functor)

initalStateFromBuildPlatform :: (MonadCacheBuildReports m, MonadNow m) => Maybe Text -> m NOMV1State
initalStateFromBuildPlatform platform = do
  now <- getNow
  buildReports <- getCachedBuildReports
  pure $
    MkNOMV1State
      mempty
      mempty
      mempty
      mempty
      buildReports
      now
      JustStarted
      mempty
      mempty
      mempty
      mempty
      mempty
      platform

instance Semigroup DependencySummary where
  (MkDependencySummary ls1 lm2 lm3 lm4 ls5 lm6 lm7 lm8 lm9) <> (MkDependencySummary rs1 rm2 rm3 rm4 rs5 rm6 rm7 rm8 rm9) = MkDependencySummary (ls1 <> rs1) (lm2 <> rm2) (lm3 <> rm3) (lm4 <> rm4) (ls5 <> rs5) (lm6 <> rm6) (lm7 <> rm7) (lm8 <> rm8) (lm9 <> rm9)

instance Monoid DependencySummary where
  mempty = MkDependencySummary mempty mempty mempty mempty mempty mempty mempty mempty mempty

getRunningBuilds :: NOMState (DerivationMap RunningBuildInfo)
getRunningBuilds = gets (.fullSummary.runningBuilds)

getRunningBuildsByHost :: Host -> NOMState (DerivationMap RunningBuildInfo)
getRunningBuildsByHost host = CMap.filter (\x -> x.host == host) <$> getRunningBuilds

lookupStorePathId :: StorePathId -> NOMState StorePath
lookupStorePathId pathId = (.name) <$> getStorePathInfos pathId

lookupDerivationId :: DerivationId -> NOMState Derivation
lookupDerivationId drvId = (.name) <$> getDerivationInfos drvId

type NOMState a = forall m. MonadState NOMV1State m => m a

type NOMStateT m a = MonadState NOMV1State m => m a

emptyStorePathInfo :: StorePath -> StorePathInfo
emptyStorePathInfo path = MkStorePathInfo path mempty Nothing mempty

emptyDerivationInfo :: Derivation -> DerivationInfo
emptyDerivationInfo drv = MkDerivationInfo drv mempty mempty mempty Unknown mempty False mempty Nothing Nothing

getStorePathId :: StorePath -> NOMState StorePathId
getStorePathId path = do
  let newId = do
        key <- gets (CMap.nextKey . (.storePathInfos))
        modify (field @"storePathInfos" %~ CMap.insert key (emptyStorePathInfo path))
        modify (field @"storePathIds" %~ Map.insert path key)
        pure key
  gets (Map.lookup path . (.storePathIds)) >>= maybe newId pure

getDerivationId :: Derivation -> NOMState DerivationId
getDerivationId drv = do
  let newId = do
        key <- gets (CMap.nextKey . (.derivationInfos))
        modify (field @"derivationInfos" %~ CMap.insert key (emptyDerivationInfo drv))
        modify (field @"derivationIds" %~ Map.insert drv key)
        pure key
  gets (Map.lookup drv . (.derivationIds)) >>= maybe newId pure

drv2out :: DerivationId -> NOMState (Maybe StorePath)
drv2out drv =
  gets (CMap.lookup drv . (.derivationInfos) >=> Map.lookup "out" . (.outputs))
    >>= mapM (\pathId -> lookupStorePathId pathId)

out2drv :: StorePathId -> NOMState (Maybe DerivationId)
out2drv path = gets (CMap.lookup path . (.storePathInfos) >=> (.producer))

-- Only do this with derivationIds that you got via lookupDerivation
getDerivationInfos :: DerivationId -> NOMState DerivationInfo
getDerivationInfos drvId =
  fromMaybe (error "BUG: drvId is no key in derivationInfos")
    . CMap.lookup drvId
    . (.derivationInfos)
    <$> get

-- Only do this with derivationIds that you got via lookupDerivation
getStorePathInfos :: StorePathId -> NOMState StorePathInfo
getStorePathInfos storePathId =
  fromMaybe (error "BUG: storePathId is no key in storePathInfos")
    . CMap.lookup storePathId
    . (.storePathInfos)
    <$> get

clearDerivationIdFromSummary :: BuildStatus -> DerivationId -> DependencySummary -> DependencySummary
clearDerivationIdFromSummary oldStatus drvId = case oldStatus of
    Unknown -> id
    Planned -> field @"plannedBuilds" %~ CSet.delete drvId
    Building _ -> field @"runningBuilds" %~ CMap.delete drvId
    Failed _ -> field @"failedBuilds" %~ CMap.delete drvId
    Built _ -> field @"completedBuilds" %~ CMap.delete drvId

updateSummaryForDerivation :: BuildStatus -> BuildStatus -> DerivationId -> DependencySummary -> DependencySummary
updateSummaryForDerivation oldStatus newStatus drvId = clearDerivationIdFromSummary oldStatus drvId . case newStatus of
    Unknown -> id
    Planned -> field @"plannedBuilds" %~ CSet.insert drvId
    Building bi -> field @"runningBuilds" %~ CMap.insert drvId (void bi)
    Failed bi -> field @"failedBuilds" %~ CMap.insert drvId bi
    Built bi -> field @"completedBuilds" %~ CMap.insert drvId bi

clearStorePathsFromSummary :: Set StorePathState -> StorePathId -> DependencySummary -> DependencySummary
clearStorePathsFromSummary deleted_states path_id =
  foldMapEndo remove_deleted deleted_states
 where
  remove_deleted :: StorePathState -> DependencySummary -> DependencySummary
  remove_deleted = \case
    DownloadPlanned -> field @"plannedDownloads" %~ CSet.delete path_id
    Downloading _ -> field @"runningDownloads" %~ CMap.delete path_id
    Uploading _ -> field @"runningUploads" %~ CMap.delete path_id
    Downloaded _ -> field @"completedDownloads" %~ CMap.delete path_id
    Uploaded _ -> field @"completedUploads" %~ CMap.delete path_id

updateSummaryForStorePath :: Set StorePathState -> Set StorePathState -> StorePathId -> DependencySummary -> DependencySummary
updateSummaryForStorePath old_states new_states path_id =
  foldMapEndo insert_added added_states . clearStorePathsFromSummary deleted_states path_id
 where
  insert_added :: StorePathState -> DependencySummary -> DependencySummary
  insert_added = \case
    DownloadPlanned -> field @"plannedDownloads" %~ CSet.insert path_id
    Downloading ho -> field @"runningDownloads" %~ CMap.insert path_id ho
    Uploading ho -> field @"runningUploads" %~ CMap.insert path_id ho
    Downloaded ho -> field @"completedDownloads" %~ CMap.insert path_id ho
    Uploaded ho -> field @"completedUploads" %~ CMap.insert path_id ho
  deleted_states = Set.difference old_states new_states
  added_states = Set.difference new_states old_states
