{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module NOM.State (
  ProgressState (..),
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
  BuildFail (..),
  NOMState,
  NOMV1State (..),
  ActivityStatus (..),
  InterestingActivity (..),
  InputDerivation (..),
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
  getStorePathId,
  getDerivationId,
  outPathToDerivation,
  derivationToAnyOutPath,
  updateSummaryForDerivation,
  inputStorePaths,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Strict qualified as Strict
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
import Optics (gfield, (%~))
import Relude
import Type.Strict qualified as StrictType

instance StrictType.StrictType seen v => StrictType.StrictType seen (IntMap v)

instance StrictType.StrictType seen v => StrictType.StrictType seen (Map k v)

instance StrictType.StrictType seen IntSet

instance StrictType.StrictType seen v => StrictType.StrictType seen (Seq v)

data StorePathState
  = DownloadPlanned
  | Downloading RunningTransferInfo
  | Uploading RunningTransferInfo
  | Downloaded CompletedTransferInfo
  | Uploaded CompletedTransferInfo
  deriving stock (Show, Eq, Ord, Generic)

data InputDerivation = MkInputDerivation
  { derivation :: DerivationId
  , outputs :: Set Text
  }
  deriving stock (Show, Eq, Ord, Generic)

data DerivationInfo = MkDerivationInfo
  { name :: Derivation
  , outputs :: Map Text StorePathId
  , inputDerivations :: Seq InputDerivation
  , inputSources :: StorePathSet
  , buildStatus :: BuildStatus
  , dependencySummary :: DependencySummary
  , cached :: Bool
  , derivationParents :: DerivationSet
  , pname :: Strict.Maybe Text
  , platform :: Strict.Maybe Text
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
  , producer :: Strict.Maybe DerivationId
  , inputFor :: DerivationSet
  }
  deriving stock (Show, Eq, Ord, Generic)

type RunningBuildInfo = BuildInfo ()

type CompletedBuildInfo = BuildInfo Double

type RunningTransferInfo = TransferInfo ()

type CompletedTransferInfo = TransferInfo (Strict.Maybe Double)

type FailedBuildInfo = BuildInfo BuildFail

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

data ActivityStatus = MkActivityStatus
  { activity :: Activity
  , phase :: Strict.Maybe Text
  , progress :: Strict.Maybe ActivityProgress
  }
  deriving stock (Show, Eq, Ord, Generic)

data InterestingActivity = MkInterestingUnknownActivity
  { text :: Text
  , start :: Double
  }
  deriving stock (Show, Eq, Ord, Generic)

data NOMV1State = MkNOMV1State
  { derivationInfos :: DerivationMap DerivationInfo
  , storePathInfos :: StorePathMap StorePathInfo
  , fullSummary :: DependencySummary
  , forestRoots :: Seq DerivationId
  , buildReports :: BuildReportMap
  , startTime :: Double
  , progressState :: ProgressState
  , storePathIds :: Map StorePath StorePathId
  , derivationIds :: Map Derivation DerivationId
  , touchedIds :: DerivationSet
  , activities :: IntMap ActivityStatus
  , nixErrors :: Seq Text
  , buildPlatform :: Strict.Maybe Text
  , interestingActivities :: IntMap InterestingActivity
  , currentMessage :: Strict.Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)

data ProgressState = JustStarted | InputReceived | Finished
  deriving stock (Show, Eq, Ord, Generic)

data BuildFail = MkBuildFail
  { at :: Double
  , failType :: FailType
  }
  deriving stock (Show, Eq, Ord, Generic)

data BuildStatus
  = Unknown
  | Planned
  | Building (BuildInfo ())
  | Failed (BuildInfo BuildFail) -- End
  | Built (BuildInfo Double) -- End
  deriving stock (Show, Eq, Ord, Generic)

data BuildInfo a = MkBuildInfo
  { start :: Double
  , host :: Host
  , estimate :: Strict.Maybe Int
  , activityId :: Strict.Maybe ActivityId
  , end :: a
  }
  deriving stock (Show, Eq, Ord, Generic, Functor)

data TransferInfo a = MkTransferInfo
  { host :: Host
  , start :: Double
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
      (Strict.toStrict platform)
      mempty
      mempty

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

type NOMState a = forall m. MonadState NOMV1State m => m a

type NOMStateT m a = MonadState NOMV1State m => m a

emptyStorePathInfo :: StorePath -> StorePathInfo
emptyStorePathInfo path = MkStorePathInfo path mempty Strict.Nothing mempty

emptyDerivationInfo :: Derivation -> DerivationInfo
emptyDerivationInfo drv = MkDerivationInfo drv mempty mempty mempty Unknown mempty False mempty Strict.Nothing Strict.Nothing

getStorePathId :: StorePath -> NOMState StorePathId
getStorePathId path = do
  let newId = do
        key <- gets (CMap.nextKey . (.storePathInfos))
        modify (gfield @"storePathInfos" %~ CMap.insert key (emptyStorePathInfo path))
        modify (gfield @"storePathIds" %~ Map.insert path key)
        pure key
  gets (Map.lookup path . (.storePathIds)) >>= maybe newId pure

getDerivationId :: Derivation -> NOMState DerivationId
getDerivationId drv = do
  let newId = do
        key <- gets (CMap.nextKey . (.derivationInfos))
        modify (gfield @"derivationInfos" %~ CMap.insert key (emptyDerivationInfo drv))
        modify (gfield @"derivationIds" %~ Map.insert drv key)
        pure key
  gets (Map.lookup drv . (.derivationIds)) >>= maybe newId pure

inputStorePaths :: DerivationInfo -> NOMState (Map Text StorePathId)
inputStorePaths drv_info = do
  inputs <- forM (CSet.toList drv_info.inputSources) \source -> do
    store_path_infos <- getStorePathInfos source
    pure (store_path_infos.name.name, source)
  pure $ Map.fromList inputs

derivationToAnyOutPath :: DerivationId -> NOMState (Maybe StorePath)
derivationToAnyOutPath drv =
  gets (CMap.lookup drv . (.derivationInfos) >=> listToMaybe . Map.elems . (.outputs))
    >>= mapM (\pathId -> lookupStorePathId pathId)

outPathToDerivation :: StorePathId -> NOMState (Maybe DerivationId)
outPathToDerivation path = gets (CMap.lookup path . (.storePathInfos) >=> Strict.toLazy . (.producer))

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
  Planned -> gfield @"plannedBuilds" %~ CSet.delete drvId
  Building _ -> gfield @"runningBuilds" %~ CMap.delete drvId
  Failed _ -> gfield @"failedBuilds" %~ CMap.delete drvId
  Built _ -> gfield @"completedBuilds" %~ CMap.delete drvId

updateSummaryForDerivation :: BuildStatus -> BuildStatus -> DerivationId -> DependencySummary -> DependencySummary
updateSummaryForDerivation oldStatus newStatus drvId =
  clearDerivationIdFromSummary oldStatus drvId . case newStatus of
    Unknown -> id
    Planned -> gfield @"plannedBuilds" %~ CSet.insert drvId
    Building bi -> gfield @"runningBuilds" %~ CMap.insert drvId (void bi)
    Failed bi -> gfield @"failedBuilds" %~ CMap.insert drvId bi
    Built bi -> gfield @"completedBuilds" %~ CMap.insert drvId bi

clearStorePathsFromSummary :: Set StorePathState -> StorePathId -> DependencySummary -> DependencySummary
clearStorePathsFromSummary deleted_states path_id =
  foldMapEndo remove_deleted deleted_states
 where
  remove_deleted :: StorePathState -> DependencySummary -> DependencySummary
  remove_deleted = \case
    DownloadPlanned -> gfield @"plannedDownloads" %~ CSet.delete path_id
    Downloading _ -> gfield @"runningDownloads" %~ CMap.delete path_id
    Uploading _ -> gfield @"runningUploads" %~ CMap.delete path_id
    Downloaded _ -> gfield @"completedDownloads" %~ CMap.delete path_id
    Uploaded _ -> gfield @"completedUploads" %~ CMap.delete path_id

updateSummaryForStorePath :: Set StorePathState -> Set StorePathState -> StorePathId -> DependencySummary -> DependencySummary
updateSummaryForStorePath old_states new_states path_id =
  foldMapEndo insert_added added_states . clearStorePathsFromSummary deleted_states path_id
 where
  insert_added :: StorePathState -> DependencySummary -> DependencySummary
  insert_added = \case
    DownloadPlanned -> gfield @"plannedDownloads" %~ CSet.insert path_id
    Downloading ho -> gfield @"runningDownloads" %~ CMap.insert path_id ho
    Uploading ho -> gfield @"runningUploads" %~ CMap.insert path_id ho
    Downloaded ho -> gfield @"completedDownloads" %~ CMap.insert path_id ho
    Uploaded ho -> gfield @"completedUploads" %~ CMap.insert path_id ho
  deleted_states = Set.difference old_states new_states
  added_states = Set.difference new_states old_states
