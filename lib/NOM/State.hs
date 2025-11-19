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
  MonadNOMState,
  NOMState (..),
  ActivityStatus (..),
  InterestingActivity (..),
  InputDerivation (..),
  EvalInfo (..),
  getDerivationInfos,
  initalStateFromBuildPlatform,
  updateSummaryForStorePath,
  clearDerivationIdFromSummary,
  clearStorePathsFromSummary,
  getStorePathInfos,
  getRunningBuilds,
  getRunningBuildsByHost,
  lookupStorePathId,
  getStorePathId,
  getDerivationId,
  outPathToDerivation,
  derivationToAnyOutPath,
  updateSummaryForDerivation,
  inputStorePaths,
  parseOutputName,
  OutputName (..),
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Strict qualified as Strict
import Data.Text qualified as Text
import NOM.Builds (Derivation (..), FailType, Host (..), HostContext (..), StorePath (..))
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
import NOM.Util (repeatedly)
import Optics (modifying', (%~))
import Optics.TH (makeFieldLabelsNoPrefix, makePrismLabels)
import Relude

data TransferInfo a = MkTransferInfo
  { host :: Host WithContext
  , start :: Double
  , activityId :: Strict.Maybe ActivityId
  , end :: a
  }
  deriving stock (Show, Eq, Ord, Functor)

makeFieldLabelsNoPrefix ''TransferInfo

type RunningTransferInfo = TransferInfo ()

type CompletedTransferInfo = TransferInfo (Strict.Maybe Double)

data StorePathState
  = DownloadPlanned
  | Downloading RunningTransferInfo
  | Uploading RunningTransferInfo
  | Downloaded CompletedTransferInfo
  | Uploaded CompletedTransferInfo
  deriving stock (Show, Eq, Ord)

makePrismLabels ''StorePathState

data OutputName
  = Out
  | Doc
  | Dev
  | Bin
  | Info
  | Lib
  | Man
  | Dist
  | Other Text
  deriving stock (Show, Eq, Ord)

outputNames :: Map Text OutputName
outputNames =
  Map.fromList
    . fmap (\x -> (Text.toLower (show x), x))
    $ [ Out
      , Doc
      , Dev
      , Bin
      , Info
      , Lib
      , Man
      , Dist
      ]

parseOutputName :: Text -> OutputName
parseOutputName name = fromMaybe (Other name) $ Map.lookup name outputNames

data BuildFail = MkBuildFail
  { at :: Double
  , failType :: FailType
  }
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''BuildFail

data BuildInfo a = MkBuildInfo
  { start :: Double
  , host :: Host WithContext
  , estimate :: Strict.Maybe Int
  , activityId :: Strict.Maybe ActivityId
  , end :: a
  }
  deriving stock (Show, Eq, Ord, Functor)

makeFieldLabelsNoPrefix ''BuildInfo

data BuildStatus
  = Unknown
  | Planned
  | Building (BuildInfo ())
  | Failed (BuildInfo BuildFail) -- End
  | Built (BuildInfo Double) -- End
  deriving stock (Show, Eq, Ord)

makePrismLabels ''BuildStatus

type RunningBuildInfo = BuildInfo ()

type CompletedBuildInfo = BuildInfo Double

type FailedBuildInfo = BuildInfo BuildFail

type StorePathId = CacheId StorePath

type DerivationId = CacheId Derivation

type StorePathMap = CacheIdMap StorePath

type DerivationMap = CacheIdMap Derivation

type StorePathSet = CacheIdSet StorePath

type DerivationSet = CacheIdSet Derivation

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
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''DependencySummary

data InputDerivation = MkInputDerivation
  { derivation :: DerivationId
  , outputs :: Set OutputName
  }
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''InputDerivation

data DerivationInfo = MkDerivationInfo
  { name :: Derivation
  , outputs :: Map OutputName StorePathId
  , inputDerivations :: Seq InputDerivation
  , inputSources :: StorePathSet
  , buildStatus :: BuildStatus
  , dependencySummary :: DependencySummary
  , cached :: Bool
  , derivationParents :: DerivationSet
  , pname :: Strict.Maybe Text
  , platform :: Strict.Maybe Text
  }
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''DerivationInfo

data StorePathInfo = MkStorePathInfo
  { name :: StorePath
  , states :: Set StorePathState
  , producer :: Strict.Maybe DerivationId
  , inputFor :: DerivationSet
  }
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''StorePathInfo

data ActivityStatus = MkActivityStatus
  { activity :: Activity
  , phase :: Strict.Maybe Text
  , progress :: Strict.Maybe ActivityProgress
  }
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''ActivityStatus

data InterestingActivity = MkInterestingUnknownActivity
  { text :: Text
  , start :: Double
  }
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''InterestingActivity

data EvalInfo = MkEvalInfo
  { lastFileName :: Strict.Maybe Text
  , count :: Int
  , at :: Double
  }
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''EvalInfo

data ProgressState = JustStarted | InputReceived | Finished
  deriving stock (Show, Eq, Ord)

data NOMState = MkNOMState
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
  , nixTraces :: Seq Text
  , buildPlatform :: Strict.Maybe Text
  , interestingActivities :: IntMap InterestingActivity
  , evaluationState :: EvalInfo
  , successTokens :: Int
  , buildsActivity :: Strict.Maybe ActivityId
  }
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''NOMState

initalStateFromBuildPlatform :: (MonadCacheBuildReports m, MonadNow m) => Maybe Text -> m NOMState
initalStateFromBuildPlatform platform = do
  now <- getNow
  buildReports <- getCachedBuildReports
  pure
    $ MkNOMState
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
      mempty
      (Strict.toStrict platform)
      mempty
      MkEvalInfo{count = 0, at = 0, lastFileName = Strict.Nothing}
      0
      Strict.Nothing

instance Semigroup DependencySummary where
  (MkDependencySummary ls1 lm2 lm3 lm4 ls5 lm6 lm7 lm8 lm9) <> (MkDependencySummary rs1 rm2 rm3 rm4 rs5 rm6 rm7 rm8 rm9) = MkDependencySummary (ls1 <> rs1) (lm2 <> rm2) (lm3 <> rm3) (lm4 <> rm4) (ls5 <> rs5) (lm6 <> rm6) (lm7 <> rm7) (lm8 <> rm8) (lm9 <> rm9)

instance Monoid DependencySummary where
  mempty = MkDependencySummary mempty mempty mempty mempty mempty mempty mempty mempty mempty

getRunningBuilds :: (MonadNOMState m) => m (DerivationMap RunningBuildInfo)
getRunningBuilds = gets (.fullSummary.runningBuilds)

getRunningBuildsByHost :: (MonadNOMState m) => Host WithContext -> m (DerivationMap RunningBuildInfo)
getRunningBuildsByHost host = CMap.filter (\x -> x.host == host) <$> getRunningBuilds

lookupStorePathId :: (MonadNOMState m) => StorePathId -> m StorePath
lookupStorePathId pathId = (.name) <$> getStorePathInfos pathId

type MonadNOMState m = MonadState NOMState m

emptyStorePathInfo :: StorePath -> StorePathInfo
emptyStorePathInfo path = MkStorePathInfo path mempty Strict.Nothing mempty

emptyDerivationInfo :: Derivation -> DerivationInfo
emptyDerivationInfo drv = MkDerivationInfo drv mempty mempty mempty Unknown mempty False mempty Strict.Nothing Strict.Nothing

getStorePathId :: (MonadNOMState m) => StorePath -> m StorePathId
getStorePathId path = do
  let newId = do
        key <- gets (CMap.nextKey . (.storePathInfos))
        modifying' #storePathInfos $ CMap.insert key (emptyStorePathInfo path)
        modifying' #storePathIds $ Map.insert path key
        pure key
  gets (Map.lookup path . (.storePathIds)) >>= maybe newId pure

getDerivationId :: (MonadNOMState m) => Derivation -> m DerivationId
getDerivationId drv = do
  let newId = do
        key <- gets (CMap.nextKey . (.derivationInfos))
        modifying' #derivationInfos $ CMap.insert key (emptyDerivationInfo drv)
        modifying' #derivationIds $ Map.insert drv key
        pure key
  gets (Map.lookup drv . (.derivationIds)) >>= maybe newId pure

inputStorePaths :: (MonadNOMState m) => DerivationInfo -> m (Map Text StorePathId)
inputStorePaths drv_info = do
  inputs <- forM (CSet.toList drv_info.inputSources) \source -> do
    store_path_infos <- getStorePathInfos source
    pure (store_path_infos.name.name, source)
  pure $ Map.fromList inputs

derivationToAnyOutPath :: (MonadNOMState m) => DerivationId -> m (Maybe StorePath)
derivationToAnyOutPath drv =
  gets (CMap.lookup drv . (.derivationInfos) >=> listToMaybe . Map.elems . (.outputs))
    >>= mapM (\pathId -> lookupStorePathId pathId)

outPathToDerivation :: (MonadNOMState m) => StorePathId -> m (Maybe DerivationId)
outPathToDerivation path = gets (CMap.lookup path . (.storePathInfos) >=> Strict.toLazy . (.producer))

-- Only do this with derivationIds that you got via lookupDerivation
getDerivationInfos :: (MonadNOMState m) => DerivationId -> m DerivationInfo
getDerivationInfos drvId =
  fromMaybe (error "BUG: drvId is no key in derivationInfos")
    . CMap.lookup drvId
    . (.derivationInfos)
    <$> get

-- Only do this with derivationIds that you got via lookupDerivation
getStorePathInfos :: (MonadNOMState m) => StorePathId -> m StorePathInfo
getStorePathInfos storePathId =
  fromMaybe (error "BUG: storePathId is no key in storePathInfos")
    . CMap.lookup storePathId
    . (.storePathInfos)
    <$> get

clearDerivationIdFromSummary :: BuildStatus -> DerivationId -> DependencySummary -> DependencySummary
clearDerivationIdFromSummary oldStatus drvId = case oldStatus of
  Unknown -> id
  Planned -> #plannedBuilds %~ CSet.delete drvId
  Building _ -> #runningBuilds %~ CMap.delete drvId
  Failed _ -> #failedBuilds %~ CMap.delete drvId
  Built _ -> #completedBuilds %~ CMap.delete drvId

updateSummaryForDerivation :: BuildStatus -> BuildStatus -> DerivationId -> DependencySummary -> DependencySummary
updateSummaryForDerivation oldStatus newStatus drvId =
  clearDerivationIdFromSummary oldStatus drvId >>> case newStatus of
    Unknown -> id
    Planned -> #plannedBuilds %~ CSet.insert drvId
    Building bi -> #runningBuilds %~ CMap.insert drvId (void bi)
    Failed bi -> #failedBuilds %~ CMap.insert drvId bi
    Built bi -> #completedBuilds %~ CMap.insert drvId bi

clearStorePathsFromSummary :: Set StorePathState -> StorePathId -> DependencySummary -> DependencySummary
clearStorePathsFromSummary deleted_states path_id =
  repeatedly remove_deleted deleted_states
 where
  remove_deleted :: StorePathState -> DependencySummary -> DependencySummary
  remove_deleted = \case
    DownloadPlanned -> #plannedDownloads %~ CSet.delete path_id
    Downloading _ -> #runningDownloads %~ CMap.delete path_id
    Uploading _ -> #runningUploads %~ CMap.delete path_id
    Downloaded _ -> #completedDownloads %~ CMap.delete path_id
    Uploaded _ -> #completedUploads %~ CMap.delete path_id

updateSummaryForStorePath :: Set StorePathState -> Set StorePathState -> StorePathId -> DependencySummary -> DependencySummary
updateSummaryForStorePath old_states new_states path_id =
  clearStorePathsFromSummary deleted_states path_id >>> repeatedly insert_added added_states
 where
  insert_added :: StorePathState -> DependencySummary -> DependencySummary
  insert_added = \case
    DownloadPlanned -> #plannedDownloads %~ CSet.insert path_id
    Downloading ho -> #runningDownloads %~ CMap.insert path_id ho
    Uploading ho -> #runningUploads %~ CMap.insert path_id ho
    Downloaded ho -> #completedDownloads %~ CMap.insert path_id ho
    Uploaded ho -> #completedUploads %~ CMap.insert path_id ho
  deleted_states = Set.difference old_states new_states
  added_states = Set.difference new_states old_states
