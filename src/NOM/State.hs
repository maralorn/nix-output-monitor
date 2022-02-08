{-# LANGUAGE OverloadedLabels #-}

module NOM.State where

import Relude

import Data.Time (UTCTime)

import Data.Generics.Product (the, typed)
import Data.Generics.Sum (_As, _Ctor)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.MemoTrie (HasTrie, memo)
import qualified Data.Set as Set
import Data.Vector (Vector)
import NOM.Parser (Derivation (..), Host (..), StorePath (..))
import NOM.Update.Monad (
  BuildReportMap,
  MonadCacheBuildReports (getCachedBuildReports),
  MonadNow,
  getNow,
 )
import NOM.Util ((.>), (<.>>), (<|>>), (|>))
import Optics (coerced, has, only, preview, review, view, (%), (^.))
import Relude.Extra (toSnd, traverseToSnd)

data StorePathState = DownloadPlanned | Downloading Host | Uploading Host | Downloaded Host | Uploaded Host
  deriving stock (Show, Eq, Ord, Read, Generic)

data DerivationInfo = MkDerivationInfo
  { derivationName :: Derivation
  , outputs :: Map Text StorePathId
  , -- True if this node is the first occurrence in the tree
    inputDerivations :: Vector (DerivationId, DisplayState, Vector Text)
  , inputSources :: StorePathSet
  , buildStatus :: BuildStatus
  , dependencySummary :: DependencySummary
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

data DisplayState = DisplayNode | OptionalNode | LinkOnly
  deriving stock (Show, Eq, Ord, Read, Generic)

type StorePathId = CacheId StorePath
type DerivationId = CacheId Derivation
type StorePathMap = CacheIdMap StorePath
type DerivationMap = CacheIdMap Derivation
type StorePathSet = CacheIdSet StorePath
type DerivationSet = CacheIdSet Derivation

newtype CacheId b = MkCacheId {cidToInt :: Int}
  deriving stock (Show, Eq, Ord, Read, Generic)

newtype CacheIdSet b = MkCacheIdSet {cidSet :: IntSet}
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving newtype (Semigroup, Monoid)
newtype CacheIdMap b a = MkCacheIdMap {cidMap :: IntMap a}
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving newtype (Semigroup, Monoid)

data StorePathInfo = MkStorePathInfo
  { storePathName :: StorePath
  , storePathStates :: NonEmpty StorePathState
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

type RunningBuildInfo = BuildInfo ()
type CompletedBuildInfo = BuildInfo UTCTime
type FailedBuildInfo = BuildInfo (UTCTime, Int)

data DependencySummary = MkDependencySummary
  { plannedBuilds :: DerivationSet
  , runningBuilds :: DerivationMap RunningBuildInfo
  , completedBuilds :: DerivationMap CompletedBuildInfo
  , failedBuilds :: DerivationMap FailedBuildInfo
  , plannedDownloads :: StorePathSet
  , completedDownloads :: StorePathMap Host
  , completedUploads :: StorePathMap Host
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

data NOMV1State = MkNOMV1State
  { derivationInfos :: DerivationMap DerivationInfo
  , storePathInfos :: StorePathMap StorePathInfo
  , derivationParents :: DerivationMap DerivationSet
  , storePathProducers :: StorePathMap DerivationId
  , storePathInputFor :: StorePathMap DerivationSet
  , fullSummary :: DependencySummary
  , forestRoots :: Vector DerivationId
  , buildReports :: BuildReportMap
  , startTime :: UTCTime
  , errors :: [Text]
  , processState :: ProcessState
  , storePathIds :: Map StorePath StorePathId
  , derivationIds :: Map Derivation DerivationId
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

data ProcessState = JustStarted | InputReceived | Finished
  deriving stock (Show, Eq, Ord, Read, Generic)

data BuildStatus
  = Unknown
  | Planned
  | Building (BuildInfo ())
  | Failed (BuildInfo (UTCTime, Int)) -- End and ExitCode
  | Built (BuildInfo UTCTime) -- End
  deriving (Show, Eq, Ord, Read, Generic)

data BuildInfo a = MkBuildInfo
  { buildStart :: UTCTime
  , buildHost :: Host
  , buildEstimate :: Maybe Int
  , buildEnd :: a
  }
  deriving (Show, Eq, Ord, Read, Generic, Functor)

initalState :: (MonadCacheBuildReports m, MonadNow m) => m NOMV1State
initalState = do
  now <- getNow
  buildReports <- getCachedBuildReports
  pure $
    MkNOMV1State
      mempty
      mempty
      mempty
      mempty
      mempty
      emptySummary
      mempty
      buildReports
      now
      mempty
      JustStarted
      mempty
      mempty

{-
pathDeps :: NOMV1State -> Derivation -> [StorePath]
pathDeps s drv =
  Map.lookup drv (derivationInfos s)
    |> maybe
      mempty
      ( toSnd (outputs .> Map.elems)
          .> first (inputSources .> toList)
          .> uncurry (<>)
      )

drvDeps :: NOMV1State -> Derivation -> [Derivation]
drvDeps s drv =
  Map.lookup drv (derivationInfos s)
    |> maybe
      mempty
      ( inputDerivations
          .> Map.keys
      )

instance Semigroup DependencySummary where
  (MkDependencySummary ls1 lm2 lm3 lm4 ls5 lm6 lm7) <> (MkDependencySummary rs1 rm2 rm3 rm4 rs5 rm6 rm7) = MkDependencySummary (ls1 <> rs1) (lm2 <> rm2) (lm3 <> rm3) (lm4 <> rm4) (ls5 <> rs5) (lm6 <> rm6) (lm7 <> rm7)

-}

emptySummary :: DependencySummary
emptySummary = MkDependencySummary mempty mempty mempty mempty mempty mempty mempty

{-
instance Monoid DependencySummary where
  mempty =
sumDrv :: NOMV1State -> Derivation -> DependencySummary
sumDrv s drv
  | Just (buildStatus -> status) <- Map.lookup drv (derivationInfos s) =
    mempty
      { plannedBuilds = if has (_Ctor @"Planned") status then one drv else mempty
      , runningBuilds = preview (_Ctor @"Building") status |> maybe mempty (Map.singleton drv)
      , completedBuilds = preview (_Ctor @"Built") status |> maybe mempty (Map.singleton drv)
      , failedBuilds = preview (_Ctor @"Failed") status |> maybe mempty (Map.singleton drv)
      }
  | otherwise = mempty

sumPath :: NOMV1State -> StorePath -> DependencySummary
sumPath s path
  | Just (toList -> infos) <- Map.lookup path (storePathInfos s) =
    mempty
      { completedDownloads = mapMaybe (preview (_Ctor @"Downloaded")) infos |> viaNonEmpty head .> maybe mempty (Map.singleton path)
      , plannedDownloads = if any (has (_Ctor @"DownloadPlanned")) infos then one path else mempty
      , completedUploads = mapMaybe (preview (_Ctor @"Uploaded")) infos |> viaNonEmpty head .> maybe mempty (Map.singleton path)
      }
  | otherwise = mempty
o

topDrvs :: NOMV1State -> [Derivation]
topDrvs s = Map.filter Set.null (derivationParents s) |> Map.keys

getSummary :: NOMV1State -> Maybe Derivation -> DependencySummary
getSummary s = go
 where
  go = memo innerGo
  innerGo = \case
    Just drv -> (drvDeps s drv <|>> sumDrv s) <> (pathDeps s drv <|>> sumPath s) <> (drvDeps s drv <|>> Just .> go) |> fold
    Nothing -> let t = topDrvs s in (t <|>> Just .> go) <> (t <|>> sumDrv s) |> fold

getOutstandingDownloads :: NOMV1State -> Set StorePath
getOutstandingDownloads = storePathInfos .> Map.filter (elem DownloadPlanned) .> Map.keysSet
getCompletedUploads :: NOMV1State -> Map StorePath Host
getCompletedUploads = storePathInfos .> Map.mapMaybe ((toList .> mapMaybe (preview (_Ctor @"Uploaded"))) .> listToMaybe)
getCompletedDownloads :: NOMV1State -> Map StorePath Host
getCompletedDownloads = storePathInfos .> Map.mapMaybe ((toList .> mapMaybe (preview (_Ctor @"Downloaded"))) .> listToMaybe)
getRunningBuilds :: NOMV1State -> Map Derivation (BuildInfo ())
getRunningBuilds = derivationInfos .> Map.mapMaybe (buildStatus .> preview (_Ctor @"Building"))
getFailedBuilds :: NOMV1State -> Map Derivation (BuildInfo (UTCTime, Int))
getFailedBuilds = derivationInfos .> Map.mapMaybe (buildStatus .> preview (_Ctor @"Failed"))
getCompletedBuilds :: NOMV1State -> Map Derivation (BuildInfo UTCTime)
getCompletedBuilds = derivationInfos .> Map.mapMaybe (buildStatus .> preview (_Ctor @"Built"))
getOutstandingBuilds :: NOMV1State -> Set Derivation
getOutstandingBuilds = derivationInfos .> Map.filter (buildStatus .> has (_Ctor @"Planned")) .> Map.keysSet
-}
getRunningBuilds :: NOMState (DerivationMap RunningBuildInfo)
getRunningBuilds = gets (fullSummary .> runningBuilds)
getRunningBuildsByHost :: Host -> NOMState (DerivationMap RunningBuildInfo)
getRunningBuildsByHost host = getRunningBuilds <|>> filterMap (buildHost .> (== host))

filterMap :: (a -> Bool) -> CacheIdMap b a -> CacheIdMap b a
filterMap p = coerce .> IntMap.filter p .> coerce

lookupCacheId :: Ord a => CacheId b -> CacheIdMap b a -> Maybe a
lookupCacheId i = coerce .> IntMap.lookup (coerce i)

lookupStorePathId :: StorePathId -> NOMState (Maybe StorePath)
lookupStorePathId s = gets (storePathInfos .> lookupCacheId s <.>> storePathName)
lookupDerivationId :: DerivationId -> NOMState (Maybe Derivation)
lookupDerivationId d = gets (derivationInfos .> lookupCacheId d <.>> derivationName)

type NOMState = State NOMV1State

getStorePathId :: StorePath -> NOMState StorePathId
getStorePathId = _
getDerivationId :: StorePath -> NOMState DerivationId
getDerivationId = _

drv2out :: DerivationId -> NOMState (Maybe StorePath)
drv2out drv =
  gets (derivationInfos .> lookupCacheId drv >=> outputs .> Map.lookup "out")
    >>= mapM lookupStorePathId <.>> join
out2drv :: StorePathId -> NOMState (Maybe DerivationId)
out2drv path = gets (storePathProducers .> lookupCacheId path)
