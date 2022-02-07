module NOM.State where

import Relude

import Data.Time (UTCTime)

import Data.Generics.Product (the, typed)
import Data.Generics.Sum (_As, _Ctor)
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
import Optics (has, only, preview, (%))
import Relude.Extra (toSnd, traverseToSnd)

data StorePathState = DownloadPlanned | Downloading Host | Uploading Host | Downloaded Host | Uploaded Host
  deriving stock (Show, Eq, Ord, Read, Generic)

data DerivationInfo = MkDerivationInfo
  { derivationName :: Derivation
  , outputs :: Vector (StorePathId, Text)
  , -- True if this node is the first occurrence in the tree
    inputDerivations :: Vector (Bool, DerivationId, Vector Text)
  , inputSources :: Vector StorePathId
  , buildStatus :: BuildStatus
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

newtype StorePathId = MkStorePathId {storePathId :: Int}
  deriving stock (Show, Eq, Ord, Read, Generic)
newtype DerivationId = MkDerivationId {derivationId :: Int}
  deriving stock (Show, Eq, Ord, Read, Generic)

data StorePathInfo = MkStorePathInfo
  { storePathName :: StorePath
  , storePathStates :: NonEmpty StorePathState
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

data DependencySummary = MkDependencySummary
  { plannedBuilds :: IntSet
  , runningBuilds :: IntMap (BuildInfo ())
  , completedBuilds :: IntMap (BuildInfo UTCTime)
  , failedBuilds :: IntMap (BuildInfo (UTCTime, Int))
  , plannedDownloads :: IntSet
  , completedDownloads :: IntMap Host
  , completedUploads :: IntMap Host
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

data NOMV1State = MkNOMV1State
  { derivationInfos :: IntMap DerivationInfo
  , storePathInfos :: IntMap StorePathInfo
  , derivationParents :: IntMap IntSet
  , storePathProducers :: IntMap DerivationId
  , storePathInputFor :: IntMap IntSet
  , dependencySummary :: DependencySummary
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
  | Built (BuildInfo UTCTime) -- end
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
      buildReports
      now
      mempty
      JustStarted
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

instance Monoid DependencySummary where
  mempty = MkDependencySummary mempty mempty mempty mempty mempty mempty mempty

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

topDrvs :: NOMV1State -> [Derivation]
topDrvs s = Map.filter Set.null (derivationParents s) |> Map.keys

getSummary :: NOMV1State -> Maybe Derivation -> DependencySummary
getSummary s = go
 where
  go = memo innerGo
  innerGo = \case
    Just drv -> (drvDeps s drv <|>> sumDrv s) <> (pathDeps s drv <|>> sumPath s) <> (drvDeps s drv <|>> Just .> go) |> fold
    Nothing -> let t = topDrvs s in (t <|>> Just .> go) <> (t <|>> sumDrv s) |> fold

getRunningBuildsByHost :: Host -> NOMV1State -> Map Derivation (BuildInfo ())
getRunningBuildsByHost host = getRunningBuilds .> Map.filter (buildHost .> (== host))
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
drv2out :: NOMV1State -> Derivation -> Maybe StorePath
drv2out s = Map.lookup "out" . outputs <=< flip Map.lookup (derivationInfos s)
out2drv :: NOMV1State -> StorePath -> Maybe Derivation
out2drv s = flip Map.lookup (storePathProducers s) <.>> fst
