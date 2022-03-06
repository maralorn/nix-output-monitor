module NOM.State where

import Data.Generics.Product (HasField (field))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (UTCTime)
import NOM.Parser (Derivation (..), Host (..), StorePath (..))
import NOM.State.CacheId (CacheId)
import NOM.State.CacheId.Map (CacheIdMap)
import qualified NOM.State.CacheId.Map as CMap
import NOM.State.CacheId.Set (CacheIdSet)
import qualified NOM.State.CacheId.Set as CSet
import NOM.Update.Monad
  ( BuildReportMap,
    MonadCacheBuildReports (getCachedBuildReports),
    MonadNow,
    getNow,
  )
import NOM.Util (foldMapEndo, (.>), (<.>>), (<|>>), (|>))
import Optics ((%~))
import Relude

data StorePathState = DownloadPlanned | Downloading Host | Uploading Host | Downloaded Host | Uploaded Host
  deriving stock (Show, Eq, Ord, Read, Generic)

data DerivationInfo = MkDerivationInfo
  { derivationName :: Derivation,
    outputs :: Map Text StorePathId,
    inputDerivations :: Seq (DerivationId, Set Text),
    inputSources :: StorePathSet,
    buildStatus :: BuildStatus,
    dependencySummary :: DependencySummary,
    cached :: Bool,
    derivationParents :: DerivationSet
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

type StorePathId = CacheId StorePath

type DerivationId = CacheId Derivation

type StorePathMap = CacheIdMap StorePath

type DerivationMap = CacheIdMap Derivation

type StorePathSet = CacheIdSet StorePath

type DerivationSet = CacheIdSet Derivation

data StorePathInfo = MkStorePathInfo
  { storePathName :: StorePath,
    storePathStates :: Set StorePathState,
    storePathProducer :: Maybe DerivationId,
    storePathInputFor :: DerivationSet
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

type RunningBuildInfo = BuildInfo ()

type CompletedBuildInfo = BuildInfo UTCTime

type FailedBuildInfo = BuildInfo (UTCTime, Int)

data DependencySummary = MkDependencySummary
  { plannedBuilds :: DerivationSet,
    runningBuilds :: DerivationMap RunningBuildInfo,
    completedBuilds :: DerivationMap CompletedBuildInfo,
    failedBuilds :: DerivationMap FailedBuildInfo,
    plannedDownloads :: StorePathSet,
    completedDownloads :: StorePathMap Host,
    completedUploads :: StorePathMap Host
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

data NOMV1State = MkNOMV1State
  { derivationInfos :: DerivationMap DerivationInfo,
    storePathInfos :: StorePathMap StorePathInfo,
    fullSummary :: DependencySummary,
    forestRoots :: Seq DerivationId,
    buildReports :: BuildReportMap,
    startTime :: UTCTime,
    errors :: [Text],
    processState :: ProcessState,
    storePathIds :: Map StorePath StorePathId,
    derivationIds :: Map Derivation DerivationId
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
  { buildStart :: UTCTime,
    buildHost :: Host,
    buildEstimate :: Maybe Int,
    buildEnd :: a
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

-}

instance Semigroup DependencySummary where
  (MkDependencySummary ls1 lm2 lm3 lm4 ls5 lm6 lm7) <> (MkDependencySummary rs1 rm2 rm3 rm4 rs5 rm6 rm7) = MkDependencySummary (ls1 <> rs1) (lm2 <> rm2) (lm3 <> rm3) (lm4 <> rm4) (ls5 <> rs5) (lm6 <> rm6) (lm7 <> rm7)

instance Monoid DependencySummary where
  mempty = MkDependencySummary mempty mempty mempty mempty mempty mempty mempty

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
getRunningBuildsByHost host = getRunningBuilds <|>> CMap.filter (buildHost .> (== host))

lookupStorePathId :: StorePathId -> NOMState StorePath
lookupStorePathId = getStorePathInfos <.>> storePathName

lookupDerivationId :: DerivationId -> NOMState Derivation
lookupDerivationId = getDerivationInfos <.>> derivationName

type NOMState a = forall m. MonadState NOMV1State m => m a

type NOMStateT m a = MonadState NOMV1State m => m a

emptyStorePathInfo :: StorePath -> StorePathInfo
emptyStorePathInfo path = MkStorePathInfo path mempty Nothing mempty

emptyDerivationInfo :: Derivation -> DerivationInfo
emptyDerivationInfo drv = MkDerivationInfo drv mempty mempty mempty Unknown mempty False mempty

getStorePathId :: StorePath -> NOMState StorePathId
getStorePathId path = do
  let newId = do
        key <- gets (storePathInfos .> CMap.nextKey)
        modify (field @"storePathInfos" %~ CMap.insert key (emptyStorePathInfo path))
        modify (field @"storePathIds" %~ Map.insert path key)
        pure key
  gets (storePathIds .> Map.lookup path) >>= maybe newId pure

getDerivationId :: Derivation -> NOMState DerivationId
getDerivationId drv = do
  let newId = do
        key <- gets (derivationInfos .> CMap.nextKey)
        modify (field @"derivationInfos" %~ CMap.insert key (emptyDerivationInfo drv))
        modify (field @"derivationIds" %~ Map.insert drv key)
        pure key
  gets (derivationIds .> Map.lookup drv) >>= maybe newId pure

drv2out :: DerivationId -> NOMState (Maybe StorePath)
drv2out drv =
  gets (derivationInfos .> CMap.lookup drv >=> outputs .> Map.lookup "out")
    >>= mapM lookupStorePathId

out2drv :: StorePathId -> NOMState (Maybe DerivationId)
out2drv path = gets (storePathInfos .> CMap.lookup path >=> storePathProducer)

-- Only do this with derivationIds that you got via lookupDerivation
getDerivationInfos :: DerivationId -> NOMState DerivationInfo
getDerivationInfos drvId =
  get
    <|>> derivationInfos
    .> CMap.lookup drvId
    .> fromMaybe (error "BUG: drvId is no key in derivationInfos")

-- Only do this with derivationIds that you got via lookupDerivation
getStorePathInfos :: StorePathId -> NOMState StorePathInfo
getStorePathInfos storePathId =
  get
    <|>> storePathInfos
    .> CMap.lookup storePathId
    .> fromMaybe (error "BUG: storePathId is no key in storePathInfos")

reportError :: Text -> NOMState ()
reportError msg = modify' (field @"errors" %~ (msg :))

updateSummaryForDerivation :: BuildStatus -> BuildStatus -> DerivationId -> DependencySummary -> DependencySummary
updateSummaryForDerivation oldStatus newStatus drvId = removeOld .> addNew
  where
    removeOld = case oldStatus of
      Unknown -> id
      Planned -> field @"plannedBuilds" %~ CSet.delete drvId
      Building _ -> field @"runningBuilds" %~ CMap.delete drvId
      Failed _ -> error "BUG: Failed builds need to stay failed"
      Built _ -> error "BUG: Completed builds need to stay completed"
    addNew = case newStatus of
      Unknown -> id
      Planned -> field @"plannedBuilds" %~ CSet.insert drvId
      Building bi -> field @"runningBuilds" %~ CMap.insert drvId bi
      Failed bi -> field @"failedBuilds" %~ CMap.insert drvId bi
      Built bi -> field @"completedBuilds" %~ CMap.insert drvId bi

updateSummaryForStorePath :: Set StorePathState -> Set StorePathState -> StorePathId -> DependencySummary -> DependencySummary
updateSummaryForStorePath oldStates newStates pathId =
  foldMapEndo remove_deleted deletedStates
    .> foldMapEndo insert_added addedStates
  where
    remove_deleted :: StorePathState -> DependencySummary -> DependencySummary
    remove_deleted = \case
      DownloadPlanned -> field @"plannedDownloads" %~ CSet.delete pathId
      Downloading _ -> error "BUG: Downloading state is yet unsupported"
      Uploading _ -> error "BUG: Uploading state is yet unsupported"
      Downloaded _ -> error "BUG: Don’t remove a completed download"
      Uploaded _ -> error "BUG: Don‘t remove a completed upload"
    insert_added :: StorePathState -> DependencySummary -> DependencySummary
    insert_added = \case
      DownloadPlanned -> field @"plannedDownloads" %~ CSet.insert pathId
      Downloading _ -> error "BUG: Downloading state is yet unsupported"
      Uploading _ -> error "BUG: Uploading state is yet unsupported"
      Downloaded ho -> field @"completedDownloads" %~ CMap.insert pathId ho
      Uploaded ho -> field @"completedUploads" %~ CMap.insert pathId ho
    deletedStates = Set.difference oldStates newStates |> toList
    addedStates = Set.difference newStates oldStates |> toList