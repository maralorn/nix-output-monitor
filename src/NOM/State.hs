module NOM.State where

import Relude

import Data.Time (UTCTime)

import Data.Generics.Product (the, typed)
import Data.Generics.Sum (_As, _Ctor)
import qualified Data.Map.Strict as Map
import NOM.Parser (Derivation (..), Host (..), StorePath (..))
import NOM.Update.Monad (
  BuildReportMap,
  MonadCacheBuildReports (getCachedBuildReports),
  MonadNow,
  getNow,
 )
import NOM.Util ((.>), (|>), (<.>>))
import Optics (preview, has, (%), only)

data StorePathState = DownloadPlanned | Downloading Host | Uploading Host | Downloaded Host | Uploaded Host
  deriving stock (Show, Eq, Ord, Read, Generic)

data DerivationInfo = MkDerivationInfo
  { outputs :: Map Text StorePath
  , inputDerivations :: Map Derivation (Set Text)
  , inputSources :: Set StorePath
  , buildStatus :: BuildStatus
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

data NOMV1State = MkNOMV1State
  { derivationInfos :: Map Derivation DerivationInfo
  , storePathInfos :: Map StorePath (NonEmpty StorePathState)
  , derivationParents :: Map Derivation (Set Derivation)
  , storePathProducers :: Map StorePath (Derivation, Text)
  , storePathInputFor :: Map StorePath (Set Derivation)
  , buildReports :: BuildReportMap
  , startTime :: UTCTime
  , errors :: [Text]
  , processState :: ProcessState
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

getRunningBuildsByHost :: Host -> NOMV1State -> Map Derivation (BuildInfo ())
getRunningBuildsByHost host = getRunningBuilds .> Map.filter (buildHost .> (==host))
getOutstandingDownloads :: NOMV1State -> Set StorePath
getOutstandingDownloads = storePathInfos .> Map.filter (elem DownloadPlanned) .> Map.keysSet
getCompletedUploads :: NOMV1State -> Map StorePath Host
getCompletedUploads =  storePathInfos .> Map.mapMaybe ((toList .> mapMaybe (preview (_Ctor @"Uploaded"))) .> listToMaybe)
getCompletedDownloads :: NOMV1State -> Map StorePath Host
getCompletedDownloads =  storePathInfos .> Map.mapMaybe ((toList .> mapMaybe (preview (_Ctor @"Downloaded"))) .> listToMaybe)
getRunningBuilds :: NOMV1State -> Map Derivation (BuildInfo ())
getRunningBuilds =  derivationInfos .> Map.mapMaybe (buildStatus .> preview (_Ctor @"Building"))
getFailedBuilds :: NOMV1State -> Map Derivation (BuildInfo (UTCTime, Int))
getFailedBuilds =  derivationInfos .> Map.mapMaybe (buildStatus .> preview (_Ctor @"Failed"))
getCompletedBuilds :: NOMV1State -> Map Derivation (BuildInfo UTCTime)
getCompletedBuilds = derivationInfos .> Map.mapMaybe (buildStatus .> preview (_Ctor @"Built"))
getOutstandingBuilds :: NOMV1State -> Set Derivation
getOutstandingBuilds =  derivationInfos .> Map.filter (buildStatus .> has (_Ctor @"Planned")) .> Map.keysSet
drv2out :: NOMV1State -> Derivation -> Maybe StorePath
drv2out s = Map.lookup "out" . outputs <=< flip Map.lookup (derivationInfos s)
out2drv :: NOMV1State -> StorePath -> Maybe Derivation
out2drv s = flip Map.lookup (storePathProducers s) <.>> fst
