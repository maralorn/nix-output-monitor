module NOM.State where

import Relude

import Data.Time (UTCTime, getCurrentTime)

import NOM.Parser (Derivation (..), Host (..), StorePath (..))
import NOM.Update.Monad
    ( BuildReportMap, MonadCacheBuildReports(getCachedBuildReports) )
import Data.Tree (Forest)

data DerivationNode = DerivationNode
  { derivation :: Derivation
  , state :: Maybe (Host, BuildStatus)
  }
  deriving stock (Show, Eq, Ord, Read, Generic)
data StorePathNode
  = StorePathNode
      { path :: StorePath
      , derivation :: Maybe (Derivation, Text)
      , state :: NonEmpty StorePathState
      }
  deriving stock (Show, Eq, Ord, Read, Generic)

data StorePathState = DownloadPlanned | Downloading Host | Uploading Host | Downloaded Host | Uploaded Host
  deriving stock (Show, Eq, Ord, Read, Generic)

data DerivationInfo = MkDerivationInfo
  { outputs :: Map Text StorePath
  , inputDrvs :: Map Derivation (Set Text)
  , inputSrcs :: Set StorePath
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

type BuildForest = Forest (Either DerivationNode StorePathNode)
type LinkedBuildTree = Forest (Either (Either DerivationNode StorePathNode) (Either Derivation StorePath))
type LinkTreeNode = Either (Either DerivationNode StorePathNode) (Either Derivation StorePath)
type SummariesTreeNode = (LinkTreeNode, Summaries)
type Summaries = Set Summary

data Summary
  = SummaryBuildDone Derivation
  | SummaryBuildWaiting Derivation
  | SummaryBuildRunning Derivation
  | SummaryBuildFailed Derivation
  | SummaryDownloadWaiting StorePath
  | SummaryDownloadRunning StorePath
  | SummaryDownloadDone StorePath
  | SummaryUploadRunning StorePath
  | SummaryUploadDone StorePath
  deriving (Eq, Ord, Show, Read, Generic)
type SummaryForest = Forest SummariesTreeNode

data BuildState = BuildState
  { outstandingBuilds :: Set Derivation
  , outstandingDownloads :: Set StorePath
  , plannedCopies :: Int
  , runningBuilds :: Map Host (Set (Derivation, (UTCTime, Maybe Int)))
  , completedBuilds :: Map Host (Set Derivation)
  , failedBuilds :: Map Host (Set (Derivation, Int, Int))
  , completedDownloads :: Map Host (Set StorePath)
  , completedUploads :: Map Host (Set StorePath)
  , outputToDerivation :: Map StorePath Derivation
  , derivationInfos :: Map Derivation DerivationInfo
  , derivationParents :: Map Derivation (Set Derivation)
  , lastPlannedBuild :: Maybe Derivation
  , buildReports :: BuildReportMap
  , buildForest :: BuildForest
  , cachedShowForest :: SummaryForest
  , startTime :: UTCTime
  , errors :: [Text]
  , inputReceived :: Bool
  }
  deriving stock (Show, Eq, Ord, Read, Generic)

data BuildStatus
  = Building
      { buildStart :: UTCTime
      , buildNeeded :: Maybe Int
      }
  | Failed
      { buildDuration :: Int
      , buildExitCode :: Int
      , buildEnd :: UTCTime
      }
  | Built
      { buildDuration :: Int
      , buildEnd :: UTCTime
      }
  deriving (Show, Eq, Ord, Read, Generic)

initalState :: IO BuildState
initalState = do
  now <- getCurrentTime
  buildReports <- getCachedBuildReports
  pure $
    BuildState
      mempty
      mempty
      0
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      Nothing
      buildReports
      mempty
      mempty
      now
      mempty
      False
