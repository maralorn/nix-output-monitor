module NOM.State where

import Relude

import Data.Time (UTCTime, getCurrentTime)
import Data.Tree (Forest)

import NOM.Parser (Derivation (..), Host (..), StorePath (..))
import NOM.Update.Monad
    ( BuildReportMap, MonadCacheBuildReports(getCachedBuildReports) )

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

type Node = Either DerivationNode StorePathNode
type Link = Either Derivation StorePath
type BuildForest = Forest Node
type LinkTreeNode = Either Node Link
type LinkedBuildTree = Forest LinkTreeNode
type SummaryTreeNode = (LinkTreeNode, Summary)
type SummaryForest = Forest SummaryTreeNode
type Summary = Set Node

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
      buildReports
      mempty
      mempty
      now
      mempty
      False
