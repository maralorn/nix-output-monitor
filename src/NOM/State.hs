module NOM.State where

import Relude

import Data.Time (UTCTime, getCurrentTime)

import NOM.Parser (Derivation (..), Host (..), StorePath (..))
import NOM.State.Tree ( Tree )
import NOM.Update.Monad
    ( BuildReportMap, MonadCacheBuildReports(getCachedBuildReports) )

data DerivationInfo = MkDerivationInfo
  { outputs :: Map Text StorePath
  , inputDrvs :: Map Derivation (Set Text)
  , inputSrcs :: Set StorePath
  }
  deriving stock (Show, Eq, Ord, Read)

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
  , buildForest :: [Tree Derivation Build]
  , startTime :: UTCTime
  , errors :: [Text]
  , inputReceived :: Bool
  }
  deriving stock (Show, Eq, Ord, Read)

data Build = MkBuild
  { buildHost :: Host
  , buildDerivation :: Derivation
  , buildStatus :: BuildStatus
  }
  deriving (Show, Eq, Ord, Read)

data BuildStatus
  = Building
      { buildStart :: UTCTime
      , buildNeeded :: Maybe Int
      }
  | Failed
      { buildDuration :: Int
      , buildExitCode :: Int
      }
  deriving (Show, Eq, Ord, Read)

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
      now
      mempty
      False
