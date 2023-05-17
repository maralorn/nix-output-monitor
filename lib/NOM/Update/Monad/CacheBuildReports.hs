{-# OPTIONS_GHC -Wno-missed-specialisations #-}

module NOM.Update.Monad.CacheBuildReports (
  MonadCacheBuildReports (..),
  BuildReport (..),
  BuildReportMap,
) where

import Control.Exception (IOException, catch)
import Control.Monad.Trans.Writer.CPS (WriterT)
-- cassava
import Data.Csv (FromRecord, HasHeader (NoHeader), ToRecord, decode, encode)
-- data-default
import Data.Default (def)
import Data.Map.Strict qualified as Map
-- filepath

-- lock-file

import NOM.Builds (Host (..))
import Relude
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.LockFile (
  LockingException (CaughtIOException, UnableToAcquireLockFile),
  LockingParameters (retryToAcquireLock, sleepBetweenRetries),
  RetryStrategy (NumberOfTimes),
  withLockExt,
  withLockFile,
 )

-- Exposed functions

class Monad m => MonadCacheBuildReports m where
  getCachedBuildReports :: m BuildReportMap
  updateBuildReports :: (BuildReportMap -> BuildReportMap) -> m BuildReportMap

data BuildReport = BuildReport
  { host :: Text
  , name :: Text
  , seconds :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromRecord, ToRecord)

type BuildReportMap = Map (Host, Text) Int

instance MonadCacheBuildReports IO where
  getCachedBuildReports = do
    dir <- buildReportsDir
    loadBuildReports dir
  updateBuildReports updateFunc = catch (tryUpdateBuildReports updateFunc) memptyOnLockFail

instance MonadCacheBuildReports m => MonadCacheBuildReports (StateT a m) where
  getCachedBuildReports = lift getCachedBuildReports
  updateBuildReports = lift . updateBuildReports

instance MonadCacheBuildReports m => MonadCacheBuildReports (WriterT a m) where
  getCachedBuildReports = lift getCachedBuildReports
  updateBuildReports = lift . updateBuildReports

-- Implementation

tryUpdateBuildReports :: (BuildReportMap -> BuildReportMap) -> IO BuildReportMap
tryUpdateBuildReports updateFunc = do
  dir <- buildReportsDir
  catch @IOException (createDirectoryIfMissing True dir) (const pass)
  withLockFile
    def{retryToAcquireLock = NumberOfTimes 10, sleepBetweenRetries = 500000}
    (dir </> withLockExt buildReportsFilename)
    (updateBuildReportsUnlocked updateFunc dir)

updateBuildReportsUnlocked :: (BuildReportMap -> BuildReportMap) -> FilePath -> IO BuildReportMap
updateBuildReportsUnlocked updateFunc dir = do
  reports <- updateFunc <$> loadBuildReports dir
  reports <$ saveBuildReports dir reports

memptyOnLockFail :: Monoid b => LockingException -> IO b
memptyOnLockFail = \case
  UnableToAcquireLockFile file -> mempty <$ removeFile file
  CaughtIOException{} -> pure mempty

buildReportsDir :: IO FilePath
buildReportsDir = getXdgDirectory XdgCache "nix-output-monitor"

buildReportsFilename :: FilePath
buildReportsFilename = "build-reports.csv"

saveBuildReports :: FilePath -> BuildReportMap -> IO ()
saveBuildReports dir reports = catch @IOException trySave (const pass)
 where
  trySave = do
    createDirectoryIfMissing True dir
    writeFileLBS (dir </> buildReportsFilename) (encode . toCSV $ reports)

loadBuildReports :: FilePath -> IO BuildReportMap
loadBuildReports dir = catch @IOException tryLoad (const (pure mempty))
 where
  tryLoad =
    readFileLBS (dir </> buildReportsFilename)
      <&> either (const mempty) (fromCSV . toList) . decode NoHeader

toCSV :: BuildReportMap -> [BuildReport]
toCSV = fmap (\((fromHost -> host, name), seconds) -> BuildReport{..}) . Map.assocs

fromHost :: Host -> Text
fromHost = \case
  Localhost -> ""
  Host x -> x

fromCSV :: [BuildReport] -> BuildReportMap
fromCSV = fromList . fmap (\BuildReport{..} -> ((toHost host, name), seconds))

toHost :: Text -> Host
toHost = \case
  "" -> Localhost
  x -> Host x
