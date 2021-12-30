module NOM.Update.Monad.CacheBuildReports (
  MonadCacheBuildReports (..),
  BuildReport (..),
  BuildReportMap,
) where

import Relude

import Control.Exception (IOException, catch)
import Data.Csv (FromRecord, HasHeader (NoHeader), ToRecord, decode, encode)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.LockFile (
  LockingException (CaughtIOException, UnableToAcquireLockFile),
  LockingParameters (retryToAcquireLock, sleepBetweenRetries),
  RetryStrategy (NumberOfTimes),
  withLockExt,
  withLockFile,
 )

import NOM.Parser (Host (..))

-- Exposed functions

class Monad m => MonadCacheBuildReports m where
  getCachedBuildReports :: m BuildReportMap
  updateBuildReports :: (BuildReportMap -> BuildReportMap) -> m BuildReportMap

data BuildReport = BuildReport {reportHost :: !Text, reportName :: !Text, reportSeconds :: !Int}
  deriving (Generic, Show, Read, Eq, FromRecord, ToRecord)

type BuildReportMap = Map (Host, Text) Int

instance MonadCacheBuildReports IO where
  getCachedBuildReports = do
    dir <- buildReportsDir
    loadBuildReports dir
  updateBuildReports updateFunc = catch (tryUpdateBuildReports updateFunc) memptyOnLockFail

-- Implementation

tryUpdateBuildReports :: (BuildReportMap -> BuildReportMap) -> IO BuildReportMap
tryUpdateBuildReports updateFunc = do
  dir <- buildReportsDir
  withLockFile
    def{retryToAcquireLock = NumberOfTimes 10, sleepBetweenRetries = 500000}
    (dir </> withLockExt buildReportsFilename)
    (updateBuildReportsUnlocked updateFunc dir)

updateBuildReportsUnlocked :: (BuildReportMap -> BuildReportMap) -> FilePath -> IO BuildReportMap
updateBuildReportsUnlocked updateFunc dir = do
  !reports <- updateFunc <$> loadBuildReports dir
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
toCSV = fmap (\((fromHost -> reportHost, reportName), reportSeconds) -> BuildReport{..}) . Map.assocs

fromHost :: Host -> Text
fromHost = \case
  Localhost -> ""
  Host x -> x

fromCSV :: [BuildReport] -> BuildReportMap
fromCSV = fromList . fmap (\BuildReport{..} -> ((toHost reportHost, reportName), reportSeconds))

toHost :: Text -> Host
toHost = \case
  "" -> Localhost
  x -> Host x
