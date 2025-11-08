{-# OPTIONS_GHC -Wno-missed-specialisations #-}

module NOM.Update.Monad.CacheBuildReports (
  MonadCacheBuildReports (..),
  BuildReport (..),
  BuildReportMap,
) where

import Control.Exception.Safe (catchAny, catchIO)
import Control.Monad.Trans.Writer.CPS (WriterT)
import Data.Csv (FromRecord, HasHeader (NoHeader), ToRecord, decode, encode)
import Data.Map.Strict qualified as Map
import NOM.Builds (Host (..))
import Relude
import System.Directory (XdgDirectory (XdgState), createDirectoryIfMissing, getXdgDirectory)
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.FilePath ((<.>), (</>))

-- Exposed functions

class (Monad m) => MonadCacheBuildReports m where
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
  updateBuildReports updateFunc = catchAny (tryUpdateBuildReports updateFunc) mempty

instance (MonadCacheBuildReports m) => MonadCacheBuildReports (StateT a m) where
  getCachedBuildReports = lift getCachedBuildReports
  updateBuildReports = lift . updateBuildReports

instance (MonadCacheBuildReports m) => MonadCacheBuildReports (WriterT a m) where
  getCachedBuildReports = lift getCachedBuildReports
  updateBuildReports = lift . updateBuildReports

-- Implementation

tryUpdateBuildReports :: (BuildReportMap -> BuildReportMap) -> IO BuildReportMap
tryUpdateBuildReports updateFunc = do
  dir <- buildReportsDir
  catchIO (createDirectoryIfMissing True dir) (const pass)
  withFileLock
    (dir </> buildReportsFilename <.> "lock")
    Exclusive
    (const $ updateBuildReportsUnlocked updateFunc dir)

updateBuildReportsUnlocked :: (BuildReportMap -> BuildReportMap) -> FilePath -> IO BuildReportMap
updateBuildReportsUnlocked updateFunc dir = do
  reports <- updateFunc <$> loadBuildReports dir
  reports <$ saveBuildReports dir reports

buildReportsDir :: IO FilePath
buildReportsDir = getXdgDirectory XdgState "nix-output-monitor"

buildReportsFilename :: FilePath
buildReportsFilename = "build-reports.csv"

saveBuildReports :: FilePath -> BuildReportMap -> IO ()
saveBuildReports dir reports = catchIO trySave mempty
 where
  trySave = do
    createDirectoryIfMissing True dir
    writeFileLBS (dir </> buildReportsFilename) (encode . toCSV $ reports)

loadBuildReports :: FilePath -> IO BuildReportMap
loadBuildReports dir = catchIO tryLoad mempty
 where
  tryLoad =
    readFileLBS (dir </> buildReportsFilename)
      <&> ( decode NoHeader
              >>> either mempty (fromCSV . toList)
          )

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
