{-# OPTIONS_GHC -Wno-missed-specialisations #-}

module NOM.Update.Monad.CacheBuildReports (
  MonadCacheBuildReports (..),
  BuildReport (..),
  BuildReportMap,
) where

import Control.Exception.Safe (catchAny, catchIO)
import Control.Monad.Trans.Writer.CPS (WriterT)
import Data.Csv (DefaultOrdered (..), FromNamedRecord (..), ToNamedRecord (..), decodeByName, encodeDefaultOrderedByName, header, namedRecord, (.:), (.=))
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import NOM.Builds (Host (..))
import Relude
import System.Directory (XdgDirectory (XdgState), createDirectoryIfMissing, getXdgDirectory)
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.FilePath ((<.>), (</>))

-- Exposed functions

class (Monad m) => MonadCacheBuildReports m where
  getCachedBuildReports :: m BuildReportMap
  updateBuildReports :: (BuildReportMap -> BuildReportMap) -> m BuildReportMap

timeFormat :: String
timeFormat = "%F %T"

data BuildReport = BuildReport
  { host :: Host
  , drvName :: Text
  , endTime :: UTCTime
  , buildSecs :: Int
  }
  deriving stock (Generic, Show, Eq)

csvHeaderHost, csvHeaderDrvName, csvHeaderEndTime, csvHeaderBuildSecs :: ByteString
csvHeaderHost = "hostname"
csvHeaderDrvName = "derivation name"
csvHeaderEndTime = "end time"
csvHeaderBuildSecs = "build seconds"

instance FromNamedRecord BuildReport where
  parseNamedRecord m =
    BuildReport
      . toHost
      <$> (m .: csvHeaderHost)
      <*> (m .: csvHeaderDrvName)
      <*> (parseTimeM True defaultTimeLocale timeFormat =<< m .: csvHeaderEndTime)
      <*> (m .: csvHeaderBuildSecs)

instance ToNamedRecord BuildReport where
  toNamedRecord m =
    namedRecord
      [ csvHeaderHost .= fromHost m.host
      , csvHeaderDrvName .= m.drvName
      , csvHeaderEndTime .= formatTime defaultTimeLocale timeFormat m.endTime
      , csvHeaderBuildSecs .= m.buildSecs
      ]

instance DefaultOrdered BuildReport where
  headerOrder _ =
    header
      [ csvHeaderHost
      , csvHeaderDrvName
      , csvHeaderEndTime
      , csvHeaderBuildSecs
      ]

type BuildReportMap = Map (Host, Text) (Map UTCTime Int)

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
    writeFileLBS (dir </> buildReportsFilename) (encodeDefaultOrderedByName . toCSV $ reports)

loadBuildReports :: FilePath -> IO BuildReportMap
loadBuildReports dir = catchIO tryLoad mempty
 where
  tryLoad =
    readFileLBS (dir </> buildReportsFilename)
      <&> ( decodeByName
              >>> either mempty snd
              >>> toList
              >>> fromCSV
          )

toCSV :: BuildReportMap -> [BuildReport]
toCSV =
  fmap
    (\((host, drvName), (endTime, buildSecs)) -> BuildReport{..})
    . traverse Map.assocs
    <=< Map.assocs

fromHost :: Host -> Text
fromHost = \case
  Localhost -> ""
  Host x -> x

fromCSV :: [BuildReport] -> BuildReportMap
fromCSV = Map.fromListWith Map.union . fmap (\BuildReport{..} -> ((host, drvName), Map.singleton endTime buildSecs))

toHost :: Text -> Host
toHost = \case
  "" -> Localhost
  x -> Host x
