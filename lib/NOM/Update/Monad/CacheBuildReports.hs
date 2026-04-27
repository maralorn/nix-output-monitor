{-# OPTIONS_GHC -Wno-missed-specialisations #-}

module NOM.Update.Monad.CacheBuildReports (
  MonadCacheBuildReports (..),
  BuildReport (..),
  BuildReportContext (..),
  BuildReportKey,
  BuildReportMap,
) where

import Control.Exception.Safe (catchAny, catchIO)
import Control.Monad.Trans.Writer.CPS (WriterT)
import Data.Csv (DefaultOrdered (..), FromField (..), FromNamedRecord (..), NamedRecord, Parser, ToNamedRecord (..), decodeByName, encodeDefaultOrderedByName, header, namedRecord, (.:), (.=))
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import NOM.Builds (Host (..), HostContext (..))
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
import System.Directory (XdgDirectory (XdgState), createDirectoryIfMissing, getXdgDirectory)
import System.FileLock (SharedExclusive (Exclusive), withFileLock)
import System.FilePath ((<.>), (</>))

-- Exposed functions

class (Monad m) => MonadCacheBuildReports m where
  getCachedBuildReports :: m BuildReportMap
  updateBuildReports :: (BuildReportMap -> BuildReportMap) -> m BuildReportMap

type BuildReportKey = (Host WithoutContext, Text, BuildReportContext)

type BuildReportMap = Map BuildReportKey (Map UTCTime Int)

data BuildReportContext = BuildReportContext
  { buildPlatform :: Maybe Text
  , targetPlatform :: Maybe Text
  }
  deriving stock (Show, Eq, Ord)

data BuildReport = BuildReport
  { host :: Host WithoutContext
  , drvName :: Text
  , buildPlatform :: Maybe Text
  , targetPlatform :: Maybe Text
  , endTime :: UTCTime
  , buildSecs :: Int
  }
  deriving stock (Show, Eq)

makeFieldLabelsNoPrefix ''BuildReport

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

csvHeaderHost, csvHeaderDrvName, csvHeaderBuildPlatform, csvHeaderTargetPlatform, csvHeaderEndTime, csvHeaderBuildSecs :: ByteString
csvHeaderHost = "hostname"
csvHeaderDrvName = "derivation name"
csvHeaderBuildPlatform = "build platform"
csvHeaderTargetPlatform = "target platform"
csvHeaderEndTime = "utc time"
csvHeaderBuildSecs = "build seconds"

timeFormat :: String
timeFormat = "%F %T"

instance FromNamedRecord BuildReport where
  parseNamedRecord m =
    BuildReport
      . toHost
      <$> (m .: csvHeaderHost)
      <*> (m .: csvHeaderDrvName)
      <*> parseOptionalNamedField m csvHeaderBuildPlatform
      <*> parseOptionalNamedField m csvHeaderTargetPlatform
      <*> (parseTimeM True defaultTimeLocale timeFormat =<< m .: csvHeaderEndTime)
      <*> (m .: csvHeaderBuildSecs)

parseOptionalNamedField :: NamedRecord -> ByteString -> Parser (Maybe Text)
parseOptionalNamedField record fieldName =
  maybe (pure Nothing) (fmap parseOptionalText . parseField) (HashMap.lookup fieldName record)

parseOptionalText :: Maybe Text -> Maybe Text
parseOptionalText = \case
  Just text | not (Text.null text) -> Just text
  _ -> Nothing

toHost :: Text -> Host WithoutContext
toHost = \case
  "" -> Localhost
  x -> Hostname x

instance ToNamedRecord BuildReport where
  toNamedRecord m =
    namedRecord
      [ csvHeaderHost .= fromHost m.host
      , csvHeaderDrvName .= m.drvName
      , csvHeaderBuildPlatform .= fromMaybe "" m.buildPlatform
      , csvHeaderTargetPlatform .= fromMaybe "" m.targetPlatform
      , csvHeaderEndTime .= formatTime defaultTimeLocale timeFormat m.endTime
      , csvHeaderBuildSecs .= m.buildSecs
      ]

fromHost :: Host WithoutContext -> Text
fromHost = \case
  Localhost -> ""
  Hostname x -> x

instance DefaultOrdered BuildReport where
  headerOrder _ =
    header
      [ csvHeaderHost
      , csvHeaderDrvName
      , csvHeaderBuildPlatform
      , csvHeaderTargetPlatform
      , csvHeaderEndTime
      , csvHeaderBuildSecs
      ]

saveBuildReports :: FilePath -> BuildReportMap -> IO ()
saveBuildReports dir reports = catchIO trySave mempty
 where
  trySave = do
    createDirectoryIfMissing True dir
    writeFileLBS (dir </> buildReportsFilename) (encodeDefaultOrderedByName . toCSV $ reports)

toCSV :: BuildReportMap -> [BuildReport]
toCSV = Map.assocs >=> traverse Map.assocs >>> fmap toCSVLine

toCSVLine :: (BuildReportKey, (UTCTime, Int)) -> BuildReport
toCSVLine ((host, drvName, BuildReportContext{..}), (endTime, buildSecs)) = BuildReport{..}

loadBuildReports :: FilePath -> IO BuildReportMap
loadBuildReports dir = catchIO tryLoad mempty
 where
  tryLoad =
    readFileBS (dir </> buildReportsFilename)
      >>= (toLazy >>> decodeByName >>> either (const $ fail "Could not parse CSV") (pure . snd))
      <&> (toList >>> fromCSV)

fromCSV :: [BuildReport] -> BuildReportMap
fromCSV = fmap fromCSVLine >>> Map.fromListWith Map.union

fromCSVLine :: BuildReport -> (BuildReportKey, Map UTCTime Int)
fromCSVLine BuildReport{..} = ((host, drvName, BuildReportContext{..}), Map.singleton endTime buildSecs)
