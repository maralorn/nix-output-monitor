module Update where

import Relude
import Prelude ()

import Control.Exception (IOException, catch)
import Control.Monad (foldM)
import Data.Attoparsec.Text.Lazy (maybeResult, parse)
import Data.Csv (FromRecord, HasHeader (NoHeader), ToRecord, decode, encode)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as LTextIO
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import qualified Nix.Derivation as Nix
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesPathExist, getXdgDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.LockFile (
  LockingException (CaughtIOException, UnableToAcquireLockFile),
  LockingParameters (retryToAcquireLock, sleepBetweenRetries),
  RetryStrategy (NumberOfTimes),
  withLockExt,
  withLockFile,
 )

import Parser (Derivation (..), Host, ParseResult (..), StorePath (..))
import qualified Parser

data BuildReport = BuildReport {reportHost :: !Text, reportName :: !Text, reportSeconds :: !Int}
  deriving (Generic, Show, Read, Eq, FromRecord, ToRecord)

getReportName :: Derivation -> Text
getReportName = Text.dropWhileEnd (`Set.member` fromList ".1234567890-") . name . toStorePath

buildReportsDir :: IO FilePath
buildReportsDir = getXdgDirectory XdgCache "nix-output-monitor"
buildReportsFilename :: FilePath
buildReportsFilename = "build-reports.csv"

saveBuildReports :: FilePath -> Map (Text, Text) Int -> IO ()
saveBuildReports dir reports = catch trySave (\(_ :: IOException) -> pass)
 where
  trySave =
    do
      createDirectoryIfMissing True dir
      writeFileLBS (dir </> buildReportsFilename) (encode . toCSV $ reports)

maybeUpdateBuildReportsIO :: Text -> [(Derivation, UTCTime)] -> BuildReportMap -> IO BuildReportMap
maybeUpdateBuildReportsIO name updates reports =
  nonEmpty updates & maybe (pure reports) (updateBuildReportsIO name)

updateBuildReportsIO :: Text -> NonEmpty (Derivation, UTCTime) -> IO BuildReportMap
updateBuildReportsIO name updates = catch tryUpdate handleLockFail
 where
  handleLockFail (UnableToAcquireLockFile file) = do
    removeFile file
    pure mempty
  handleLockFail (CaughtIOException _) = pure mempty
  tryUpdate = do
    dir <- buildReportsDir
    withLockFile
      def{retryToAcquireLock = NumberOfTimes 10, sleepBetweenRetries = 500000}
      (dir </> withLockExt buildReportsFilename)
      do
        !currentReports <- loadBuildReports dir
        now <- getCurrentTime
        let !reports = updateBuildReports now name updates currentReports
        saveBuildReports dir reports
        pure reports

loadBuildReports :: FilePath -> IO (Map (Text, Text) Int)
loadBuildReports dir = catch tryLoad (\(_ :: IOException) -> pure mempty)
 where
  !tryLoad = readFileLBS (dir </> buildReportsFilename) <&> either (const mempty) (fromCSV . toList) . decode NoHeader

type BuildReportMap = Map (Text, Text) Int

toCSV :: BuildReportMap -> [BuildReport]
toCSV = fmap (\((reportHost, reportName), reportSeconds) -> BuildReport{..}) . Map.assocs

fromCSV :: [BuildReport] -> BuildReportMap
fromCSV = fromList . fmap (\BuildReport{..} -> ((reportHost, reportName), reportSeconds))

data BuildState = BuildState
  { outstandingBuilds :: Set Derivation
  , outstandingDownloads :: Set StorePath
  , plannedCopies :: Int
  , runningLocalBuilds :: Set (Derivation, (UTCTime, Maybe Int))
  , runningRemoteBuilds :: Map Host (Set (Derivation, (UTCTime, Maybe Int)))
  , completedLocalBuilds :: Set Derivation
  , failedLocalBuilds :: Set (Derivation, Int, Int)
  , completedRemoteBuilds :: Map Host (Set Derivation)
  , failedRemoteBuilds :: Map Host (Set (Derivation, Int, Int))
  , completedDownloads :: Map Host (Set StorePath)
  , completedUploads :: Map Host (Set StorePath)
  , outputToDerivation :: Map StorePath Derivation
  , derivationToOutput :: Map Derivation StorePath
  , lastPlannedBuild :: Maybe Derivation
  , buildReports :: BuildReportMap
  , startTime :: UTCTime
  , errors :: [Text]
  }
  deriving stock (Show, Eq, Read)

updateState :: ParseResult -> BuildState -> IO BuildState
updateState result oldState = do
  now <- getCurrentTime
  newState <-
    case result of
      Uploading path host -> pure . uploading host path
      Downloading path host -> \s -> do
        let (done, newS) = downloading host path s
        newBuildReports <- maybeUpdateBuildReportsIO (toText host) (maybeToList done) (buildReports newS)
        pure newS{buildReports = newBuildReports}
      PlanCopies number -> pure . planCopy number
      RemoteBuild path host ->
        \s -> buildingRemote host path now <$> lookupDerivation s path
      LocalBuild path ->
        \s -> buildingLocal path now <$> lookupDerivation s path
      PlanBuilds plannedBuilds lastBuild ->
        \s ->
          planBuilds plannedBuilds
            <$> foldM lookupDerivation (s{lastPlannedBuild = Just lastBuild}) plannedBuilds
      PlanDownloads _download _unpacked plannedDownloads ->
        pure . planDownloads plannedDownloads
      Checking drv -> pure . buildingLocal drv now
      Failed drv code -> pure . failedBuild now drv code
      NotRecognized -> pure
    oldState
  newCompletedOutputs <-
    filterM
      (maybe (pure False) (doesPathExist . toString) . drv2out newState . fst)
      (toList (runningLocalBuilds newState))
  let newCompletedDrvs = fromList (fst <$> newCompletedOutputs)
      newCompletedReports = second fst <$> newCompletedOutputs
  newBuildReports <-
    maybeUpdateBuildReportsIO "" newCompletedReports (buildReports newState)
  pure $
    newState
      { runningLocalBuilds =
          Set.filter ((`Set.notMember` newCompletedDrvs) . fst) (runningLocalBuilds newState)
      , completedLocalBuilds =
          Set.union (completedLocalBuilds newState) newCompletedDrvs
      , buildReports = newBuildReports
      }

movingAverage :: Double
movingAverage = 0.5

updateBuildReports :: UTCTime -> Text -> NonEmpty (Derivation, UTCTime) -> BuildReportMap -> BuildReportMap
updateBuildReports now host builds = foldr (.) id (insertBuildReport <$> builds)
 where
  insertBuildReport (n, t) =
    Map.insertWith
      (\new old -> floor (movingAverage * fromIntegral new + (1 - movingAverage) * fromIntegral old))
      (host, getReportName n)
      (floor (diffUTCTime now t))

drv2out :: BuildState -> Derivation -> Maybe StorePath
drv2out s = flip Map.lookup (derivationToOutput s)
out2drv :: BuildState -> StorePath -> Maybe Derivation
out2drv s = flip Map.lookup (outputToDerivation s)

failedBuild :: UTCTime -> Derivation -> Int -> BuildState -> BuildState
failedBuild now drv code bs@BuildState{runningLocalBuilds, runningRemoteBuilds, failedLocalBuilds, failedRemoteBuilds} =
  bs
    { failedLocalBuilds = maybe id (\stamp -> Set.insert (drv, floor (diffUTCTime now stamp), code)) wasLocal failedLocalBuilds
    , runningLocalBuilds = maybe id (const $ Set.filter ((/= drv) . fst)) wasLocal runningLocalBuilds
    , failedRemoteBuilds = maybe id (\(host, stamp) -> Map.insertWith Set.union host $ Set.singleton (drv, floor (diffUTCTime now stamp), code)) wasRemote failedRemoteBuilds
    , runningRemoteBuilds = maybe id (Map.adjust (Set.filter ((drv /=) . fst)) . fst) wasRemote runningRemoteBuilds
    }
 where
  wasLocal = fmap (fst . snd) . find ((== drv) . fst) . toList $ runningLocalBuilds
  wasRemote = fmap (second (fst . snd)) $ find ((== drv) . fst . snd) (mapM toList =<< Map.assocs runningRemoteBuilds)

lookupDerivation :: BuildState -> Derivation -> IO BuildState
lookupDerivation bs@BuildState{outputToDerivation, derivationToOutput, errors} drv =
  do
    text <- LTextIO.readFile (toString drv)
    pure $
      ( do
          derivation <- maybeResult $ parse Nix.parseDerivation text
          path <- Nix.path <$> Map.lookup "out" (Nix.outputs derivation)
          maybeResult $ parse Parser.storePath (fromString path)
      )
        & \case
          Just path ->
            bs
              { outputToDerivation = Map.insert path drv outputToDerivation
              , derivationToOutput = Map.insert drv path derivationToOutput
              }
          Nothing ->
            bs
              { errors = "Could not determine output path for derivation" <> toText drv : errors
              }

initalState :: IO BuildState
initalState = do
  now <- getCurrentTime
  dir <- buildReportsDir
  buildReports <- loadBuildReports dir
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
      mempty
      mempty
      Nothing
      buildReports
      now
      mempty

planBuilds :: Set Derivation -> BuildState -> BuildState
planBuilds storePath s@BuildState{outstandingBuilds} =
  s{outstandingBuilds = Set.union storePath outstandingBuilds}

planDownloads :: Set StorePath -> BuildState -> BuildState
planDownloads storePath s@BuildState{outstandingDownloads, plannedCopies} =
  s
    { outstandingDownloads = Set.union storePath outstandingDownloads
    , plannedCopies = plannedCopies + 1
    }

planCopy :: Int -> BuildState -> BuildState
planCopy inc s@BuildState{plannedCopies} =
  s{plannedCopies = plannedCopies + inc}

downloading :: Host -> StorePath -> BuildState -> (Maybe (Derivation, UTCTime), BuildState)
downloading host storePath s@BuildState{outstandingDownloads, completedDownloads, completedUploads, plannedCopies, runningRemoteBuilds, completedRemoteBuilds} =
  ( second fst <$> done
  , s
      { plannedCopies = if total > plannedCopies then total else plannedCopies
      , runningRemoteBuilds = Map.adjust (Set.filter ((drv /=) . Just . fst)) host runningRemoteBuilds
      , completedRemoteBuilds = maybe id (Map.insertWith Set.union host . Set.singleton) (fst <$> done) completedRemoteBuilds
      , outstandingDownloads = Set.delete storePath outstandingDownloads
      , completedDownloads = newCompletedDownloads
      }
  )
 where
  newCompletedDownloads =
    Map.insertWith Set.union host (Set.singleton storePath) completedDownloads
  total = countPaths completedUploads + countPaths newCompletedDownloads
  drv = out2drv s storePath
  done = find ((drv ==) . Just . fst) $ toList (Map.findWithDefault mempty host runningRemoteBuilds)

uploading :: Host -> StorePath -> BuildState -> BuildState
uploading host storePath s@BuildState{completedUploads} =
  s
    { completedUploads = Map.insertWith Set.union host (Set.singleton storePath) completedUploads
    }

buildingLocal :: Derivation -> UTCTime -> BuildState -> BuildState
buildingLocal drv now s@BuildState{outstandingBuilds, runningLocalBuilds, buildReports} =
  s
    { runningLocalBuilds = Set.insert (drv, (now, lastNeeded)) runningLocalBuilds
    , outstandingBuilds = Set.delete drv outstandingBuilds
    }
 where
  lastNeeded = Map.lookup ("", getReportName drv) buildReports

buildingRemote :: Host -> Derivation -> UTCTime -> BuildState -> BuildState
buildingRemote host drv now s@BuildState{outstandingBuilds, runningRemoteBuilds, buildReports} =
  s
    { runningRemoteBuilds = Map.insertWith Set.union host (Set.singleton (drv, (now, lastNeeded))) runningRemoteBuilds
    , outstandingBuilds = Set.delete drv outstandingBuilds
    }
 where
  lastNeeded = Map.lookup (toText host, getReportName drv) buildReports

countPaths :: Map a (Set b) -> Int
countPaths = Map.foldr (\x y -> Set.size x + y) 0
