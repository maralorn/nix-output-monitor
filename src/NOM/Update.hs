module NOM.Update where

import Relude

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import Data.Attoparsec.Text (endOfInput, parseOnly)
import qualified Nix.Derivation as Nix

import NOM.Parser (Derivation (..), Host (..), ParseResult (..), StorePath (..))
import qualified NOM.Parser as Parser
import NOM.Update.Monad

getReportName :: Derivation -> Text
getReportName = Text.dropWhileEnd (`Set.member` fromList ".1234567890-") . name . toStorePath

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
  , startTime :: UTCTime
  , errors :: [Text]
  , inputReceived :: Bool
  }
  deriving stock (Show, Eq, Read)

{-
data BuildState2 = BuildState2
  { startTime :: UTCTime
  , errors :: [Text]
  , inputReceived :: Bool
  , derivations :: Map Derivation DerivationState
  , storePaths :: Map StorePath StorePathState
  }
  deriving stock (Show, Eq, Read)
-}

data DerivationInfo = MkDerivationInfo
  { outputs :: Map Text StorePath
  , inputDrvs :: Map Derivation (Set Text)
  , inputSrcs :: Set StorePath
  }
  deriving stock (Show, Eq, Read)

{-
data StorePathState = MkStorePathState
  { downloadPlanned :: Bool
  , downloaded :: [Host]
  , uploaded :: [Host]
  , present :: Bool
  }
-}

updateState :: UpdateMonad m => (ParseResult, Text) -> BuildState -> m (BuildState, Text)
updateState (update, buffer) = fmap (,buffer) <$> updateState' update

updateState' :: UpdateMonad m => ParseResult -> BuildState -> m BuildState
updateState' result oldState = do
  now <- getNow
  newState <-
    case result of
      Uploading path host -> pure . uploading host path
      Downloading path host -> \s -> do
        let (done, newS) = downloading host path s
        newBuildReports <- reportFinishingBuildsIfAny host (maybeToList done) (buildReports newS)
        pure newS{buildReports = newBuildReports}
      PlanCopies number -> pure . planCopy number
      Build path host ->
        \s -> building host path now <$> lookupDerivation s path
      PlanBuilds plannedBuilds lastBuild ->
        \s ->
          planBuilds plannedBuilds
            <$> foldM lookupDerivation (s{lastPlannedBuild = Just lastBuild}) plannedBuilds
      PlanDownloads _download _unpacked plannedDownloads ->
        pure . planDownloads plannedDownloads
      Checking drv -> pure . building Localhost drv now
      Failed drv code -> pure . failedBuild now drv code
      NotRecognized -> pure
    oldState
  let runningLocalBuilds = fromMaybe mempty $ Map.lookup Localhost (runningBuilds newState)
  newCompletedOutputs <-
    filterM
      (maybe (pure False) storePathExists . drv2out newState . fst)
      (toList runningLocalBuilds)
  let newCompletedDrvs = fromList (fst <$> newCompletedOutputs)
      newCompletedReports = second fst <$> newCompletedOutputs
  newBuildReports <-
    reportFinishingBuildsIfAny Localhost newCompletedReports (buildReports newState)
  pure $
    newState
      { runningBuilds = Map.adjust (Set.filter ((`Set.notMember` newCompletedDrvs) . fst)) Localhost (runningBuilds newState)
      , completedBuilds = insertMultiMap Localhost newCompletedDrvs (completedBuilds newState)
      , buildReports = newBuildReports
      , inputReceived = True
      }

movingAverage :: Double
movingAverage = 0.5

reportFinishingBuilds :: (MonadCacheBuildReports m, MonadNow m) => Host -> NonEmpty (Derivation, UTCTime) -> m BuildReportMap
reportFinishingBuilds host builds = do
  now <- getNow
  let timeDiffedBuilds = floor . diffUTCTime now <<$>> builds
  updateBuildReports (modifyBuildReports host timeDiffedBuilds)

reportFinishingBuildsIfAny :: (MonadCacheBuildReports m, MonadNow m) => Host -> [(Derivation, UTCTime)] -> BuildReportMap -> m BuildReportMap
reportFinishingBuildsIfAny host builds oldReports =
  nonEmpty builds & maybe (pure oldReports) (reportFinishingBuilds host)

modifyBuildReports :: Host -> NonEmpty (Derivation, Int) -> BuildReportMap -> BuildReportMap
modifyBuildReports host builds = foldr (.) id (insertBuildReport <$> builds)
 where
  insertBuildReport (n, t) =
    Map.insertWith
      (\new old -> floor (movingAverage * fromIntegral new + (1 - movingAverage) * fromIntegral old))
      (host, getReportName n)
      t

drv2out :: BuildState -> Derivation -> Maybe StorePath
drv2out s = Map.lookup "out" . outputs <=< flip Map.lookup (derivationInfos s)

out2drv :: BuildState -> StorePath -> Maybe Derivation
out2drv s = flip Map.lookup (outputToDerivation s)

failedBuild :: UTCTime -> Derivation -> Int -> BuildState -> BuildState
failedBuild now drv code bs@BuildState{runningBuilds, failedBuilds} =
  bs
    { failedBuilds = maybe id (\(host, stamp) -> insertMultiMap host $ Set.singleton (drv, floor (diffUTCTime now stamp), code)) buildHost failedBuilds
    , runningBuilds = maybe id (Map.adjust (Set.filter ((drv /=) . fst)) . fst) buildHost runningBuilds
    }
 where
  buildHost = fmap (second (fst . snd)) $ find ((== drv) . fst . snd) (mapM toList =<< Map.assocs runningBuilds)

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

lookupDerivation :: MonadReadDerivation m => BuildState -> Derivation -> m BuildState
lookupDerivation bs@BuildState{outputToDerivation, derivationInfos, derivationParents, errors} drv =
  handleEither . mkDerivationInfo <$> getDerivation drv
 where
  mkDerivationInfo = \derivationEither -> do
    derivation <- first (("during parsing the derivation: " <>) . toText) derivationEither
    -- first (toText . (("during parsing the outpath '" <> path <> "' ") <>)) $
    pure $
      MkDerivationInfo
        { outputs = Nix.outputs derivation & Map.mapMaybe (parseStorePath . Nix.path)
        , inputSrcs = fromList . mapMaybe parseStorePath . toList . Nix.inputSrcs $ derivation
        , inputDrvs = Map.fromList . mapMaybe (\(x, y) -> (,y) <$> parseDerivation x) . Map.toList . Nix.inputDrvs $ derivation
        }
  handleEither = \case
    Right infos ->
      bs
        { outputToDerivation = foldl' (.) id ((`Map.insert` drv) <$> outputs infos) outputToDerivation
        , derivationInfos = Map.insert drv infos derivationInfos
        , derivationParents = foldl' (.) id ((`insertMultiMapOne` drv) <$> Map.keys ( inputDrvs infos)) derivationParents
        }
    Left err ->
      bs
        { errors = "Could not determine output path for derivation " <> toText drv <> " Error: " <> err : errors
        }

invertMap :: Ord b => Map a b -> Map b a
invertMap = Map.fromList . fmap swap . Map.toList

parseStorePath :: FilePath -> Maybe StorePath
parseStorePath = hush . parseOnly (Parser.storePath <* endOfInput) . fromString
parseDerivation :: FilePath -> Maybe Derivation
parseDerivation = hush . parseOnly (Parser.derivation <* endOfInput) . fromString

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

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
      now
      mempty
      False

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

insertMultiMap :: (Ord k, Ord a) => k -> Set a -> Map k (Set a) -> Map k (Set a)
insertMultiMap = Map.insertWith Set.union

insertMultiMapOne :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
insertMultiMapOne k v = Map.insertWith Set.union k (one v)

downloading :: Host -> StorePath -> BuildState -> (Maybe (Derivation, UTCTime), BuildState)
downloading host storePath s@BuildState{outstandingDownloads, completedDownloads, completedUploads, plannedCopies, runningBuilds, completedBuilds} =
  ( second fst <$> done
  , s
      { plannedCopies = if total > plannedCopies then total else plannedCopies
      , runningBuilds = Map.adjust (Set.filter ((drv /=) . Just . fst)) host runningBuilds
      , completedBuilds = maybe id (insertMultiMap host . Set.singleton) (fst <$> done) completedBuilds
      , outstandingDownloads = Set.delete storePath outstandingDownloads
      , completedDownloads = newCompletedDownloads
      }
  )
 where
  newCompletedDownloads = insertMultiMap host (Set.singleton storePath) completedDownloads
  total = countPaths completedUploads + countPaths newCompletedDownloads
  drv = out2drv s storePath
  done = find ((drv ==) . Just . fst) $ toList (Map.findWithDefault mempty host runningBuilds)

uploading :: Host -> StorePath -> BuildState -> BuildState
uploading host storePath s@BuildState{completedUploads} =
  s
    { completedUploads = Map.insertWith Set.union host (Set.singleton storePath) completedUploads
    }

building :: Host -> Derivation -> UTCTime -> BuildState -> BuildState
building host drv now s@BuildState{outstandingBuilds, runningBuilds, buildReports} =
  s
    { runningBuilds = Map.insertWith Set.union host (Set.singleton (drv, (now, lastNeeded))) runningBuilds
    , outstandingBuilds = Set.delete drv outstandingBuilds
    }
 where
  lastNeeded = Map.lookup (host, getReportName drv) buildReports

collapseMultimap :: Ord b => Map a (Set b) -> Set b
collapseMultimap = Map.foldl' (<>) mempty
countPaths :: Ord b => Map a (Set b) -> Int
countPaths = Set.size . collapseMultimap
