{-# LANGUAGE DataKinds #-}

module NOM.Update where

import Relude

import Control.Monad (foldM)
import Data.Generics.Product (field)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime)

import Data.Attoparsec.Text (endOfInput, parseOnly)
import qualified Nix.Derivation as Nix

import NOM.Parser (Derivation (..), Host (..), ParseResult (..), StorePath (..))
import qualified NOM.Parser as Parser
import NOM.State (BuildForest, BuildState (..), BuildStatus (..), DerivationInfo (..), DerivationNode (..), StorePathNode)
import qualified NOM.State as State
import NOM.State.Tree (ForestUpdate (..), updateForest)
import NOM.Update.Monad (
  BuildReportMap,
  MonadCacheBuildReports (..),
  MonadCheckStorePath (..),
  MonadNow (..),
  MonadReadDerivation (..),
  UpdateMonad,
 )
import NOM.Util (hush, (.>), (<.>>), (<|>>), (|>))
import Optics ((%~), (.~))

getReportName :: Derivation -> Text
getReportName = Text.dropWhileEnd (`Set.member` fromList ".1234567890-") . name . toStorePath

{-
makeBuildForest ::
  Map Derivation (Set Derivation) ->
  Map Host (Set (Derivation, (UTCTime, Maybe Int))) ->
  Map Host (Set (Derivation, Int, Int)) ->
  [Tree Derivation Build]
makeBuildForest derivationParents runningBuilds failedBuilds =
  treeFromTupleMap toBuilding runningBuilds <> treeFromTupleMap toFailed failedBuilds
    |> nonEmpty
    <.>> reverseForest buildDerivation derivationParents
    .> sortForest order
    .> mergeForest
    .> filterDoubles buildDerivation
    .> sortForest order
    |> maybe [] toList
 where
  treeFromTupleMap tupleToBuild =
    Map.toList
      .> foldMap (uncurry (\host -> toList <.>> tupleToBuild .> uncurry (MkBuild host)))
  toBuilding = second (uncurry Building)
  toFailed (derivation, duration, exitCode) = (derivation, State.Failed duration exitCode)
  order = \case
    Leaf MkBuild{buildStatus = Building{buildStart}} -> pure (SBuilding buildStart)
    Leaf MkBuild{buildStatus = State.Failed{}} -> pure SFailed
    Node _ content -> NonEmpty.reverse $ NonEmpty.sort $ order =<< content
    Link _ -> pure SLink

data SortOrder = SFailed | SBuilding UTCTime | SLink deriving (Eq, Show, Ord)
updateBuildForest :: BuildState -> BuildState
updateBuildForest bs@BuildState{..} = bs{buildForest = makeBuildForest derivationParents runningBuilds failedBuilds}
-}

updateState :: UpdateMonad m => (Maybe ParseResult, Text) -> BuildState -> m (BuildState, Text)
updateState (update, buffer) = fmap (,buffer) <$> maybe pure updateState' update

updateState' :: UpdateMonad m => ParseResult -> BuildState -> m BuildState
updateState' result oldState = do
  now <- getNow
  newState <-
    oldState
      |> case result of
        Uploading path host -> uploading host path .> pure
        Downloading path host ->
          downloading host path
            .> first toList
            .> uncurry (finishBuilds host)
        PlanCopies _ -> pure
        Build path host ->
          flip lookupDerivation path <.>> building host path now
        PlanBuilds plannedBuilds lastBuild ->
          (field @"lastPlannedBuild" .~ Just lastBuild)
            .> flip (foldM lookupDerivation) plannedBuilds
            <.>> planBuilds plannedBuilds
        PlanDownloads _download _unpacked plannedDownloads ->
          planDownloads plannedDownloads .> pure
        Checking drv -> building Localhost drv now .> pure
        Parser.Failed drv code -> failedBuild now drv code .> pure
  let runningLocalBuilds = Map.lookup Localhost (runningBuilds newState) |> maybe mempty toList
  newCompletedOutputs <-
    second fst
      <<$>> filterM
        (maybe (pure False) storePathExists . drv2out newState . fst)
        runningLocalBuilds
  newState
    |> (field @"inputReceived" .~ True)
    .> finishBuilds Localhost newCompletedOutputs

movingAverage :: Double
movingAverage = 0.5

reportFinishingBuilds :: (MonadCacheBuildReports m, MonadNow m) => Host -> NonEmpty (Derivation, UTCTime) -> m BuildReportMap
reportFinishingBuilds host builds = do
  now <- getNow
  updateBuildReports (modifyBuildReports host (timeDiffInt now <<$>> builds))

timeDiffInt :: UTCTime -> UTCTime -> Int
timeDiffInt = diffUTCTime <.>> floor

finishBuilds :: (MonadCacheBuildReports m, MonadNow m) => Host -> [(Derivation, UTCTime)] -> BuildState -> m BuildState
finishBuilds host builds' oldState = do
  now <- getNow
  let derivationUpdates = foldl' (.) id $ updateForest . uncurry mkUpdate <$> builds'
      mkUpdate drv start = derivationUpdate oldState drv (Just (host, Built (timeDiffInt now start) now))
  nonEmpty builds' |> maybe (pure oldState) \builds -> do
    let newCompletedDrvs = builds |> toList <.>> fst |> fromList
    newBuildReports <- reportFinishingBuilds host builds
    pure oldState
      <|>> (field @"buildForest" %~ derivationUpdates)
      .> (field @"buildReports" .~ newBuildReports)
      .> (field @"completedBuilds" %~ insertMultiMap host newCompletedDrvs)
      .> (field @"runningBuilds" %~ Map.adjust (Set.filter ((`Set.notMember` newCompletedDrvs) . fst)) host)

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
failedBuild now drv code bs =
  bs
    |> (field @"failedBuilds" %~ maybe id (\(host, stamp) -> insertMultiMap host $ Set.singleton (drv, timeDiffInt now stamp, code)) buildHost)
    .> (field @"runningBuilds" %~ maybe id (Map.adjust (Set.filter ((drv /=) . fst)) . fst) buildHost)
    .> (field @"buildForest" %~ updateForest (derivationUpdate bs drv (second buildState <$> buildHost)))
 where
  buildState start = State.Failed{buildExitCode = code, buildDuration = timeDiffInt now start, buildEnd = now}
  buildHost =
    find ((== drv) . fst . snd) (mapM toList =<< Map.assocs (runningBuilds bs))
      <|>> second (fst . snd)

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

lookupDerivation :: MonadReadDerivation m => BuildState -> Derivation -> m BuildState
lookupDerivation bs@BuildState{outputToDerivation, derivationInfos, derivationParents, errors} drv =
  handleEither . mkDerivationInfo <$> getDerivation drv
 where
  mkDerivationInfo = \derivationEither -> do
    derivation <- first (("during parsing the derivation: " <>) . toText) derivationEither
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
        , derivationParents = foldl' (.) id ((`insertMultiMapOne` drv) <$> Map.keys (inputDrvs infos)) derivationParents
        }
    Left err ->
      bs
        { errors = "Could not determine output path for derivation " <> toText drv <> " Error: " <> err : errors
        }

parseStorePath :: FilePath -> Maybe StorePath
parseStorePath = hush . parseOnly (Parser.storePath <* endOfInput) . fromString
parseDerivation :: FilePath -> Maybe Derivation
parseDerivation = hush . parseOnly (Parser.derivation <* endOfInput) . fromString

planBuilds :: Set Derivation -> BuildState -> BuildState
planBuilds derivations buildState =
  buildState
    |> (field @"outstandingBuilds" %~ Set.union derivations)
    .> (field @"buildForest" %~ insertDerivations)
 where
  insertDerivations :: BuildForest -> BuildForest
  insertDerivations = foldl' (.) id $ insertDerivation <$> toList derivations
  insertDerivation :: Derivation -> BuildForest -> BuildForest
  insertDerivation derivation' = updateForest (derivationUpdate buildState derivation' Nothing)

derivationUpdate :: BuildState -> Derivation -> Maybe (Host, BuildStatus) -> ForestUpdate (Either DerivationNode StorePathNode)
derivationUpdate buildState derivation' newState = do
  let def = Left (DerivationNode derivation' newState)
  ForestUpdate
    { isParent = flip (isDirectDependency buildState) def
    , isChild = isDirectDependency buildState def
    , match = matchDerivation derivation'
    , update = const def
    , def
    }

isDirectDependency :: BuildState -> Either DerivationNode StorePathNode -> Either DerivationNode StorePathNode -> Bool
isDirectDependency buildState parent' child' = case (parent', child') of
  (Left (DerivationNode parent _), Left (DerivationNode child _)) -> maybe False (Set.member child . Map.keysSet . inputDrvs) (Map.lookup parent (derivationInfos buildState))
  _ -> False

matchDerivation :: Derivation -> Either DerivationNode b -> Bool
matchDerivation derivation' = either ((derivation' ==) . derivation) (const False)

planDownloads :: Set StorePath -> BuildState -> BuildState
planDownloads storePath s@BuildState{outstandingDownloads} =
  s
    { outstandingDownloads = Set.union storePath outstandingDownloads
    }

insertMultiMap :: (Ord k, Ord a) => k -> Set a -> Map k (Set a) -> Map k (Set a)
insertMultiMap = Map.insertWith Set.union

insertMultiMapOne :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
insertMultiMapOne k v = Map.insertWith Set.union k (one v)

downloading :: Host -> StorePath -> BuildState -> (Maybe (Derivation, UTCTime), BuildState)
downloading host storePath s@BuildState{outstandingDownloads, completedDownloads, runningBuilds} =
  ( second fst <$> done
  , s
      { outstandingDownloads = Set.delete storePath outstandingDownloads
      , completedDownloads = newCompletedDownloads
      }
  )
 where
  newCompletedDownloads = insertMultiMap host (Set.singleton storePath) completedDownloads
  drv = out2drv s storePath
  done = find ((drv ==) . Just . fst) $ toList (Map.findWithDefault mempty host runningBuilds)

uploading :: Host -> StorePath -> BuildState -> BuildState
uploading host storePath =
  field @"completedUploads" %~ Map.insertWith Set.union host (Set.singleton storePath)

building :: Host -> Derivation -> UTCTime -> BuildState -> BuildState
building host drv now oldState =
  oldState
    |> (field @"runningBuilds" %~ Map.insertWith Set.union host (Set.singleton (drv, (now, lastNeeded))))
    .> (field @"outstandingBuilds" %~ Set.delete drv)
    .> (field @"buildForest" %~ updateForest (derivationUpdate oldState drv buildStatus))
 where
  buildStatus = Just (host, Building{buildStart = now, buildNeeded = lastNeeded})
  lastNeeded = Map.lookup (host, getReportName drv) (buildReports oldState)

collapseMultimap :: Ord b => Map a (Set b) -> Set b
collapseMultimap = Map.foldl' (<>) mempty
countPaths :: Ord b => Map a (Set b) -> Int
countPaths = Set.size . collapseMultimap
