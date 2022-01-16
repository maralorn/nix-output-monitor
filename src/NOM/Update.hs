{-# LANGUAGE DataKinds #-}

module NOM.Update where

import Relude

import Control.Monad (foldM)
import Data.Generics.Product (field)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime)
import Data.Tree (Forest, Tree)

import Data.Attoparsec.Text (endOfInput, parseOnly)
import qualified Nix.Derivation as Nix

import Data.Foldable (minimum)
import Data.Graph (Tree (Node))
import NOM.Parser (Derivation (..), Host (..), ParseResult (..), StorePath (..))
import qualified NOM.Parser as Parser
import NOM.State (BuildForest, BuildState (..), BuildStatus (..), DerivationInfo (..), DerivationNode (..), LinkTreeNode, StorePathNode, Summary, path)
import qualified NOM.State as State
import NOM.State.Tree (ForestUpdate (..), aggregateTree, replaceDuplicates, sortForest, updateForest)
import NOM.Update.Monad (
  BuildReportMap,
  MonadCacheBuildReports (..),
  MonadCheckStorePath (..),
  MonadNow (..),
  MonadReadDerivation (..),
  UpdateMonad,
 )
import NOM.Util (hush, (.>), (<.>>), (<<.>>>), (<<|>>>), (<|>>), (|>), insertMultiMap, insertMultiMapOne)
import Optics ((%~), (.~))

getReportName :: Derivation -> Text
getReportName = Text.dropWhileEnd (`Set.member` fromList ".1234567890-") . name . toStorePath

data SortOrder
  = -- First the failed builds starting with the earliest failures
    SFailed UTCTime
  | -- Second the running builds starting with longest running
    -- For one build prefer the tree with the longest prefix for the highest probability of few permutations over time
    SBuilding UTCTime (Down Int)
  | SWhatever
  | -- The longer a build is completed the less it matters
    SDone (Down UTCTime)
  | -- Links are really not that interesting
    SLink
  deriving (Eq, Show, Ord)

mkOrder :: (a -> SortOrder) -> Tree a -> SortOrder
mkOrder order = go
 where
  go (Node label subForest) = minimum (order label : (increaseLevel . go <$> subForest))

increaseLevel :: SortOrder -> SortOrder
increaseLevel = \case
  SBuilding t i -> SBuilding t (i + 1)
  s -> s

nodeOrder :: Either DerivationNode b -> SortOrder
nodeOrder = \case
  Left (DerivationNode _ (Just (_, Building start _))) -> SBuilding start 0
  Left (DerivationNode _ (Just (_, State.Failed _ _ at))) -> SFailed at
  Left (DerivationNode _ (Just (_, Built _ at))) -> SDone (Down at)
  _ -> SWhatever

linkNodeOrder :: LinkTreeNode -> SortOrder
linkNodeOrder = either nodeOrder (const SLink)

updateState :: UpdateMonad m => (Maybe ParseResult, Text) -> BuildState -> m (BuildState, Text)
updateState (update, buffer) = fmap (,buffer) <$> updateState' update

setInputReceived :: BuildState -> BuildState
setInputReceived s = if inputReceived s then s else s{inputReceived = True}

updateState' :: UpdateMonad m => Maybe ParseResult -> BuildState -> m BuildState
updateState' result (setInputReceived -> state1) = do
  -- Update the state if any changes where parsed.
  state2Maybe <- mapM (processResult state1) result
  let state2 = fromMaybe state1 state2Maybe
  -- Check if any local builds have finished, because nix-build would not tell us.
  state3Maybe <- detectLocalFinishedBuilds state2
  -- If any changes happened calculate a new cachedForest
  fromMaybe state2 state3Maybe |> (if isJust state2Maybe || isJust state3Maybe then updateCachedShowForest else id) .> pure

updateCachedShowForest :: BuildState -> BuildState
updateCachedShowForest (field @"buildForest" %~ sortForest (mkOrder nodeOrder) -> newState) =
  newState
    { cachedShowForest =
        newState |> buildForest
          .> addSummaryToForest
          .> replaceLinksInForest
          .> sortForest (fmap fst .> mkOrder linkNodeOrder)
    }

detectLocalFinishedBuilds :: UpdateMonad m => BuildState -> m (Maybe BuildState)
detectLocalFinishedBuilds oldState = do
  let runningLocalBuilds = Map.lookup Localhost (runningBuilds oldState) |> maybe mempty toList
  newCompletedOutputs <-
    filterM
      (maybe (pure False) storePathExists . drv2out oldState . fst)
      runningLocalBuilds
      <<|>>> second fst
  if null newCompletedOutputs
    then pure Nothing
    else
      oldState
        |> finishBuilds Localhost newCompletedOutputs <.>> Just

processResult :: UpdateMonad m => BuildState -> ParseResult -> m BuildState
processResult oldState result = do
  now <- getNow
  oldState |> case result of
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
planDownloads storePath = field @"outstandingDownloads" %~ Set.union storePath

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

addSummaryToForest :: BuildForest -> Forest (Either DerivationNode StorePathNode, Summary)
addSummaryToForest = fmap (aggregateTree one)

replaceLinksInForest :: Forest (Either DerivationNode StorePathNode, Summary) -> Forest (LinkTreeNode, Summary)
replaceLinksInForest = replaceDuplicates mkLink <<.>>> either (first Left) (first Right)

mkLink :: (Either DerivationNode StorePathNode, Summary) -> (Either Derivation StorePath, Summary)
mkLink (node, summary) = (bimap derivation path node, Set.insert node summary)
