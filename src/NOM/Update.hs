module NOM.Update where

import Relude
import Relude.Extra (traverseToFst)

import Control.Monad (foldM)
import Data.Generics.Product (field, typed)
import Data.Generics.Sum (_As)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime)
import Optics (preview, (%), (%~), (.~))

import Data.Attoparsec.Text (endOfInput, parseOnly)

import qualified Nix.Derivation as Nix

import NOM.Parser (Derivation (..), Host (..), ParseResult (..), StorePath (..))
import qualified NOM.Parser as Parser
import NOM.State (BuildInfo (MkBuildInfo, buildStart), BuildStatus (..), DerivationInfo (..), NOMV1State (..), ProcessState (..), StorePathState (..), drv2out, getRunningBuildsByHost, out2drv)
import qualified NOM.State as State
import NOM.Update.Monad (
  BuildReportMap,
  MonadCacheBuildReports (..),
  MonadCheckStorePath (..),
  MonadNow (..),
  MonadReadDerivation (..),
  UpdateMonad,
 )
import NOM.Util (foldEndo, hush, (.>), (<.>>), (<|>>), (|>))

getReportName :: Derivation -> Text
getReportName = Text.dropWhileEnd (`Set.member` fromList ".1234567890-") . name . toStorePath

{-
data SortOrder
  = -- First the failed builds starting with the earliest failures
    SFailed UTCTime
  | -- Second the running builds starting with longest running
    -- For one build prefer the tree with the longest prefix for the highest probability of few permutations over time
    SBuilding UTCTime (Down Int)
  | SDownloading
  | SUploading
  | SWaiting
  | SDownloadWaiting
  | -- The longer a build is completed the less it matters
    SDone (Down UTCTime)
  | SDownloaded
  | SUploaded
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

nodeOrder :: BuildTreeNode -> SortOrder
nodeOrder = \case
  Left (DerivationNode _ (Just (_, Building start _))) -> SBuilding start 0
  Left (DerivationNode _ (Just (_, State.Failed _ _ at))) -> SFailed at
  Left (DerivationNode _ (Just (_, Built _ at))) -> SDone (Down at)
  Left (DerivationNode _ Nothing) -> SWaiting
  Right (StorePathNode _ _ state') -> state' |> fmap storePathOrder .> minimum

storePathOrder :: StorePathState -> SortOrder
storePathOrder = \case
  State.Downloaded _ -> SDownloaded
  State.Downloading _ -> SDownloading
  State.Uploaded _ -> SUploaded
  State.Uploading _ -> SUploading
  State.DownloadPlanned -> SDownloadWaiting
-}
setInputReceived :: NOMV1State -> Maybe NOMV1State
setInputReceived s = if processState s == JustStarted then Just s{processState = InputReceived} else Nothing

updateState :: UpdateMonad m => Maybe ParseResult -> (Maybe UTCTime, NOMV1State) -> m (Maybe UTCTime, Maybe NOMV1State)
updateState result (lastAccess, state0) = do
  now <- getNow
  let (nextAccessTime, check) =
        if maybe True (diffUTCTime now .> (>= 0.2)) lastAccess
          then (Just now, detectLocalFinishedBuilds)
          else (lastAccess, const (pure Nothing))
  -- A Left means that we have not changed the state
  Left state0 |>
  -- First check if we this is the first time that we receive input (for error messages)
     tryAndPropagate (setInputReceived .> pure) >=>
  -- Update the state if any changes where parsed.
     tryAndPropagate (processResult .> forM result) >=>
  -- Check if any local builds have finished, because nix-build would not tell us.
  -- If we haven‘t done so in the last 200ms.
     tryAndPropagate check
  -- If we have a Right now, we return that
       <.>> hush .> (nextAccessTime,)

tryAndPropagate :: Functor f => (a -> f (Maybe a)) -> Either a a -> f (Either a a)
tryAndPropagate f x = either f f x <|>> maybe x Right

detectLocalFinishedBuilds :: UpdateMonad m => NOMV1State -> m (Maybe NOMV1State)
detectLocalFinishedBuilds oldState = do
  let runningLocalBuilds = getRunningBuildsByHost Localhost oldState |> Map.toList
  newCompletedOutputs <-
    filterM
      (maybe (pure False) storePathExists . drv2out oldState . fst)
      runningLocalBuilds
  if null newCompletedOutputs
    then pure Nothing
    else
      oldState
        |> finishBuilds Localhost newCompletedOutputs <.>> Just

processResult :: UpdateMonad m => NOMV1State -> ParseResult -> m NOMV1State
processResult oldState result = do
  now <- getNow
  oldState |> case result of
    Parser.Uploading path host -> uploaded host path .> pure
    Parser.Downloading path host ->
      downloaded host path
        .> first toList
        .> uncurry (finishBuilds host)
    PlanCopies _ -> pure
    Build path host ->
      flip lookupDerivation path <.>> building host path now
    PlanBuilds plannedBuilds _lastBuild ->
      flip (foldM lookupDerivation) plannedBuilds
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

finishBuilds :: (MonadCacheBuildReports m, MonadNow m) => Host -> [(Derivation, BuildInfo ())] -> NOMV1State -> m NOMV1State
finishBuilds host builds' oldState = do
  now <- getNow
  --  let derivationUpdates = foldl' (.) id $ updateForest . uncurry mkUpdate <$> builds'
  --      mkUpdate drv start = derivationUpdate oldState drv (Just (host, Built (timeDiffInt now start) now))
  updateReport <- nonEmpty builds' |> maybe (pure id) \builds -> reportFinishingBuilds host (builds <|>> second buildStart) <|>> (typed .~)
  let derivationUpdates = builds' <|>> \(drv, info) -> Map.adjust (typed .~ Built (info $> now)) drv
  oldState |> updateReport .> (field @"derivationInfos" %~ appEndo (foldMap Endo derivationUpdates)) .> pure

modifyBuildReports :: Host -> NonEmpty (Derivation, Int) -> BuildReportMap -> BuildReportMap
modifyBuildReports host = fmap (uncurry insertBuildReport) .> foldEndo
 where
  insertBuildReport name =
    Map.insertWith
      (\new old -> floor (movingAverage * fromIntegral new + (1 - movingAverage) * fromIntegral old))
      (host, getReportName name)

failedBuild :: UTCTime -> Derivation -> Int -> NOMV1State -> NOMV1State
failedBuild now drv code = field @"derivationInfos" %~ Map.adjust (typed %~ update) drv
 where
  update = \case
    Building a -> State.Failed (a $> (now, code))
    x -> x

lookupDerivation :: MonadReadDerivation m => NOMV1State -> Derivation -> m NOMV1State
lookupDerivation buildState@MkNOMV1State{..} drv =
  if Map.member drv derivationInfos
    then pure buildState
    else do
      derivationInfo <- getDerivation drv <|>> mkDerivationInfo
      derivationInfo
        |> fmap (inputDerivations .> Map.keysSet)
        .> fromRight mempty
        .> foldM lookupDerivation (handleEither derivationInfo)
 where
  mkDerivationInfo = \derivationEither -> do
    derivation <- first (("during parsing the derivation: " <>) . toText) derivationEither
    pure $
      MkDerivationInfo
        { outputs = Nix.outputs derivation & Map.mapMaybe (parseStorePath . Nix.path)
        , inputSources = fromList . mapMaybe parseStorePath . toList . Nix.inputSrcs $ derivation
        , inputDerivations = Map.fromList . mapMaybe (bitraverse parseDerivation pure) . Map.toList . Nix.inputDrvs $ derivation
        , buildStatus = Unknown
        }
  handleEither = \case
    Right infos ->
      buildState
        |> (typed %~ Map.insert drv infos)
        .> (field @"derivationParents" %~ foldEndo (inputDerivations infos |> Map.keys <.>> \drv' -> Map.alter (maybe (one drv) (Set.insert drv) .> Just) drv'))
        .> (typed %~ foldEndo (outputs infos |> Map.toList <.>> \(name, path) -> Map.insert path (drv, name)))
        .> (typed %~ foldEndo (inputSources infos |> toList <.>> \path -> Map.alter (maybe (one drv) (Set.insert drv) .> Just) path))
        .> (field @"derivationParents" %~ Map.alter (fromMaybe mempty .> Just) drv)
    Left err ->
      buildState
        { errors = "Could not determine output path for derivation " <> toText drv <> " Error: " <> err : errors
        }

parseStorePath :: FilePath -> Maybe StorePath
parseStorePath = hush . parseOnly (Parser.storePath <* endOfInput) . fromString
parseDerivation :: FilePath -> Maybe Derivation
parseDerivation = hush . parseOnly (Parser.derivation <* endOfInput) . fromString

planBuilds :: Set Derivation -> NOMV1State -> NOMV1State
planBuilds = (toList <.>> \drv -> field @"derivationInfos" %~ Map.adjust (typed .~ Planned) drv) .> foldEndo

{-
derivationUpdate :: NOMV1State -> Derivation -> Maybe (Host, BuildStatus) -> ForestUpdate BuildTreeNode
derivationUpdate buildState derivation' newState = do
  let def = Left @DerivationNode @StorePathNode (DerivationNode derivation' newState)
  ForestUpdate
    { isParent = flip (isDirectDependency buildState) def
    , isChild = isDirectDependency buildState def
    , match = matchDerivation derivation'
    , update = const def
    , def
    }

storePathUpdate :: NOMV1State -> StorePath -> StorePathState -> ForestUpdate BuildTreeNode
storePathUpdate buildState storePath newState = do
  let drv = Map.lookup storePath (outputToDerivation buildState)
      def = Right (StorePathNode storePath drv (one newState))
  ForestUpdate
    { isParent = flip (isDirectDependency buildState) def
    , isChild = isDirectDependency buildState def
    , match = matchStorePath storePath
    , update = second ((typed %~ insertStorePathState newState) .> (typed .~ drv))
    , def
    }
-}
insertStorePathState :: StorePathState -> StorePath -> NOMV1State -> NOMV1State
insertStorePathState storePathState storePath = typed %~ Map.alter (maybe (pure storePathState) (toList .> localFilter .> (storePathState :|)) .> Just) storePath
 where
  localFilter = case storePathState of
    DownloadPlanned -> id
    State.Downloading _ -> filter (DownloadPlanned /=)
    Downloaded h -> filter (State.Downloading h /=) .> filter (DownloadPlanned /=)
    State.Uploading _ -> id
    Uploaded h -> filter (State.Uploading h /=)

{-
isDirectDependency :: NOMV1State -> BuildTreeNode -> BuildTreeNode -> Bool
isDirectDependency buildState parent' child' = fromMaybe False $ case (parent', child') of
  (Left (DerivationNode parent _), Left (DerivationNode child _)) -> drvsDep parent child
  (Left (DerivationNode parent _), Right child) -> drv2path parent child
  (Right (StorePathNode _ (Just parent) _), Right child) -> drv2path parent child
  (Right (StorePathNode _ Nothing _), Right _) -> Nothing
  (Right _, Left _) -> Nothing
 where
  getInfo parent = Map.lookup parent (derivationInfos buildState)
  drvsDep parent child = getInfo parent <|>> inputDrvs .> Map.keysSet .> Set.member child
  drv2path parent = \case
    (StorePathNode child Nothing _) -> getInfo parent <|>> inputSrcs .> Set.member child
    (StorePathNode _ (Just child) _) -> drvsDep parent child
matchDerivation :: Derivation -> BuildTreeNode -> Bool
matchDerivation derivation' = either ((derivation' ==) . derivation) (const False)
matchStorePath :: StorePath -> BuildTreeNode -> Bool
matchStorePath storePath = either (const False) ((storePath ==) . path)
-}

planDownloads :: Set StorePath -> NOMV1State -> NOMV1State
planDownloads = (toList <.>> insertStorePathState DownloadPlanned) .> foldEndo

downloaded :: Host -> StorePath -> NOMV1State -> (Maybe (Derivation, BuildInfo ()), NOMV1State)
downloaded host storePath buildState =
  ( done
  , buildState |> insertStorePathState (Downloaded host) storePath
  )
 where
  done = do
    d <- out2drv buildState storePath
    Map.lookup d (derivationInfos buildState) >>= preview (typed @BuildStatus % _As @"Building") <.>> (d,)

uploaded :: Host -> StorePath -> NOMV1State -> NOMV1State
uploaded host = insertStorePathState (Downloaded host)

building :: Host -> Derivation -> UTCTime -> NOMV1State -> NOMV1State
building host drv now nomState = (field @"derivationInfos" %~ Map.adjust (typed .~ Building (MkBuildInfo now host lastNeeded ())) drv) nomState
 where
  lastNeeded = Map.lookup (host, getReportName drv) (buildReports nomState)
