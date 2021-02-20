module Update where

import Relude
import Prelude ()

import Control.Monad (foldM)
import Data.Attoparsec.Text.Lazy (maybeResult, parse)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as LTextIO
import Data.Time (UTCTime, getCurrentTime)
import qualified Nix.Derivation as Nix
import System.Directory (doesPathExist)

import Parser (Derivation (..), Host, ParseResult (..), StorePath (..))
import qualified Parser

updateState :: ParseResult -> BuildState -> IO BuildState
updateState result oldState = do
  now <- getCurrentTime
  set
    =<< ( case result of
            Uploading path host -> pure . uploading host path
            Downloading path host -> pure . downloading host path
            PlanCopies number -> pure . planCopy number
            RemoteBuild path host ->
              \s -> buildingRemote host path now <$> lookupDerivation s path
            LocalBuild path ->
              \s -> buildingLocal path now <$> lookupDerivation s path
            PlanBuilds plannedBuilds ->
              \s ->
                planBuilds plannedBuilds
                  <$> foldM lookupDerivation s plannedBuilds
            PlanDownloads _download _unpacked plannedDownloads ->
              pure . planDownloads plannedDownloads
            NotRecognized -> pure
        )
      oldState
 where
  set s = do
    newlyCompletedOutputs <-
      fromList
        <$> filterM
          (maybe (pure False) (doesPathExist . toString) . drv2out s)
          (fst <$> toList (runningLocalBuilds s))
    pure $
      s
        { runningLocalBuilds =
            Set.filter
              ( \x ->
                  fst x `Set.notMember` newlyCompletedOutputs
              )
              (runningLocalBuilds s)
        , completedLocalBuilds =
            Set.union
              (completedLocalBuilds s)
              newlyCompletedOutputs
        }

drv2out :: BuildState -> Derivation -> Maybe StorePath
drv2out s = flip Map.lookup (derivationToOutput s)
out2drv :: BuildState -> StorePath -> Maybe Derivation
out2drv s = flip Map.lookup (outputToDerivation s)

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
data BuildState = BuildState
  { outstandingBuilds :: Set Derivation
  , outstandingDownloads :: Set StorePath
  , plannedCopies :: Int
  , runningLocalBuilds :: Set (Derivation, UTCTime)
  , runningRemoteBuilds :: Map Host (Set (Derivation, UTCTime))
  , completedLocalBuilds :: Set Derivation
  , completedRemoteBuilds :: Map Host (Set Derivation)
  , completedDownloads :: Map Host (Set StorePath)
  , completedUploads :: Map Host (Set StorePath)
  , outputToDerivation :: Map StorePath Derivation
  , derivationToOutput :: Map Derivation StorePath
  , startTime :: UTCTime
  , errors :: [Text]
  }
  deriving stock (Show, Eq, Read)

initalState :: UTCTime -> BuildState
initalState now =
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

downloading :: Host -> StorePath -> BuildState -> BuildState
downloading host storePath s@BuildState{outstandingDownloads, completedDownloads, completedUploads, plannedCopies, runningRemoteBuilds, completedRemoteBuilds} =
  s
    { plannedCopies = if total > plannedCopies then total else plannedCopies
    , runningRemoteBuilds = Map.adjust (Set.filter ((drv /=) . Just . fst)) host runningRemoteBuilds
    , completedRemoteBuilds = maybe id (Map.insertWith Set.union host . Set.singleton) done completedRemoteBuilds
    , outstandingDownloads = Set.delete storePath outstandingDownloads
    , completedDownloads = newCompletedDownloads
    }
 where
  newCompletedDownloads =
    Map.insertWith Set.union host (Set.singleton storePath) completedDownloads
  total = countPaths completedUploads + countPaths newCompletedDownloads
  drv = out2drv s storePath
  done = join . find (drv ==) $ Just . fst <$> toList (Map.findWithDefault mempty host runningRemoteBuilds)

uploading :: Host -> StorePath -> BuildState -> BuildState
uploading host storePath s@BuildState{completedUploads} =
  s
    { completedUploads = Map.insertWith Set.union host (Set.singleton storePath) completedUploads
    }

buildingLocal :: Derivation -> UTCTime -> BuildState -> BuildState
buildingLocal drv now s@BuildState{outstandingBuilds, runningLocalBuilds} =
  s
    { runningLocalBuilds = Set.insert (drv, now) runningLocalBuilds
    , outstandingBuilds = Set.delete drv outstandingBuilds
    }
buildingRemote :: Host -> Derivation -> UTCTime -> BuildState -> BuildState
buildingRemote host drv now s@BuildState{outstandingBuilds, runningRemoteBuilds} =
  s
    { runningRemoteBuilds = Map.insertWith Set.union host (Set.singleton (drv, now)) runningRemoteBuilds
    , outstandingBuilds = Set.delete drv outstandingBuilds
    }

countPaths :: Map a (Set b) -> Int
countPaths = Map.foldr (\x y -> Set.size x + y) 0
