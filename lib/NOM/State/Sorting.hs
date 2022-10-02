module NOM.State.Sorting (
  sortDepsOfSet,
  summaryIncludingRoot,
  sortKey,
  SortKey,
) where

import Relude

import Control.Monad.Extra (pureIf)
import Data.Generics.Product (HasField (field))
import Data.List.Extra (firstJust)
import Data.MemoTrie (memo)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime)
import Optics (view, (%~), _1)
import Safe.Foldable (maximumMay, minimumMay)

import NOM.State (
  BuildInfo (..),
  BuildStatus (Unknown),
  DependencySummary (..),
  DerivationId,
  DerivationInfo (..),
  DerivationSet,
  NOMState,
  NOMV1State,
  getDerivationInfos,
  updateSummaryForDerivation,
 )
import NOM.State.CacheId.Map qualified as CMap
import NOM.State.CacheId.Set qualified as CSet

sortDepsOfSet :: DerivationSet -> NOMState ()
sortDepsOfSet parents = do
  currentState <- get
  let sort_parent :: DerivationId -> NOMState ()
      sort_parent drvId = do
        drvInfo <- getDerivationInfos drvId
        let newDrvInfo = (field @"inputDerivations" %~ sort_derivations) drvInfo
        modify (field @"derivationInfos" %~ CMap.insert drvId newDrvInfo)
      sort_derivations :: Seq (DerivationId, Set Text) -> Seq (DerivationId, Set Text)
      sort_derivations = Seq.sortOn (sort_key . fst)

      sort_key :: DerivationId -> SortKey
      sort_key = memo (sortKey currentState)
  mapM_ (\drvId -> sort_parent drvId) $ CSet.toList parents

type SortKey =
  ( SortOrder
  , -- We always want to show all running builds and transfers so we try to display them low in the tree.
    Down Int -- Running Builds, prefer more
  , Down Int -- Running Downloads, prefer more
  -- But we want to show the smallest tree showing all builds and downloads to save screen estate.
  , Int -- Waiting Builds, prefer less
  , Int -- Waiting Downloads, prefer less
  )

data SortOrder
  = -- First the failed builds starting with the earliest failures
    SFailed UTCTime
  | -- Second the running builds starting with longest running
    SBuilding UTCTime
  | SDownloading
  | SUploading
  | SWaiting
  | SDownloadWaiting
  | -- The longer a build is completed the less it matters
    SDone (Down UTCTime)
  | SDownloaded
  | SUploaded
  | SUnknown
  deriving stock (Eq, Show, Ord)

summaryIncludingRoot :: DerivationId -> NOMState DependencySummary
summaryIncludingRoot drvId = do
  MkDerivationInfo{dependencySummary, buildStatus} <- getDerivationInfos drvId
  pure (updateSummaryForDerivation Unknown buildStatus drvId dependencySummary)

sortKey :: NOMV1State -> DerivationId -> SortKey
sortKey nom_state drvId =
  let MkDependencySummary{..} = evalState (summaryIncludingRoot drvId) nom_state
      sort_entries =
        [ SFailed <$> minimumMay (view _1 . (.end) <$> failedBuilds)
        , SBuilding <$> minimumMay ((.start) <$> runningBuilds)
        , pureIf (not (CSet.null plannedBuilds)) SWaiting
        , pureIf (not (CSet.null plannedDownloads)) SDownloadWaiting
        , SDone <$> maximumMay (Down . (.end) <$> completedBuilds)
        , pureIf (not (CMap.null completedDownloads)) SDownloaded
        , pureIf (not (CMap.null completedUploads)) SUploaded
        ]
   in (fromMaybe SUnknown (firstJust id sort_entries), Down (CMap.size runningBuilds), Down (CMap.size runningDownloads), CSet.size plannedBuilds, CSet.size plannedDownloads)
