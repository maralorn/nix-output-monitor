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
import Optics ((%~), view, _1)
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
import NOM.Util ((.>), (<|>>), (|>))

sortDepsOfSet :: DerivationSet -> NOMState ()
sortDepsOfSet parents = do
  currentState <- get
  let sort_parent :: DerivationId -> NOMState ()
      sort_parent drvId = do
        drvInfo <- getDerivationInfos drvId
        let newDrvInfo = (field @"inputDerivations" %~ sort_derivations) drvInfo
        modify (field @"derivationInfos" %~ CMap.insert drvId newDrvInfo)
      sort_derivations :: Seq (DerivationId, Set Text) -> Seq (DerivationId, Set Text)
      sort_derivations = Seq.sortOn (fst .> sort_key)

      sort_key :: DerivationId -> SortKey
      sort_key = memo (sortKey currentState)
  parents |> CSet.toList .> mapM_ \drvId -> sort_parent drvId

-- We order by type and disambiguate by the number of a) waiting builds, b) running builds
type SortKey =
  ( SortOrder
  , Down Int -- Waiting Builds
  , Down Int -- Running Builds
  , Down Int -- Waiting Downloads
  , Down Int -- Completed Downloads
  )

data SortOrder
  = -- First the failed builds starting with the earliest failures
    SFailed UTCTime
  | -- Second the running builds starting with longest running
    -- For one build prefer the tree with the longest prefix for the highest probability of few permutations over time
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
        [ minimumMay (failedBuilds <|>> (.end) .> view _1) <|>> SFailed
        , minimumMay (runningBuilds <|>> (.start)) <|>> SBuilding
        , pureIf (not (CSet.null plannedBuilds)) SWaiting
        , pureIf (not (CSet.null plannedDownloads)) SDownloadWaiting
        , maximumMay (completedBuilds <|>> (.end)) <|>> Down .> SDone
        , pureIf (not (CMap.null completedDownloads)) SDownloaded
        , pureIf (not (CMap.null completedUploads)) SUploaded
        ]
   in (fromMaybe SUnknown (firstJust id sort_entries), Down (CSet.size plannedBuilds), Down (CMap.size runningBuilds), Down (CSet.size plannedDownloads), Down (CMap.size completedDownloads))
