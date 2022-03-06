module NOM.Print (stateToText) where

import Control.Monad (foldM)
import Data.Generics.Product (typed)
import Data.Generics.Sum (_Ctor)
import Data.List (partition)
import Data.List.NonEmpty.Extra (appendr)
import qualified Data.Map.Strict as Map
import Data.MemoTrie (HasTrie, memo)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime, ZonedTime, defaultTimeLocale, diffUTCTime, formatTime, zonedTimeToUTC)
import Data.Tree (Forest, Tree (Node, rootLabel))
import qualified Data.Tree as Tree
import NOM.Parser (Derivation (toStorePath), Host (Localhost), StorePath (name))
import NOM.Print.Table (Entry, blue, bold, cells, cyan, disp, dummy, green, grey, header, label, magenta, markup, markups, prependLines, printAlignedSep, red, text, yellow)
import NOM.Print.Tree (showForest)
import NOM.State (BuildInfo (MkBuildInfo), BuildStatus (..), DependencySummary (..), DerivationId, DerivationInfo (..), DerivationSet, NOMState, NOMV1State (..), ProcessState (Finished, JustStarted), StorePathInfo, StorePathState (..), buildEnd, buildEstimate, buildHost, buildStart, getDerivationInfos)
import qualified NOM.State.CacheId.Map as CMap
import qualified NOM.State.CacheId.Set as CSet
import NOM.State.Sorting (SortOrder (SWaiting), sortKey, summaryIncludingRoot)
import NOM.State.Tree (aggregateTree, collapseForestN, mapRootsTwigsAndLeafs, replaceDuplicates, sortForest)
import NOM.Util (collapseMultimap, countPaths, (.>), (<.>>), (<<.>>>), (<|>>), (|>))
import Optics (has, preview, summing, view, (%), _1, _2, _Just, _Left, _Nothing, _Right)
import Optics.Fold (hasn't)
import Relude
import System.Console.Terminal.Size (Window)
import qualified System.Console.Terminal.Size as Window

lb, vertical, lowerleft, upperleft, horizontal, down, up, clock, running, done, bigsum, warning, todo, leftT, average, goal :: Text
vertical = "┃"
lowerleft = "┗"
upperleft = "┏"
leftT = "┣"
horizontal = "━"
down = "⬇"
up = "⬆"
clock = "⏲"
running = "▶"
goal = "🏁"
done = "✔"
todo = "⏳"
warning = "⚠"
average = "∅"
bigsum = "∑"
lb = "▓"

showCond :: Monoid m => Bool -> m -> m
showCond = memptyIfFalse

targetRatio, defaultTreeMax :: Int
targetRatio = 3
defaultTreeMax = 20

stateToText :: NOMV1State -> Maybe (Window Int) -> ZonedTime -> Text
stateToText buildState@MkNOMV1State {..} = fmap Window.height .> memo printWithSize
  where
    printWithSize :: Maybe Int -> ZonedTime -> Text
    printWithSize maybeWindow = printWithTime
      where
        printWithTime :: ZonedTime -> Text
        printWithTime
          | processState == JustStarted = \now -> time now <> showCond (diffUTCTime (zonedTimeToUTC now) startTime > 15) (markup grey " nom hasn‘t detected any input. Have you redirected nix-build stderr into nom? (See the README for details.)")
          | not anythingGoingOn = time
          | otherwise = \now -> buildsDisplay now <> table (time now) <> errorList
        maxHeight = case maybeWindow of
          Just limit -> limit `div` targetRatio
          Nothing -> defaultTreeMax
        buildsDisplay now =
          prependLines
            (upperleft <> horizontal)
            (vertical <> " ")
            (vertical <> " ")
            (printBuilds buildState maxHeight (zonedTimeToUTC now))
            <> "\n"
    runTime now = timeDiff (zonedTimeToUTC now) startTime
    time
      | processState == Finished = \now -> finishMarkup (" at " <> toText (formatTime defaultTimeLocale "%H:%M:%S" now) <> " after " <> runTime now)
      | otherwise = \now -> clock <> " " <> runTime now
    errorList = if null errors then "" else "\n" <> unlines errors
    MkDependencySummary {..} = fullSummary
    runningBuilds' = runningBuilds <|>> buildHost
    completedBuilds' = completedBuilds <|>> buildHost
    numFailedBuilds = CMap.size failedBuilds
    anythingGoingOn = totalBuilds + downloadsDone + numPlannedDownloads + numFailedBuilds > 0
    table time' =
      prependLines
        (leftT <> stimes (3 :: Int) horizontal <> " ")
        (vertical <> "    ")
        (lowerleft <> horizontal <> " " <> bigsum <> " ")
        $ printAlignedSep (innerTable `appendr` one (lastRow time'))
    innerTable :: [NonEmpty Entry]
    innerTable = fromMaybe (one (text "")) (nonEmpty headers) : showCond showHosts printHosts
    headers =
      (cells 3 <$> optHeader showBuilds "Builds")
        <> (cells 2 <$> optHeader showDownloads "Downloads")
        <> optHeader showUploads "Uploads"
        <> optHeader showHosts "Host"
    optHeader cond = showCond cond . one . bold . header :: Text -> [Entry]
    partial_last_row =
      showCond
        showBuilds
        [ nonZeroBold numRunningBuilds (yellow (label running (disp numRunningBuilds))),
          nonZeroBold numCompletedBuilds (green (label done (disp numCompletedBuilds))),
          nonZeroBold numPlannedBuilds (blue (label todo (disp numPlannedBuilds)))
        ]
        <> showCond
          showDownloads
          [ nonZeroBold downloadsDone (green (label down (disp downloadsDone))),
            nonZeroBold numPlannedDownloads . blue . label todo . disp $ numPlannedDownloads
          ]
        <> showCond showUploads [nonZeroBold uploadsDone (green (label up (disp uploadsDone)))]
    lastRow time' = partial_last_row `appendr` one (bold (header time'))

    showHosts = numHosts > 0
    showBuilds = totalBuilds > 0
    showDownloads = downloadsDone + CSet.size plannedDownloads > 0
    showUploads = CMap.size completedUploads > 0
    numPlannedDownloads = CSet.size plannedDownloads
    numHosts =
      Set.size (Set.filter (/= Localhost) (foldMap one runningBuilds' <> foldMap one completedBuilds' <> foldMap one completedUploads))
    numRunningBuilds = CMap.size runningBuilds
    numCompletedBuilds = CMap.size completedBuilds
    numPlannedBuilds = CSet.size plannedBuilds
    totalBuilds = numPlannedBuilds + numRunningBuilds + numCompletedBuilds
    downloadsDone = CMap.size completedDownloads
    uploadsDone = CMap.size completedUploads
    finishMarkup = if numFailedBuilds == 0 then ((goal <> "Finished") <>) .> markup green else ((warning <> " Exited with failures") <>) .> markup red

    printHosts :: [NonEmpty Entry]
    printHosts =
      mapMaybe nonEmpty $ labelForHost <$> hosts
      where
        labelForHost :: Host -> [Entry]
        labelForHost h =
          showCond
            showBuilds
            [ nonZeroShowBold numRunningBuilds (yellow (label running (disp numRunningBuildsOnHost))),
              nonZeroShowBold doneBuilds (green (label done (disp doneBuilds))),
              dummy
            ]
            <> showCond
              showDownloads
              [nonZeroShowBold downloads (green (label down (disp downloads))), dummy]
            <> showCond
              showUploads
              [nonZeroShowBold uploads (green (label up (disp uploads)))]
            <> one (magenta (header (toText h)))
          where
            uploads = l h completedUploads
            downloads = l h completedDownloads
            numRunningBuildsOnHost = l h runningBuilds'
            doneBuilds = l h completedBuilds'
        hosts =
          sort . toList @Set $
            foldMap (foldMap one) [runningBuilds', completedBuilds'] <> foldMap (foldMap one) [completedUploads, completedDownloads]
        l host = CMap.size . CMap.filter (host ==)

nonZeroShowBold :: Int -> Entry -> Entry
nonZeroShowBold num = if num > 0 then bold else const dummy

nonZeroBold :: Int -> Entry -> Entry
nonZeroBold num = if num > 0 then bold else id

data TreeNode = DerivationNode DerivationInfo | Link DerivationId | Summary DependencySummary deriving (Generic)

data TreeLocation = Root | Twig | Leaf deriving (Eq)

printBuilds ::
  NOMV1State ->
  Int ->
  UTCTime ->
  NonEmpty Text
printBuilds nomState _maxHeight =
  \now -> preparedPrintForest |> fmap (fmap (now |>)) .> showForest .> (markup bold " Dependency Graph:" :|)
  where
    preparedPrintForest :: Forest (UTCTime -> Text)
    preparedPrintForest = resizedForest <|>> mapRootsTwigsAndLeafs (printTreeNode Root) (printTreeNode Twig) (printTreeNode Leaf)
    printTreeNode :: TreeLocation -> TreeNode -> UTCTime -> Text
    printTreeNode location = \case
      DerivationNode drvInfo ->
        let summary = showSummary (dependencySummary drvInfo)
         in \now -> printDerivation drvInfo now <> showCond ((location == Root || location == Leaf) && not (Text.null summary)) (markup grey " & " <> summary)
      Link drv -> const (printLink drv)
      Summary summary -> const (showSummary summary)

    buildForest :: Seq DerivationId -> NOMState (Forest TreeNode)
    buildForest drvId = go mempty drvId <|>> snd
      where
        mkSummaryNode :: DependencySummary -> Forest TreeNode -> Forest TreeNode
        mkSummaryNode summary rest
          | summary == mempty = rest
          | otherwise = pure (Summary summary) : rest

        go :: DerivationSet -> Seq DerivationId -> NOMState (DerivationSet, Forest TreeNode)
        go seenIds = \case
          (thisDrv Seq.:<| restDrvs)
            | CSet.member thisDrv seenIds -> do
              (newSeenIds, restForest) <- go seenIds restDrvs
              let mkSubforest
                    | (Node (Summary theirSummary) _ : restTail) <- restForest = do
                      ourSummary <- summaryIncludingRoot thisDrv
                      pure (mkSummaryNode (ourSummary <> theirSummary) restTail)
                    | (Node (Link linkId) _ : restTail) <- restForest = do
                      ourSummary <- summaryIncludingRoot thisDrv
                      theirSummary <- summaryIncludingRoot linkId
                      pure (mkSummaryNode (ourSummary <> theirSummary) restTail)
                    | otherwise = pure [pure (Link thisDrv)]
              mkSubforest <|>> (newSeenIds,)
            | hasUnseenBuild thisDrv -> do
              drvInfo <- getDerivationInfos thisDrv
              let newSeenIds = CSet.insert thisDrv seenIds
              (thenSeenIds, subforest) <- go newSeenIds (fmap fst (inputDerivations drvInfo))
              let subforestToUse
                    | all (rootLabel .> hasn't (_Ctor @"DerivationNode")) subforest = []
                    | otherwise = subforest
              (lastlySeenIds, restforest) <- go thenSeenIds restDrvs
              pure (lastlySeenIds, Node (DerivationNode drvInfo) subforestToUse : restforest)
            | otherwise -> do
              summaries <- mapM summaryIncludingRoot (thisDrv Seq.<| restDrvs) <|>> fold
              pure (seenIds, mkSummaryNode summaries [])
          _ -> pure (seenIds, [])
          where
            hasUnseenBuild thisDrv =
              let MkDependencySummary {..} = evalState (summaryIncludingRoot thisDrv) nomState
               in not (CSet.isSubsetOf (CMap.keysSet failedBuilds <> CMap.keysSet runningBuilds) seenIds)

    resizedForest :: Forest TreeNode
    resizedForest = flip evalState nomState $ do
      gets forestRoots >>= buildForest

    showSummary :: DependencySummary -> Text
    showSummary MkDependencySummary {..} =
      [ [totalBar | not (Text.null totalBar)],
        memptyIfTrue
          (0 == downloads)
          [ markup
              cyan
              ( down
                  <> show fullDownloads
                  <> showCond (downloads > fullDownloads) ("/" <> show downloads)
              )
          ],
        memptyIfTrue
          (0 == uploads)
          [markup magenta (up <> show uploads)]
      ]
        |> join .> unwords
      where
        totalBar =
          bar red (CMap.size failedBuilds)
            <> bar green (CMap.size completedBuilds)
            <> bar yellow (CMap.size runningBuilds)
            <> bar blue (CSet.size plannedBuilds)
        uploads = CMap.size completedUploads
        fullDownloads = CMap.size completedDownloads
        downloads = fullDownloads + CSet.size plannedDownloads
        bar color c
          | c == 0 = ""
          | c <= 10 = stimesMonoid c lb |> markup color
          | otherwise = ("▓▓▓┄" <> show c <> "┄▓▓▓") |> markup color

    printDerivation :: DerivationInfo -> UTCTime -> Text
    printDerivation MkDerivationInfo {..} = case buildStatus of
      Unknown -> const drvName
      Planned -> const (markup blue (todo <> " " <> drvName))
      Building MkBuildInfo {..} -> \now ->
        unwords $
          [markups [yellow, bold] (running <> " " <> drvName)]
            <> hostMarkup buildHost
            <> [clock, timeDiff now buildStart]
            <> maybe [] (\x -> ["(" <> average <> timeDiffSeconds x <> ")"]) buildEstimate
      Failed MkBuildInfo {..} ->
        const $
          unwords $
            [markups [red, bold] (warning <> " " <> drvName)]
              <> hostMarkup buildHost
              <> [markups [red, bold] (unwords ["failed with exit code", show (snd buildEnd), "after", clock, timeDiff (fst buildEnd) buildStart])]
      Built MkBuildInfo {..} ->
        const $
          unwords $
            [markup green (done <> " " <> drvName)]
              <> hostMarkup buildHost
              <> [markup grey (clock <> " " <> timeDiff buildEnd buildStart)]
      where
        drvName = derivationName |> toStorePath .> name

    printLink :: DerivationId -> Text
    printLink drvId =
      flip evalState nomState $
        getDerivationInfos drvId <|>> derivationName
          .> toStorePath
          .> name
          .> (<> " ↴")
          .> markup grey

hostMarkup :: Host -> [Text]
hostMarkup Localhost = mempty
hostMarkup host = ["on " <> markup magenta (toText host)]

timeDiff :: UTCTime -> UTCTime -> Text
timeDiff =
  diffUTCTime
    <.>> printDuration
    .> toText

printDuration :: NominalDiffTime -> Text
printDuration diff
  | diff < 60 = p "%Ss"
  | diff < 60 * 60 = p "%Mm%Ss"
  | otherwise = p "%Hh%Mm%Ss"
  where
    p x = diff |> formatTime defaultTimeLocale x .> toText

timeDiffSeconds :: Int -> Text
timeDiffSeconds = fromIntegral .> printDuration