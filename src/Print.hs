module Print where

import           Prelude                        ( )
import           Parser                         ( StorePath(..)
                                                , Host(..)
                                                , Derivation(..)
                                                )
import           Relude
import           Update
import           Data.Time
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Text.Printf
import qualified Data.Text                     as Text
import           System.Console.ANSI.Types
import           System.Console.ANSI

vertical, verticalSlim, lowerleft, upperleft, horizontal, down, up, clock, running, done, todo, leftT, bold, green, yellow, blue, reset, cyan, magenta, cellBorder, tablePadding, emptyCell, skipCell
  :: Text
vertical = "┃"
verticalSlim = "│"
lowerleft = "┗"
upperleft = "┏"
leftT = "┣"
horizontal = "━"
down = "⬇"
up = "⬆"
clock = "⏲"
running = "▶"
done = "☑"
todo = "☐"
bold = toText $ setSGRCode [SetConsoleIntensity BoldIntensity]
green = toText $ setSGRCode [SetColor Foreground Dull Green]
yellow = toText $ setSGRCode [SetColor Foreground Dull Yellow]
blue = toText $ setSGRCode [SetColor Foreground Dull Blue]
cyan = toText $ setSGRCode [SetColor Foreground Dull Cyan]
magenta = toText $ setSGRCode [SetColor Foreground Dull Magenta]
reset = toText $ setSGRCode [Reset]

times :: Int -> Text -> Text
times = stimesMonoid

cell :: Int -> Text
cell x =
  let str = fromString . printf "% 4d" $ x
  in  (if x > 0 then bold <> str else str) <> reset

cellBorder = " " <> verticalSlim <> " "
tablePadding = vertical <> "    "
emptyCell = "     "
skipCell = emptyCell <> cellBorder

showCond :: Bool -> Text -> Text
showCond = memptyIfFalse
showCondHide :: Bool -> Text -> Text
showCondHide cond text =
  if cond then text else stimesMonoid (Text.length text) " "
condCell :: Bool -> Text -> Text
condCell cond text =
  if cond then text else stimesMonoid (Text.length text - 3) " " <> cellBorder

stateToText :: BuildState -> Text
stateToText buildState@BuildState { outstandingBuilds, outstandingDownloads, plannedCopies, runningRemoteBuilds, runningLocalBuilds, completedLocalBuilds, completedDownloads, completedUploads, currentTime, startTime, completedRemoteBuilds }
  | totalBuilds + plannedCopies == 0
  = ""
  | otherwise
  = (if runningBuilds > 0
      then
        printBuilds currentTime runningRemoteBuilds runningLocalBuilds <> leftT
      else upperleft
    )
    <> times 3 horizontal
    <> showCond showBuilds
                (bold <> " Builds               " <> reset <> cellBorder)
    <> showCond showDownloads (bold <> "Downloads    " <> reset <> cellBorder)
    <> showCond showUploads   (bold <> "Uploads" <> reset <> cellBorder)
    <> showCond showHosts     (bold <> "Host" <> reset)
    <> "\n"
    <> (if showHosts
         then printHosts buildState showBuilds showDownloads showUploads
         else ""
       )
    <> lowerleft
    <> horizontal
    <> " 𝚺 "
    <> showCond
         showBuilds
         (  yellow
         <> running
         <> cell runningBuilds
         <> cellBorder
         <> green
         <> done
         <> cell numCompletedBuilds
         <> cellBorder
         <> blue
         <> todo
         <> cell numOutstandingBuilds
         <> cellBorder
         )
    <> showCond
         showDownloads
         (  green
         <> down
         <> cell downloadsDone
         <> cellBorder
         <> blue
         <> todo
         <> cell (length outstandingDownloads)
         <> cellBorder
         )
    <> showCond showUploads ("       " <> cellBorder)
    <> clock
    <> " "
    <> timeDiff currentTime startTime
    <> reset
 where
  showHosts     = numHosts > 0
  showBuilds    = totalBuilds > 0
  showDownloads = downloadsDone + length outstandingDownloads > 0
  showUploads   = countPaths completedUploads > 0
  numHosts =
    length (Map.keysSet runningRemoteBuilds)
      + length (Map.keysSet completedRemoteBuilds)
      + length (Map.keysSet completedUploads)
  runningBuilds = countPaths runningRemoteBuilds + length runningLocalBuilds
  numCompletedBuilds =
    countPaths completedRemoteBuilds + length completedLocalBuilds
  numOutstandingBuilds = length outstandingBuilds
  totalBuilds = numOutstandingBuilds + runningBuilds + numCompletedBuilds
  downloadsDone = countPaths completedDownloads

printHosts :: BuildState -> Bool -> Bool -> Bool -> Text
printHosts BuildState { runningRemoteBuilds, runningLocalBuilds, completedLocalBuilds, completedDownloads, completedUploads, completedRemoteBuilds } showBuilds showDownloads showUploads
  = unlines
    (  tablePadding
    <> showCond
         showBuilds
         (  yellow
         <> running
         <> cell (Set.size runningLocalBuilds)
         <> cellBorder
         <> green
         <> done
         <> cell (Set.size completedLocalBuilds)
         <> cellBorder
         <> skipCell
         )
    <> showCond showDownloads (skipCell <> skipCell)
    <> showCond showUploads   ("  " <> skipCell)
    <> "local"
    :  remoteLabels
    )
 where
  remoteLabels =
    (\h ->
        let runningBuilds = l h runningRemoteBuilds
            doneBuilds    = l h completedRemoteBuilds
        in  tablePadding
              <> showCond
                   showBuilds
                   (  c (yellow <> running) runningBuilds
                   <> cellBorder
                   <> c (green <> done) doneBuilds
                   <> cellBorder
                   <> skipCell
                   )

              <> showCond
                   showDownloads
                   (  c (green <> down) (l h completedDownloads)
                   <> cellBorder
                   <> skipCell
                   )

              <> showCond
                   showUploads
                   (c (green <> up) (l h completedUploads) <> "  " <> cellBorder)
              <> magenta
              <> toText h
              <> reset
      )
      <$> hosts
  hosts =
    sort
      .  toList
      $  Map.keysSet runningRemoteBuilds
      <> Map.keysSet completedRemoteBuilds
      <> Map.keysSet completedUploads
      <> Map.keysSet completedDownloads
  l host = Set.size . Map.findWithDefault mempty host
  c icon num = if num > 0 then icon <> cell num else emptyCell
printBuilds
  :: UTCTime
  -> Map Host (Set (Derivation, UTCTime))
  -> Set (Derivation, UTCTime)
  -> Text
printBuilds now remoteBuilds localBuilds =
  unlines
    .  (upperleft <> horizontal <> " Currently building:" :)
    .  fmap printBuild
    .  sortOn snd
    $  remoteLabels
    <> localLabels
 where
  remoteLabels = Map.foldMapWithKey
    (\host builds ->
      (\(x, t) ->
          (name (toStorePath x) <> reset <> " on " <> magenta <> toText host, t)
        )
        <$> toList builds
    )
    remoteBuilds
  localLabels = (\(x, t) -> (name $ toStorePath x, t)) <$> toList localBuilds
  printBuild (p, t) =
    vertical
      <> " "
      <> yellow
      <> running
      <> reset
      <> " "
      <> cyan
      <> p
      <> reset
      <> " "
      <> clock
      <> " "
      <> timeDiff now t

timeDiff :: UTCTime -> UTCTime -> Text
timeDiff larger smaller = toText
  $ formatTime defaultTimeLocale "%02H:%02M:%02S" (diffUTCTime larger smaller)
