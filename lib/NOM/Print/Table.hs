module NOM.Print.Table (
  Entry,
  cells,
  printAlignedSep,
  prependLines,
  text,
  label,
  bold,
  green,
  yellow,
  blue,
  magenta,
  red,
  dummy,
  header,
  displayWidth,
  truncate,
  markup,
  markups,
  grey,
  displayWidthBS,
) where

import Control.Exception (assert)
-- ansi-terminal

import Data.ByteString.Char8 qualified as ByteString
import Data.Text qualified as Text
import Relude hiding (truncate)
import System.Console.ANSI (
  Color (Black, Blue, Green, Magenta, Red, Yellow),
  ColorIntensity (Dull, Vivid),
  ConsoleIntensity (BoldIntensity),
  ConsoleLayer (Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity),
  setSGRCode,
 )

data Entry = Entry
  { codes :: [SGR]
  , lcontent :: Text
  , rcontent :: Text
  , width :: Int
  }

-- >>> displayWidth "∑"
-- 1

{- | Gives display width of a string, correctly ignoring ANSI codes and trying to
 guess correct with for unicode symbols like emojis.
-}
displayWidth :: Text -> Int
displayWidth = fst . Text.foldl' widthFold (0, False)

-- | Like displayWidth but for ByteString
displayWidthBS :: ByteString -> Int
displayWidthBS = fst . ByteString.foldl' widthFold (0, False)

truncate :: Int -> Text -> Text
truncate cut = either id (\(x, _, _) -> x) . Text.foldl' (truncateFold cut) (Right ("", 0, False))

truncateFold :: Int -> Either Text (Text, Int, Bool) -> Char -> Either Text (Text, Int, Bool)
truncateFold _ (Left x) _ = Left x
truncateFold cut (Right (l, x, e)) c =
  let (newX, newE) = widthFold (x, e) c
   in if newX > cut then Left l else Right (l <> Text.singleton c, newX, newE)

-- See: https://github.com/maralorn/nix-output-monitor/issues/78
widthFold ::
  -- | (Width so far, in an ANSI escape sequence)
  (Int, Bool) ->
  Char ->
  (Int, Bool)
widthFold (x, True) 'm' = (x, False)
widthFold (x, True) _ = (x, True)
widthFold (x, False) (fromEnum -> 0x1b) = (x, True) -- Escape sequence
widthFold (x, False) _ = (x + 1, False)

dummy :: Entry
dummy = text ""

text :: Text -> Entry
text t = Entry [] "" t 1

header :: Text -> Entry
header t = Entry [] t "" 1

cells :: Int -> Entry -> Entry
cells width e = e{width}

label :: Text -> Entry -> Entry
label t e = e{lcontent = t}

addCode :: SGR -> Entry -> Entry
addCode code e = e{codes = code : e.codes}

addColor :: Color -> Entry -> Entry
addColor = addCode . SetColor Foreground Dull

bold, red, green, yellow, blue, magenta, grey :: Entry -> Entry
bold = addCode (SetConsoleIntensity BoldIntensity)
green = addColor Green
red = addColor Red
yellow = addColor Yellow
blue = addColor Blue
magenta = addColor Magenta
grey = addCode $ SetColor Foreground Vivid Black

prependLines :: Text -> Text -> Text -> NonEmpty Text -> Text
prependLines top mid bot rows =
  assert
    matching
    ( top
        <> Text.intercalate ("\n" <> mid) allButLast
        <> memptyIfTrue (null allButLast) ("\n" <> bot)
        <> last rows
    )
 where
  allButLast = init rows
  matching = Text.length top == Text.length mid && Text.length mid == Text.length bot

verticalSlim, hsep :: Text
verticalSlim = "│"
hsep = " " <> verticalSlim <> " "

printAlignedSep :: NonEmpty (NonEmpty Entry) -> NonEmpty Text
printAlignedSep rows = printRow hsep (toList $ widths hsep rows) <$> rows

widths :: Text -> NonEmpty (NonEmpty Entry) -> NonEmpty Int
widths sep rows = nonEmpty restList & maybe (one width) (\rest -> width :| toList (widths sep rest))
 where
  (width, restList) = nextWidth sep rows

nextWidth :: Text -> NonEmpty (NonEmpty Entry) -> (Int, [NonEmpty Entry])
nextWidth sep rows = (width, chopWidthFromRows sep width rows)
 where
  width = getWidthForNextColumn rows

getWidthForNextColumn :: NonEmpty (NonEmpty Entry) -> Int
getWidthForNextColumn = getWidthForColumn . fmap head

getWidthForColumn :: NonEmpty Entry -> Int
getWidthForColumn = foldl' max 0 . fmap getRelevantWidthForEntry

getRelevantWidthForEntry :: Entry -> Int
getRelevantWidthForEntry entry
  | entry.width == 1 = entryWidth entry
getRelevantWidthForEntry _ = 0

entryWidth :: Entry -> Int
entryWidth Entry{lcontent, rcontent} = displayWidth lcontent + displayWidth rcontent + if Text.null lcontent || Text.null rcontent then 0 else 1

chopWidthFromRows :: Text -> Int -> NonEmpty (NonEmpty Entry) -> [NonEmpty Entry]
chopWidthFromRows sep width = mapMaybe (nonEmpty . chopWidthFromRow sep width) . toList

chopWidthFromRow :: Text -> Int -> NonEmpty Entry -> [Entry]
chopWidthFromRow sep targetWidth (entry@Entry{width} :| rest)
  | width > 1 = entry{width = width - 1, lcontent = "", rcontent = mtimesDefault (max 0 (entryWidth entry - targetWidth - displayWidth sep)) " "} : rest
chopWidthFromRow _ _ (_ :| rest) = rest

printRow :: Text -> [Int] -> NonEmpty Entry -> Text
printRow sep colWidths entries = Text.intercalate sep $ snd (foldl' foldFun (colWidths, id) entries) []
 where
  foldFun (colsLeft, line) entry@Entry{width} =
    (drop width colsLeft, line . (printEntry sep entry (take width colsLeft) :))

-- >>> printEntry "" (cells 2 (label ">" (text "<"))) []
-- "><"

markups :: [Entry -> Entry] -> Text -> Text
markups fs = foldl' (.) id (markup <$> fs)

markup :: (Entry -> Entry) -> Text -> Text
markup f = showEntry . f . text

showEntry :: Entry -> Text
showEntry = flip (printEntry "") []

printEntry :: Text -> Entry -> [Int] -> Text
printEntry sep Entry{codes, lcontent, rcontent} entryWidths = whenCodes codes <> lcontent <> spacing <> rcontent <> whenCodes [Reset]
 where
  spaces = max 0 (width - displayWidth rcontent - displayWidth lcontent)
  spacing = mtimesDefault spaces " "
  whenCodes = memptyIfFalse (not . null $ codes) . toText . setSGRCode
  width = sum entryWidths + (Text.length sep * (length entryWidths - 1))
