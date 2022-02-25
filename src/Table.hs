module Table (Entry, cells, printAligned, printAlignedSep, prependLines, text, label, bold, green, yellow, blue, cyan, magenta, red, disp, dummy, header, displayWidth, Table.truncate) where

import Relude
import Prelude ()

import Control.Exception (assert)
import Data.Char.WCWidth (wcwidth)
import qualified Data.Text as Text
import System.Console.ANSI (
  Color (Blue, Cyan, Green, Magenta, Red, Yellow),
  ColorIntensity (Dull),
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

displayWidth :: Text -> Int
displayWidth = fst . Text.foldl' widthFold (0, False)

truncate :: Int -> Text -> Text
truncate cut = either id (\(x,_,_) -> x) . Text.foldl' (truncateFold cut) (Right ("",  0, False))

truncateFold :: Int -> Either Text (Text, Int, Bool) -> Char -> Either Text (Text, Int, Bool)
truncateFold _ (Left x) _ = Left x
truncateFold cut (Right (l, x, e)) c = if newX > cut then Left l else Right (l <> Text.singleton c, newX, newE)
 where (newX, newE) = widthFold (x, e) c
widthFold :: (Int, Bool) -> Char -> (Int, Bool)
widthFold (x, True) 'm' = (x, False)
widthFold (x, True) _ = (x, True)
widthFold (x, False) (fromEnum -> 27) = (x, True) -- Escape sequence
widthFold (x, False) c = (x + wcwidth c, False)

disp :: Int -> Entry
disp = text . show

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
addCode code e = e{codes = code : codes e}
addColor :: Color -> Entry -> Entry
addColor = addCode . SetColor Foreground Dull

bold, red, green, yellow, blue, cyan, magenta :: Entry -> Entry
bold = addCode (SetConsoleIntensity BoldIntensity)
green = addColor Green
red = addColor Red
yellow = addColor Yellow
blue = addColor Blue
cyan = addColor Cyan
magenta = addColor Magenta

prependLines :: Text -> Text -> Text -> NonEmpty Text -> Text
prependLines top mid bot rows = assert matching (top <> Text.intercalate ("\n" <> mid) (init rows) <> "\n" <> bot <> last rows)
 where
  matching = Text.length top == Text.length mid && Text.length mid == Text.length bot

verticalSlim, hsep :: Text
verticalSlim = "│"
hsep = " " <> verticalSlim <> " "

printAligned :: NonEmpty (NonEmpty Entry) -> NonEmpty Text
printAligned rows = printRow " " (toList $ widths "" rows) <$> rows
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
  | width entry == 1 = entryWidth entry
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

-- >>> printEntry (cells 2 (label ">" (text "<"))) [5,5]
-- ">           <"

printEntry :: Text -> Entry -> [Int] -> Text
printEntry sep Entry{codes, lcontent, rcontent} entryWidths = whenCodes codes <> lcontent <> spacing <> rcontent <> whenCodes [Reset]
 where
  spaces = max 0 (width - displayWidth rcontent - displayWidth lcontent)
  spacing = mtimesDefault spaces " "
  whenCodes = memptyIfFalse (not . null $ codes) . toText . setSGRCode
  width = sum entryWidths + (Text.length sep * (length entryWidths - 1))
