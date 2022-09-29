module NOM.IO (interact, processTextStream) where

import Relude

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.STM (check, modifyTVar, swapTVar)
import Control.Exception (IOException, try)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString
import Data.Text qualified as Text
import Data.Time (ZonedTime, getZonedTime)
import System.Console.ANSI qualified as Terminal
import System.Console.Terminal.Size qualified as Terminal.Size
import System.IO qualified

import Streamly.Data.Fold qualified as Fold
import Streamly.Prelude ((.:))
import Streamly.Prelude qualified as Stream

import System.Console.ANSI (SGR (Reset), setSGRCode)

import NOM.Error (NOMError (InputError))
import NOM.IO.ParseStream (parseStream)
import NOM.Print (Config (..))
import NOM.Print.Table as Table (bold, displayWidth, displayWidthBS, markup, red, truncate)
import NOM.Update.Monad (UpdateMonad)
import NOM.Util ((.>), (<|>>), (|>))
import Streamly ((|&))

type Stream = Stream.SerialT IO
type Output = Text
type UpdateFunc update state = forall m. UpdateMonad m => (update, ByteString) -> StateT state m ([NOMError], ByteString)
type OutputFunc state = state -> Maybe Window -> ZonedTime -> Output
type Finalizer state = forall m. UpdateMonad m => StateT state m ()
type Window = Terminal.Size.Window Int

readTextChunks :: Handle -> Stream (Either NOMError ByteString)
readTextChunks handle = loop
 where
  -- We read up-to 4kb of input at once. We will rarely need more than that for one succesful parse (i.e. a line).
  -- I don‘t know much about computers, but 4k seems like something which would be cached efficiently.
  bufferSize :: Int
  bufferSize = 4096
  tryRead :: Stream (Either IOException ByteString)
  tryRead = liftIO $ try $ ByteString.hGetSome handle bufferSize
  loop :: Stream (Either NOMError ByteString)
  loop =
    tryRead >>= \case
      Left err -> Left (InputError err) .: loop -- Forward Exceptions, when we encounter them
      Right "" -> mempty -- EOF
      Right input -> Right input .: loop

runUpdate ::
  forall update state.
  TVar ByteString ->
  TVar state ->
  UpdateFunc update state ->
  (update, ByteString) ->
  IO ByteString
runUpdate bufferVar stateVar updater input = do
  oldState <- readTVarIO stateVar
  ((!errors, !output), !newState) <- runStateT (updater input) oldState
  atomically $ do
    forM_ errors (\error' -> modifyTVar bufferVar (appendError error'))
    writeTVar stateVar newState
  pure output

writeStateToScreen :: forall state. Bool -> TVar Int -> TVar state -> TVar ByteString -> (state -> state) -> OutputFunc state -> Handle -> IO ()
writeStateToScreen pad printed_lines_var nom_state_var nix_output_buffer_var maintenance printer output_handle = do
  now <- getZonedTime
  terminalSize <- Terminal.Size.hSize output_handle

  (nom_state, nix_output_raw) <- atomically do
    -- ==== Time Critical Segment - calculating to much in atomically can lead
    -- to recalculations.  In this section we are racing with the input parsing
    -- thread to update the state.
    modifyTVar nom_state_var maintenance
    -- we bind those lazily to not calculate them during STM
    ~nom_state <- readTVar nom_state_var
    ~nix_output_raw <- swapTVar nix_output_buffer_var mempty
    pure (nom_state, nix_output_raw)
  -- ====

  let nix_output = ByteString.lines nix_output_raw
      nix_output_length = length nix_output

      nom_output = ByteString.lines $ encodeUtf8 $ truncateOutput terminalSize (printer nom_state terminalSize now)
      nom_output_length = length nom_output

      -- We will try to calculate how many lines we can draw without reaching the end
      -- of the screen so that we can avoid flickering redraws triggered by
      -- printing a newline.
      -- For the output passed through from Nix the lines could be to long leading
      -- to reflow by the terminal and therefor messing with our line count.
      -- We try to predict the number of introduced linebreaks here. The number
      -- might be slightly to high in corner cases but that will only trigger
      -- sligthly more redraws which is totally acceptable.
      reflow_line_count_correction =
        terminalSize <|>> (.width) .> \terminalWidth ->
          getSum $ foldMap (\line -> Sum (displayWidthBS line `div` terminalWidth)) nix_output

  (last_printed_line_count, lines_to_pad) <- atomically do
    last_printed_line_count <- readTVar printed_lines_var
    -- When the nom output suddenly gets smaller, it might jump up from the bottom of the screen.
    -- To prevent this we insert a few newlines before it.
    -- We only do this if we know the size of the terminal.
    let lines_to_pad = case reflow_line_count_correction of
          Just reflow_correction | pad -> max 0 (last_printed_line_count - reflow_correction - nix_output_length - nom_output_length)
          _ -> 0
        line_count_to_print = nom_output_length + lines_to_pad
    writeTVar printed_lines_var line_count_to_print
    pure (last_printed_line_count, lines_to_pad)

  let output_to_print = nix_output <> mtimesDefault lines_to_pad [""] <> nom_output
      -- We have Strict enabled and at this point we are using force to make sure
      -- the whole output has been calculated before we clear the screen.
      output_to_print_with_newline_annotations = force (zip (howToGoToNextLine last_printed_line_count reflow_line_count_correction <$> [0 ..])) output_to_print

  -- when clear the line, but don‘t use cursorUpLine, the cursor needs to be moved to the start for printing.
  -- we do that before clearing because we can
  when (last_printed_line_count == 1) do Terminal.hSetCursorColumn output_handle 0

  -- ==== Time Critical Segment - When we start to clear the screen we need to
  -- print content as fast as possible
  -- Clear last output from screen.
  -- First we clear the current line, if we have written on it.
  when (last_printed_line_count > 0) do Terminal.hClearLine output_handle

  -- Then, if necessary we, move up and clear more lines.
  replicateM_ (last_printed_line_count - 1) do
    Terminal.hCursorUpLine output_handle 1 -- Moves cursor one line up and to the beginning of the line.
    Terminal.hClearLine output_handle -- We are avoiding to use clearFromCursorToScreenEnd
    -- because it apparently triggers a flush on some terminals.

  -- Write new output to screen.
  forM_ output_to_print_with_newline_annotations \(newline, line) -> do
    case newline of
      StayInLine -> pass
      MoveToNextLine -> Terminal.hCursorDownLine output_handle 1
      PrintNewLine -> ByteString.hPut output_handle "\n"
    ByteString.hPut output_handle line

  System.IO.hFlush output_handle

-- ====

data ToNextLine = StayInLine | MoveToNextLine | PrintNewLine
  deriving stock (Generic)
  deriving anyclass (NFData)

-- Depending on the current line of the output we are printing we need to decide
-- how to move to a new line before printing.
howToGoToNextLine :: Int -> Maybe Int -> Int -> ToNextLine
howToGoToNextLine _ Nothing = \case
  -- When we have no info about terminal size, better be careful and always print
  -- newlines if necessary.
  0 -> StayInLine
  _ -> PrintNewLine
howToGoToNextLine previousPrintedLines (Just correction) = \case
  -- When starting to print we are always in an empty line with the cursor at the start.
  -- So we don‘t need to go to a new line
  0 -> StayInLine
  -- When the current offset is smaller than the number of previously printed lines.
  -- e.g. we have printed 1 line, but before we had printed 2
  -- then we can probably move the cursor a row down without needing to print a newline.
  x
    | x + correction < previousPrintedLines ->
        MoveToNextLine
  -- When we are at the bottom of the terminal we have no choice but need to
  -- print a newline and thus (sadly) flush the terminal
  _ -> PrintNewLine

interact ::
  forall update state.
  Config ->
  Parser update ->
  UpdateFunc update state ->
  (state -> state) ->
  OutputFunc state ->
  Finalizer state ->
  Handle ->
  Handle ->
  state ->
  IO state
interact config parser updater maintenance printer finalize inputHandle output_handle initialState =
  readTextChunks inputHandle
    |> processTextStream config parser updater maintenance (Just (printer, output_handle)) finalize initialState

-- frame durations are passed to threadDelay and thus are given in microseconds

maxFrameDuration :: Int
maxFrameDuration = 1_000_000 -- once per second to update timestamps

minFrameDuration :: Int
minFrameDuration =
  -- this seems to be a nice compromise to reduce excessive
  -- flickering, since the movement is not continuous this low frequency doesn‘t
  -- feel to sluggish for the eye, for me.
  100_000 -- 10 times per second

processTextStream ::
  forall update state.
  Config ->
  Parser update ->
  UpdateFunc update state ->
  (state -> state) ->
  Maybe (OutputFunc state, Handle) ->
  Finalizer state ->
  state ->
  Stream (Either NOMError ByteString) ->
  IO state
processTextStream config parser updater maintenance printerMay finalize initialState inputStream = do
  stateVar <- newTVarIO initialState
  bufferVar <- newTVarIO mempty
  let keepProcessing :: IO ()
      keepProcessing =
        inputStream
          |& Stream.tap (writeErrorsToBuffer bufferVar)
          |& Stream.mapMaybe rightToMaybe
          |& parseStream parser
          |& Stream.mapM (snd .> runUpdate bufferVar stateVar updater >=> flip (<>) .> modifyTVar bufferVar .> atomically)
          |> Stream.drain
      waitForInput :: IO ()
      waitForInput = atomically $ check . not . ByteString.null =<< readTVar bufferVar
  printerMay |> maybe keepProcessing \(printer, output_handle) -> do
    linesVar <- newTVarIO 0
    let writeToScreen :: IO ()
        writeToScreen = writeStateToScreen (not config.silent) linesVar stateVar bufferVar maintenance printer output_handle
        keepPrinting :: IO ()
        keepPrinting = forever do
          race_ (concurrently_ (threadDelay minFrameDuration) waitForInput) (threadDelay maxFrameDuration)
          writeToScreen
    race_ keepProcessing keepPrinting
    readTVarIO stateVar >>= execStateT finalize >>= writeTVar stateVar .> atomically
    writeToScreen
  readTVarIO stateVar |> (if isNothing printerMay then (>>= execStateT finalize) else id)

writeErrorsToBuffer :: TVar ByteString -> Fold.Fold IO (Either NOMError ByteString) ()
writeErrorsToBuffer bufferVar = Fold.drainBy saveInput
 where
  saveInput :: Either NOMError ByteString -> IO ()
  saveInput = \case
    Left error' -> atomically $ modifyTVar bufferVar (appendError error')
    _ -> pass

appendError :: NOMError -> ByteString -> ByteString
appendError err prev = (if ByteString.null prev || ByteString.isSuffixOf "\n" prev then "" else "\n") <> nomError <> show err <> "\n"

nomError :: ByteString
nomError = encodeUtf8 (markup (red . bold) "nix-output-monitor internal error: ")

truncateOutput :: Maybe Window -> Text -> Text
truncateOutput win output = maybe output go win
 where
  go :: Window -> Text
  go window = Text.intercalate "\n" $ truncateColumns window.width <$> truncateRows window.height

  truncateColumns :: Int -> Text -> Text
  truncateColumns columns line = if displayWidth line > columns then Table.truncate (columns - 1) line <> "…" <> toText (setSGRCode [Reset]) else line

  truncateRows :: Int -> [Text]
  truncateRows rows
    | length outputLines >= rows - outputLinesToAlwaysShow = take 1 outputLines <> [" ⋮ "] <> drop (length outputLines + outputLinesToAlwaysShow + 2 - rows) outputLines
    | otherwise = outputLines

  outputLines :: [Text]
  outputLines = Text.lines output

outputLinesToAlwaysShow :: Int
outputLinesToAlwaysShow = 5
