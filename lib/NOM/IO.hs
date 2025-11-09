module NOM.IO (interact, processTextStream, StreamParser, Stream) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.STM (check, swapTVar)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as ByteString
import Data.Text qualified as Text
import Data.Time (ZonedTime, getZonedTime)
import NOM.Error (NOMError)
import NOM.Print (Config (..))
import NOM.Print.Table as Table (bold, displayWidth, displayWidthBS, markup, red, truncate)
import NOM.Update.Monad (UpdateMonad, getNow)
import Relude
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import System.Console.ANSI (SGR (Reset), setSGRCode)
import System.Console.ANSI qualified as Terminal
import System.Console.Terminal.Size qualified as Terminal.Size
import System.IO qualified

type Stream = Stream.Stream IO

type StreamParser update = Stream ByteString -> Stream update

type Output = Text

type UpdateFunc update state = forall m. (UpdateMonad m) => update -> StateT state m ([NOMError], ByteString, Bool)

type OutputFunc state = state -> Maybe Window -> (ZonedTime, Double) -> Output

type Finalizer state = forall m. (UpdateMonad m) => StateT state m ()

type Window = Terminal.Size.Window Int

runUpdate ::
  forall update state.
  TVar [ByteString] ->
  TMVar state ->
  TVar Bool ->
  UpdateFunc update state ->
  update ->
  IO ()
runUpdate output_builder_var state_var refresh_display_var updater input = do
  -- Since we are taking the state_var here we prevent any other state update
  -- to happen simultaneously.
  old_state <- atomically $ takeTMVar state_var
  ((errors, log_output, display_changed), !newState) <- runStateT (updater input) old_state
  atomically $ do
    forM_ errors (writeErrorToBuilder output_builder_var)
    putTMVar state_var newState
    unless (ByteString.null log_output) do
      modifyTVar' output_builder_var (log_output :)
    modifyTVar' refresh_display_var (|| display_changed)

-- https://gitlab.com/gnachman/iterm2/-/wikis/synchronized-updates-spec
startAtomicUpdate, endAtomicUpdate :: Builder.Builder
startAtomicUpdate = "\x1b[?2026h"
endAtomicUpdate = "\x1b[?2026l"

writeStateToScreen ::
  forall state.
  Bool ->
  TVar Int ->
  TMVar state ->
  TVar [ByteString] ->
  TVar Bool ->
  (Double -> state -> state) ->
  OutputFunc state ->
  Handle ->
  IO ()
writeStateToScreen pad printed_lines_var nom_state_var nix_output_buffer_var refresh_display_var maintenance printer output_handle = do
  nowClock <- getZonedTime
  now <- getNow
  terminalSize <-
    Terminal.Size.hSize output_handle <&> \case
      -- We throw away non positive window sizes, which some terminals apparently report
      -- to avoid divisions by zero later on.
      val@(Just window) | window.width > 0, window.height > 0 -> val
      _ -> Nothing

  (nom_state, nix_output_raw) <- atomically do
    -- ==== Time Critical Segment - calculating to much in atomically can lead
    -- to recalculations.  In this section we are racing with the input parsing
    -- thread to update the state.
    -- we bind those lazily to not calculate them during the STM transaction
    nom_state <- maintenance now <$> takeTMVar nom_state_var
    putTMVar nom_state_var nom_state

    writeTVar refresh_display_var False

    nix_output_raw <- swapTVar nix_output_buffer_var []
    pure (nom_state, nix_output_raw)
  -- ====

  let nix_output = ByteString.lines $ ByteString.concat $ reverse nix_output_raw
      nix_output_length = length nix_output

      nom_output = ByteString.lines $ encodeUtf8 $ truncateOutput terminalSize (printer nom_state terminalSize (nowClock, now))
      nom_output_length = length nom_output

      -- We will try to calculate how many lines we can draw without reaching the end
      -- of the screen so that we can avoid flickering redraws triggered by
      -- printing a newline.
      -- For the output passed through from Nix the lines could be to long leading
      -- to reflow by the terminal and therefor messing with our line count.
      -- We try to predict the number of introduced linebreaks here. The number
      -- might be slightly to high in corner cases but that will only trigger
      -- slightly more redraws which is totally acceptable.
      reflow_line_count_correction =
        terminalSize <&> \size ->
          -- This division is fine, because we don‘t accept non positive window sizes.
          getSum $ foldMap (\line -> Sum (displayWidthBS line `div` size.width)) nix_output

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

  -- Prepare ByteString to write on terminal
  let output_to_print = nix_output <> mtimesDefault lines_to_pad [""] <> nom_output
      output_to_print_with_newline_annotations = zip (howToGoToNextLine last_printed_line_count reflow_line_count_correction <$> [0 ..]) output_to_print
      output =
        toStrict
          . Builder.toLazyByteString
          $ startAtomicUpdate
          <>
          -- when we clear the line, but don‘t use cursorUpLine, the cursor needs to be moved to the start for printing.
          -- we do that before clearing because we can
          memptyIfFalse (last_printed_line_count == 1) (Builder.stringUtf8 $ Terminal.setCursorColumnCode 0)
          <>
          -- Clear last output from screen.
          -- First we clear the current line, if we have written on it.
          memptyIfFalse (last_printed_line_count > 0) (Builder.stringUtf8 Terminal.clearLineCode)
          <>
          -- Then, if necessary we, move up and clear more lines.
          stimesMonoid
            (max (last_printed_line_count - 1) 0)
            ( Builder.stringUtf8 (Terminal.cursorUpLineCode 1) -- Moves cursor one line up and to the beginning of the line.
                <> Builder.stringUtf8 Terminal.clearLineCode -- We are avoiding to use clearFromCursorToScreenEnd
                -- because it apparently triggers a flush on some terminals.
            )
          <>
          -- Insert the output to write to the screen.
          ( output_to_print_with_newline_annotations & foldMap \(newline, line) ->
              ( case newline of
                  StayInLine -> mempty
                  MoveToNextLine -> Builder.stringUtf8 (Terminal.cursorDownLineCode 1)
                  PrintNewLine -> Builder.byteString "\n"
              )
                <> Builder.byteString line
          )
          -- Corner case: If nom is not outputting anything but we are printing output from nix, then we want to append a newline
          <> memptyIfFalse (nom_output_length == 0 && nix_output_length > 0) Builder.byteString "\n"
          <> endAtomicUpdate

  -- Actually write to the buffer. We do this all in one step and with a strict
  -- ByteString so that everything is precalculated and the actual put is
  -- definitely just a simple copy.  Any delay while writing could create
  -- flickering.
  ByteString.hPut output_handle output
  System.IO.hFlush output_handle

data ToNextLine = StayInLine | MoveToNextLine | PrintNewLine

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
  StreamParser update ->
  UpdateFunc update state ->
  (Double -> state -> state) ->
  OutputFunc state ->
  Finalizer state ->
  Stream (Either NOMError ByteString) ->
  Handle ->
  state ->
  IO state
interact config parser updater maintenance printer finalize input_stream output_handle initialState =
  processTextStream config parser updater maintenance (Just (printer, output_handle)) finalize initialState input_stream

-- frame durations are passed to threadDelay and thus are given in microseconds

maxFrameDuration :: Int
maxFrameDuration = 1_000_000 -- once per second to update timestamps

minFrameDuration :: Int
minFrameDuration =
  -- this seems to be a nice compromise to reduce excessive
  -- flickering, since the movement is not continuous this low frequency doesn‘t
  -- feel to sluggish for the eye, for me.
  60_000 -- ~17 times per second

processTextStream ::
  forall update state.
  Config ->
  StreamParser update ->
  UpdateFunc update state ->
  (Double -> state -> state) ->
  Maybe (OutputFunc state, Handle) ->
  Finalizer state ->
  state ->
  Stream (Either NOMError ByteString) ->
  IO state
processTextStream config parser updater maintenance printerMay finalize initialState inputStream = do
  state_var <- newTMVarIO initialState
  output_builder_var <- newTVarIO []
  refresh_display_var <- newTVarIO False
  let keepProcessing :: IO ()
      keepProcessing =
        inputStream
          & Stream.tap (errorsToBuilderFold output_builder_var)
          & Stream.mapMaybe rightToMaybe
          & parser
          & Stream.fold (Fold.drainMapM (runUpdate output_builder_var state_var refresh_display_var updater))
      waitForInput :: IO ()
      waitForInput = atomically $ check =<< readTVar refresh_display_var
  printerMay & maybe keepProcessing \(printer, output_handle) -> do
    linesVar <- newTVarIO 0
    let writeToScreen :: IO ()
        writeToScreen = writeStateToScreen (not config.silent) linesVar state_var output_builder_var refresh_display_var maintenance printer output_handle
        keepPrinting :: IO ()
        keepPrinting = forever do
          race_ (concurrently_ (threadDelay minFrameDuration) waitForInput) (threadDelay maxFrameDuration)
          writeToScreen
    race_ keepProcessing keepPrinting
    atomically (takeTMVar state_var) >>= execStateT finalize >>= atomically . putTMVar state_var
    writeToScreen
  (if isNothing printerMay then (>>= execStateT finalize) else id) $ atomically $ takeTMVar state_var

errorsToBuilderFold :: TVar [ByteString] -> Fold.Fold IO (Either NOMError ByteString) ()
errorsToBuilderFold builder_var = Fold.drainMapM saveInput
 where
  saveInput :: Either NOMError ByteString -> IO ()
  saveInput = \case
    Left nom_error -> atomically $ writeErrorToBuilder builder_var nom_error
    _ -> pass

writeErrorToBuilder :: TVar [ByteString] -> NOMError -> STM ()
writeErrorToBuilder output_builder_var nom_error = do
  modifyTVar' output_builder_var (appendError nom_error)

appendError :: NOMError -> [ByteString] -> [ByteString]
appendError err prev = error_line : prev
 where
  !error_line = optional_linebreak <> nomError <> show err <> "\n"
  optional_linebreak
    | (last_chunk : _) <- prev
    , not (ByteString.isSuffixOf "\n" last_chunk) =
        "\n"
    | otherwise = ""

nomError :: ByteString
nomError = encodeUtf8 (markup (red . bold) "nix-output-monitor error: ")

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
