module NOM.IO where

import Relude

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.STM (check, modifyTVar, swapTVar)
import Control.Exception (IOException, try)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import Data.Time (ZonedTime, getZonedTime)
import qualified Data.Word8 as Word8
import qualified System.IO

import qualified Streamly.Data.Fold as Fold
import Streamly.Prelude ((.:))
import qualified Streamly.Prelude as Stream

import Data.Attoparsec.ByteString (IResult (..), Parser, Result, parse)

import System.Console.ANSI (SGR (Reset), clearLine, cursorUpLine, setSGRCode)
import System.Console.Terminal.Size (Window (Window), size)

import NOM.Error (NOMError (InputError))
import NOM.Print.Table as Table (bold, displayWidth, markup, red, truncate)
import NOM.Update.Monad (UpdateMonad)
import NOM.Util ((.>), (|>))

type Stream = Stream.SerialT IO
type Output = Text
type UpdateFunc update state = forall m. UpdateMonad m => (update, ByteString) -> StateT state m ([NOMError], ByteString)
type OutputFunc state = state -> Maybe (Window Int) -> ZonedTime -> Output
type Finalizer state = forall m. UpdateMonad m => StateT state m ()

type ContParser update = (ByteString -> Result update, ByteString)

parseChunk :: forall update. ContParser update -> (ByteString, ByteString) -> Stream.SerialT (StateT (ContParser update) IO) (update, ByteString)
parseChunk initState (strippedInput, rawInput) = join $ state \(currentParser, consumed) ->
  case currentParser strippedInput of
    Done "" result -> (pure (result, consumed <> rawInput), initState)
    Done rest result ->
      let (consumedNow, rawLeft) = ByteString.splitAt (ByteString.length strippedInput - ByteString.length rest) rawInput
       in ((result, consumed <> consumedNow) .: parseChunk initState (rest, rawLeft), initState)
    Fail{} -> (Stream.nil, second (const (consumed <> rawInput)) initState)
    Partial cont -> (Stream.nil, (cont, consumed <> rawInput))

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
      Left err -> Left (InputError err) .: loop -- Ignore Exception
      Right "" -> mempty -- EOF
      Right input -> Right input .: loop

csi :: ByteString
csi = "\27["
breakOnANSIStartCode :: ByteString -> (ByteString, ByteString)
breakOnANSIStartCode = ByteString.breakSubstring csi
streamANSIChunks :: ByteString -> Stream (ByteString, ByteString)
streamANSIChunks input =
  let (filtered, unfiltered) = breakOnANSIStartCode input
      (codeParts, rest) = ByteString.break Word8.isLetter unfiltered
      (code, restOfStream) = case ByteString.uncons rest of
        Just (headOfRest, tailOfRest) -> (ByteString.snoc codeParts headOfRest, streamANSIChunks tailOfRest)
        Nothing -> (codeParts, Stream.nil)
   in (filtered, filtered <> code) .: restOfStream

runUpdate ::
  forall update state.
  TVar ByteString ->
  TVar state ->
  UpdateFunc update state ->
  (update, ByteString) ->
  IO ByteString
runUpdate bufferVar stateVar updater input = do
  oldState <- readTVarIO stateVar
  ((errors, output), newState) <- runStateT (updater input) oldState
  atomically $ do
    forM_ errors (\error' -> modifyTVar bufferVar (<> printError error'))
    writeTVar stateVar newState
  pure output

writeStateToScreen :: forall state. TVar Int -> TVar state -> TVar ByteString -> (state -> state) -> OutputFunc state -> IO ()
writeStateToScreen linesVar stateVar bufferVar maintenance printer = do
  now <- getZonedTime
  terminalSize <- size

  -- Do this strictly so that rendering the output does not flicker
  (!writtenLines, !buffer, !linesToWrite, !output) <- atomically $ do
    unprepared_nom_state <- readTVar stateVar
    let prepared_nom_state = maintenance unprepared_nom_state
        output = truncateOutput terminalSize (printer prepared_nom_state terminalSize now)
        linesToWrite = length (Text.lines output)
    writeTVar stateVar prepared_nom_state
    writtenLines <- swapTVar linesVar linesToWrite
    buffer <- swapTVar bufferVar mempty
    pure (writtenLines, buffer, linesToWrite, output)

  -- Clear last output from screen.
  replicateM_ writtenLines do
    cursorUpLine 1
    clearLine

  -- Write new output to screen.
  ByteString.putStr buffer
  when (linesToWrite > 0) $ putTextLn output
  System.IO.hFlush stdout

interact ::
  forall update state.
  Parser update ->
  UpdateFunc update state ->
  (state -> state) ->
  OutputFunc state ->
  Finalizer state ->
  state ->
  IO state
interact parser updater maintenance printer finalize initialState =
  readTextChunks stdin
    |> processTextStream parser updater maintenance (Just printer) finalize initialState

processTextStream ::
  forall update state.
  Parser update ->
  UpdateFunc update state ->
  (state -> state) ->
  Maybe (OutputFunc state) ->
  Finalizer state ->
  state ->
  Stream (Either NOMError ByteString) ->
  IO state
processTextStream parser updater maintenance printerMay finalize initialState inputStream = do
  stateVar <- newTVarIO initialState
  bufferVar <- newTVarIO mempty
  let parserInitState = (parse parser, mempty)
  let keepProcessing :: IO ()
      keepProcessing =
        inputStream
          |> Stream.tap (writeErrorsToBuffer bufferVar)
          .> Stream.mapMaybe rightToMaybe
          .> Stream.concatMap streamANSIChunks
          .> Stream.liftInner
          .> Stream.concatMap (parseChunk parserInitState)
          .> Stream.runStateT (pure parserInitState)
          .> Stream.mapM (snd .> runUpdate bufferVar stateVar updater >=> flip (<>) .> modifyTVar bufferVar .> atomically)
          .> Stream.drain
      waitForInput :: IO ()
      waitForInput = atomically $ check . not . ByteString.null =<< readTVar bufferVar
  printerMay |> maybe keepProcessing \printer -> do
    linesVar <- newTVarIO 0
    let writeToScreen :: IO ()
        writeToScreen = writeStateToScreen linesVar stateVar bufferVar maintenance printer
        keepPrinting :: IO ()
        keepPrinting = forever do
          race_ (concurrently_ (threadDelay 20000) waitForInput) (threadDelay 1000000)
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
    Left error' -> atomically $ modifyTVar bufferVar (<> printError error')
    _ -> pass

printError :: NOMError -> ByteString
printError err = "\n" <> nomError <> show err <> "\n"

nomError :: ByteString
nomError = encodeUtf8 (markup (red . bold) "NOMError: ")

truncateOutput :: Maybe (Window Int) -> Text -> Text
truncateOutput win output = maybe output go win
 where
  go :: Window Int -> Text
  go (Window rows columns) = Text.intercalate "\n" $ truncateColumns columns <$> truncatedRows rows

  truncateColumns :: Int -> Text -> Text
  truncateColumns columns line = if displayWidth line > columns then Table.truncate (columns - 1) line <> "…" <> toText (setSGRCode [Reset]) else line

  truncatedRows :: Int -> [Text]
  truncatedRows rows
    | length outputLines >= rows - outputLinesToAlwaysShow = take 1 outputLines <> [" ⋮ "] <> drop (length outputLines + outputLinesToAlwaysShow + 2 - rows) outputLines
    | otherwise = outputLines

  outputLines :: [Text]
  outputLines = Text.lines output

outputLinesToAlwaysShow :: Int
outputLinesToAlwaysShow = 5
