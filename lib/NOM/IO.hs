module NOM.IO where

import Relude

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.STM (check, modifyTVar, swapTVar)
import Control.Exception (IOException, try)
import qualified Data.Text as Text
import Data.Time (ZonedTime, getZonedTime)
import qualified System.IO

import Streamly (SerialT) -- Keep this import for streamly < 0.8 compat
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Prelude as S

import Data.Attoparsec.ByteString (IResult (..), Parser, Result, feed, parse)

import qualified Data.ByteString as ByteString
import System.Console.ANSI (SGR (Reset), clearLine, cursorUpLine, setSGRCode)
import System.Console.Terminal.Size (Window (Window), size)

import qualified Data.Word8 as Word8
import NOM.Print.Table as Table (displayWidth, truncate)
import NOM.Update.Monad (UpdateMonad)
import NOM.Util ((.>), (|>))
import Streamly.Prelude ((.:))

type Stream = SerialT IO
type Output = Text
type UpdateFunc update state = forall m. UpdateMonad m => (update -> StateT state m ())
type OutputFunc state = state -> Maybe (Window Int) -> ZonedTime -> Output
type Finalizer state = forall m. UpdateMonad m => StateT state m ()

type ContParser update = Maybe (ByteString -> Result update)

parseStream :: forall update. Parser update -> Stream ByteString -> Stream update
parseStream (parse -> parseFresh) = S.concatMap snd . S.scanl' step (Nothing, mempty)
 where
  step :: (ContParser update, Stream update) -> ByteString -> (ContParser update, Stream update)
  step (maybe parseFresh (feed . Partial) . fst -> parse') = process mempty . parse'
  process :: Stream update -> Result update -> (ContParser update, Stream update)
  process acc = \case
    Done "" result -> (Nothing, acc <> pure result)
    Done rest result -> process (acc <> pure result) (parseFresh rest)
    Fail{} -> (Nothing, acc)
    Partial cont -> (Just cont, acc)

readTextChunks :: Handle -> Stream ByteString
readTextChunks handle = loop
 where
  -- We read up-to 4kb of input at once. We will rarely need more than that for one succesful parse.
  bufferSize :: Int
  bufferSize = 4096
  tryRead :: Stream (Either IOException ByteString)
  tryRead = liftIO $ try @IOException $ ByteString.hGetSome handle bufferSize
  loop :: Stream ByteString
  loop =
    tryRead >>= \case
      Left _ -> loop -- Ignore Exception
      Right "" -> mempty -- EOF
      Right input -> input .: loop

filterANSICodes :: ByteString -> ByteString
filterANSICodes = go ""
 where
  csi = "\27["
  go acc "" = acc
  go acc remaining = go (acc <> filtered) (stripCode unfiltered)
   where
    (filtered, unfiltered) = ByteString.breakSubstring csi remaining
    stripCode bs = ByteString.drop 1 (ByteString.dropWhile (not . Word8.isLetter) bs)

runUpdates ::
  forall update state.
  TVar state ->
  UpdateFunc update state ->
  FL.Fold IO update ()
runUpdates stateVar updater = FL.drainBy \input -> do
  oldState <- readTVarIO stateVar
  newState <- execStateT (updater input) oldState
  atomically $ writeTVar stateVar newState

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
  Stream ByteString ->
  IO state
processTextStream parser updater maintenance printerMay finalize initialState inputStream = do
  stateVar <- newTVarIO initialState
  bufferVar <- newTVarIO mempty
  let keepProcessing :: IO ()
      keepProcessing =
        inputStream
          |> S.tap (saveInputIntoBuffer bufferVar)
          .> fmap filterANSICodes
          .> parseStream parser
          .> S.fold (runUpdates stateVar updater)
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

saveInputIntoBuffer :: TVar ByteString -> FL.Fold IO ByteString ()
saveInputIntoBuffer bufferVar = FL.drainBy (\input -> atomically $ modifyTVar bufferVar (<> input))

truncateOutput :: Maybe (Window Int) -> Text -> Text
truncateOutput win output = maybe output go win
 where
  go (Window rows columns) = Text.intercalate "\n" $ truncateColumns columns <$> truncatedRows rows
  truncateColumns columns line = if displayWidth line > columns then Table.truncate (columns - 1) line <> "…" <> toText (setSGRCode [Reset]) else line
  truncatedRows rows =
    if length outputLines >= rows - outputLinesToAlwaysShow
      then take 1 outputLines <> [" ⋮ "] <> drop (length outputLines + outputLinesToAlwaysShow + 2 - rows) outputLines
      else outputLines
  outputLines = Text.lines output

outputLinesToAlwaysShow :: Int
outputLinesToAlwaysShow = 5
