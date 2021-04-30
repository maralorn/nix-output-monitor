module IO where

import Relude
import Prelude ()

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.STM (check, swapTVar)
import Control.Exception (IOException, catch)
import Data.Attoparsec.Text.Lazy (Parser, Result (Done, Fail), match, parse)
import qualified Data.Text as Text
import Data.Text.Lazy as LText (Text)
import Data.Text.Lazy.IO as LTextIO (getContents)
import Data.Time (UTCTime, getCurrentTime)
import System.Console.ANSI (SGR (Reset), clearFromCursorToScreenEnd, cursorUpLine, setSGRCode)
import System.Console.Terminal.Size (Window (Window), size)
import System.IO (hFlush)

import Table (displayWidth, truncate)

processStream ::
  forall a b.
  Parser a ->
  TVar b ->
  (a -> b -> IO b) ->
  (UTCTime -> b -> Text.Text) ->
  IO b
processStream parser stateVar updater printer = run
 where
  run = catch go (\(_ :: IOException) -> go)
  go = processText parser stateVar updater (Just printer) =<< LTextIO.getContents

processText ::
  forall a b.
  Parser a ->
  TVar b ->
  (a -> b -> IO b) ->
  Maybe (UTCTime -> b -> Text.Text) ->
  LText.Text ->
  IO b
processText parser stateVar updater printerMay lazyInput = do
  bufferVar <- newTVarIO ""
  linesVar <- newTVarIO 0
  let keepPrinting :: IO ()
      keepPrinting = forever $ do
        race_ (concurrently_ (threadDelay 20000) waitForInput) (threadDelay 1000000)
        writeToScreen
      keepProcessing :: IO ()
      keepProcessing = mapM_ processResult . parseStream (match parser) $ lazyInput
      processResult :: (Text.Text, a) -> IO ()
      processResult (text, result) = do
        oldState <- readTVarIO stateVar
        newState <- liftIO $ updater result oldState
        atomically do
          writeTVar stateVar newState
          modifyTVar' bufferVar (<> text)
      writeToScreen :: IO ()
      writeToScreen =
        printerMay & maybe pass \printer -> do
          now <- getCurrentTime
          terminalSize <- size
          (writtenLines, buffer, linesToWrite, output) <- atomically $ do
            buildState <- readTVar stateVar
            let output = truncateOutput terminalSize (printer now buildState)
                linesToWrite = Relude.length (Text.lines output)
            writtenLines <- swapTVar linesVar linesToWrite
            buffer <- swapTVar bufferVar ""
            pure (writtenLines, buffer, linesToWrite, output)
          liftIO $ do
            when (writtenLines > 0) do
              cursorUpLine writtenLines
              clearFromCursorToScreenEnd
            putText buffer
            when (linesToWrite > 0) $ putTextLn output
            hFlush stdout
      waitForInput :: IO ()
      waitForInput =
        atomically $ check . not . Text.null =<< readTVar bufferVar
  race_ keepProcessing keepPrinting
  writeToScreen
  readTVarIO stateVar

truncateOutput :: Maybe (Window Int) -> Text.Text -> Text.Text
truncateOutput win output = maybe output go win
 where
  go (Window rows columns) = Text.intercalate "\n" $ truncateColumns columns <$> truncatedRows rows
  truncateColumns columns line = if displayWidth line > columns then Table.truncate (columns - 1) line <> "…" <> toText (setSGRCode [Reset]) else line
  truncatedRows rows =
    if length outputLines >= rows
      then take 1 outputLines <> [" ⋮ "] <> drop (length outputLines + 4 - rows) outputLines
      else outputLines
  outputLines = Text.lines output

parseStream :: Parser a -> LText.Text -> [a]
parseStream parser input = case parse parser input of
  Fail{} -> []
  Done rest result -> result : parseStream parser rest
