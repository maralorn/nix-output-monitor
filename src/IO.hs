module IO where

import Relude
import Prelude ()

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, race_, withAsync)
import Control.Concurrent.STM (check, swapTVar)
import Data.Attoparsec.Text.Lazy (Parser, Result (Done, Fail), match, parse)
import Data.Text as Text (Text, lines, null)
import Data.Text.Lazy as LText (Text)
import Data.Text.Lazy.IO as LTextIO (getContents)
import Data.Time (UTCTime, getCurrentTime)
import System.Console.ANSI (clearFromCursorToScreenEnd, cursorUpLine)
import System.IO (hFlush)

processStream ::
  forall a b.
  Parser a ->
  b ->
  (a -> b -> IO b) ->
  (UTCTime -> b -> Text.Text) ->
  IO b
processStream parser initalState updater printer =
  processText parser initalState updater (Just printer)
    =<< LTextIO.getContents

processText ::
  forall a b.
  Parser a ->
  b ->
  (a -> b -> IO b) ->
  Maybe (UTCTime -> b -> Text.Text) ->
  LText.Text ->
  IO b
processText parser initialState updater printerMay lazyInput = do
  bufferVar <- newTVarIO ""
  stateVar <- newTVarIO initialState
  linesVar <- newTVarIO 0
  let keepPrinting :: IO ()
      keepPrinting = forever $ do
        race_ (concurrently_ (threadDelay 20000) waitForInput) (threadDelay 1000000)
        writeToScreen
      keepProcessing :: IO ()
      keepProcessing =
        mapM_ processResult . parseStream (match parser) $ lazyInput
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
          (writtenLines, buffer, linesToWrite, output) <- atomically $ do
            buildState <- readTVar stateVar
            let output = printer now buildState
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
      checkForInput :: IO ()
      checkForInput = race_ waitForInput do
        threadDelay 10000000
        putStrLn "No input for more than 10 seconds. Have you redirected nix-build stderr into nom? Please read the README."
  withAsync checkForInput $ const (race_ keepProcessing keepPrinting)
  writeToScreen
  readTVarIO stateVar

parseStream :: Parser a -> LText.Text -> [a]
parseStream parser input = case parse parser input of
  Fail{} -> []
  Done rest result -> result : parseStream parser rest
