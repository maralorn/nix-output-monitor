module IO where

import           Prelude                        ( )
import           Relude
import           Data.Attoparsec.Text.Lazy
import           Data.Text                     as Text
import           Data.Text.Lazy                as LText
import           Data.Text.Lazy.IO             as LTextIO
import           System.IO                      ( hFlush )
import           System.Console.ANSI
import           Control.Concurrent.STM         ( swapTVar
                                                , check
                                                )
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async
import           Data.Time

processStream
  :: forall a b
   . Parser a
  -> b
  -> (a -> b -> IO b)
  -> (UTCTime -> b -> Text.Text)
  -> IO ()
processStream parser initalState updater printer =
  void
    .   processText parser initalState updater (Just printer)
    =<< LTextIO.getContents

processText
  :: forall a b
   . Parser a
  -> b
  -> (a -> b -> IO b)
  -> Maybe (UTCTime -> b -> Text.Text)
  -> LText.Text
  -> IO b
processText parser initialState updater printerMay lazyInput = do
  bufferVar <- newTVarIO ""
  stateVar  <- newTVarIO initialState
  linesVar  <- newTVarIO 0
  let
    keepPrinting :: IO ()
    keepPrinting = forever $ do
      race_ (concurrently_ (threadDelay 20000) waitForInput)  (threadDelay 1000000)
      writeToScreen
    keepProcessing :: IO ()
    keepProcessing =
      mapM_ processResult . parseStream (match parser) $ lazyInput
    processResult :: (Text.Text, a) -> IO ()
    processResult (text, result) = do
      oldState <- readTVarIO stateVar
      newState <- liftIO $ updater result oldState
      atomically $ do
        writeTVar stateVar newState
        modifyTVar' bufferVar (<> text)
    writeToScreen :: IO ()
    writeToScreen = case printerMay of
      Nothing      -> pass
      Just printer -> do
        now <- getCurrentTime
        (writtenLines, buffer, linesToWrite, output) <- atomically $ do
          buildState <- readTVar stateVar
          let output       = printer now buildState
              linesToWrite = Relude.length (Text.lines output)
          writtenLines <- swapTVar linesVar linesToWrite
          buffer       <- swapTVar bufferVar ""
          pure (writtenLines, buffer, linesToWrite, output)
        liftIO $ do
          when (writtenLines > 0) $ do
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

parseStream :: Parser a -> LText.Text -> [a]
parseStream parser input = case parse parser input of
  Fail{}           -> []
  Done rest result -> result : parseStream parser rest
