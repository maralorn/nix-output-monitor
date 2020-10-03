module IO where

import           Prelude                        ( )
import           Relude
import           Data.Attoparsec.Text.Lazy
import           Data.Text                     as Text
import           Data.Text.Lazy                as LText
import           Data.Text.Lazy.IO             as LTextIO
import           Data.Text.IO                  as TextIO
import           System.IO                      ( hFlush )
import           System.Console.ANSI

processStream
  :: forall a b . Parser a -> b -> (a -> b -> IO b) -> (b -> Text.Text) -> IO ()
processStream parser initalState updater printer =
  void . processText parser initalState updater (Just printer) =<< LTextIO.getContents

processText
  :: forall a b
   . Parser a
  -> b
  -> (a -> b -> IO b)
  -> Maybe (b -> Text.Text)
  -> LText.Text
  -> IO b
processText parser initalState updater printerMay lazyInput = do
  ((), (endState, _)) <-
    flip runStateT (initalState, 0)
    . mapM_ processResult
    . parseStream (match parser)
    $ lazyInput
  pure endState
 where
  processResult :: (Text.Text, a) -> StateT (b, Int) IO ()
  processResult (text, result) = do
    (oldState, writtenLines) <- get
    newState                 <- liftIO $ updater result oldState
    case printerMay of
      Nothing      -> put (newState, 0)
      Just printer -> do
        let output       = printer newState
            linesToWrite = Relude.length (Text.lines output)
        put (newState, linesToWrite)
        liftIO $ do
          when (writtenLines > 0) $ do
            cursorUpLine writtenLines
            clearFromCursorToScreenEnd
          TextIO.putStr text
          when (linesToWrite > 0) $ TextIO.putStrLn output
          hFlush stdout


parseStream :: Parser a -> LText.Text -> [a]
parseStream parser input = case parse parser input of
  Fail _ _ _       -> []
  Done rest result -> (result : parseStream parser rest)
