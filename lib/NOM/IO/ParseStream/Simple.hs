module NOM.IO.ParseStream.Simple (parseStreamSimple) where

import Data.ByteString qualified as ByteString
import Relude
import Streamly.Prelude ((.:), (|$))
import Streamly.Prelude qualified as Stream

type ContParser = Maybe ByteString

streamLines :: forall m. Monad m => ByteString -> Stream.SerialT (StateT ContParser m) ByteString
streamLines input = join $ state \currentState ->
  let wholeInput = maybe input (<> input) currentState
      inputLines = ByteString.split 10 wholeInput
      emitLines :: [ByteString] -> (Stream.SerialT n ByteString, Maybe ByteString)
      emitLines = \case
        [] -> (Stream.nil, Nothing)
        [""] -> (Stream.nil, Nothing)
        [rest] -> (Stream.nil, Just rest)
        (line : rest) -> first (line .:) (emitLines rest)
   in emitLines inputLines

chunksToLines :: Monad m => Stream.SerialT m ByteString -> Stream.SerialT m ByteString
chunksToLines =
  Stream.map snd
    . Stream.runStateT (pure Nothing)
    . Stream.concatMap streamLines
    . Stream.liftInner

parseStreamSimple :: Stream.MonadAsync m => (ByteString -> update) -> Stream.SerialT m ByteString -> Stream.SerialT m update
parseStreamSimple parser chunks = (Stream.fromAhead . Stream.map parser . Stream.fromSerial) |$ chunksToLines chunks
