module NOM.IO.ParseStream.Simple (parseStreamSimple) where

import Relude

import Data.ByteString qualified as ByteString
import Streamly.Prelude ((.:))
import Streamly.Prelude qualified as Stream

type ContParser = Maybe ByteString

parseChunk :: forall update m. Monad m => (ByteString -> update) -> ByteString -> Stream.SerialT (StateT ContParser m) (update, ByteString)
parseChunk parser input = join $ state \currentState ->
   let
   wholeInput = maybe input (<> input) currentState
   inputLines = ByteString.split 10 wholeInput
   emitLines = \case
    [] -> (Stream.nil, Nothing)
    [""] -> (Stream.nil, Nothing)
    [rest] -> (Stream.nil, Just rest)
    (line:rest) -> first ((parser line,line) .:) (emitLines rest)
   in
      emitLines inputLines

parseStreamSimple :: Monad m => (ByteString -> update) -> Stream.SerialT m ByteString -> Stream.SerialT m (update, ByteString)
parseStreamSimple parser =
    Stream.map snd . Stream.runStateT (pure Nothing) . Stream.concatMap (parseChunk parser) . Stream.liftInner
