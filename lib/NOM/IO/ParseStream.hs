module NOM.IO.ParseStream (parseStream, parseOneText, stripANSICodes) where

import Data.Attoparsec.ByteString (IResult (..), Parser, Result, parse)
import Data.ByteString qualified as ByteString
import Data.Word8 qualified as Word8
import NOM.Util ((.>))
import Relude
import Streamly.Prelude ((.:))
import Streamly.Prelude qualified as Stream

type ContParser update = (ByteString -> Result update, ByteString)

parseChunk :: forall update m. Monad m => ContParser update -> (ByteString, ByteString) -> Stream.SerialT (StateT (ContParser update) m) (update, ByteString)
parseChunk initState (strippedInput, rawInput) = join $ state \(currentParser, consumed) ->
  case currentParser strippedInput of
    Done "" result -> (pure (result, consumed <> rawInput), initState)
    Done rest result ->
      let (!consumedNow, !rawLeft) = ByteString.splitAt (ByteString.length strippedInput - ByteString.length rest) rawInput
       in ((result, consumed <> consumedNow) .: parseChunk initState (rest, rawLeft), initState)
    Fail{} -> (Stream.nil, second (const (consumed <> rawInput)) initState)
    Partial cont -> (Stream.nil, (cont, consumed <> rawInput))

csi :: ByteString
csi = "\27["
breakOnANSIStartCode :: ByteString -> (ByteString, ByteString)
breakOnANSIStartCode = ByteString.breakSubstring csi
streamANSIChunks :: ByteString -> Stream.SerialT m (ByteString, ByteString)
streamANSIChunks input =
  let (!filtered, !unfiltered) = breakOnANSIStartCode input
      (!codeParts, !rest) = ByteString.break Word8.isLetter unfiltered
      (!code, !restOfStream) = case ByteString.uncons rest of
        Just (!headOfRest, !tailOfRest) -> (ByteString.snoc codeParts headOfRest, streamANSIChunks tailOfRest)
        Nothing -> (codeParts, Stream.nil)
   in (filtered, filtered <> code) .: restOfStream

parseStream :: Monad m => Parser update -> Stream.SerialT m ByteString -> Stream.SerialT m (ContParser update, (update, ByteString))
parseStream parser =
  let parserInitState = (parse parser, mempty)
   in Stream.concatMap streamANSIChunks
        .> Stream.liftInner
        .> Stream.concatMap (parseChunk parserInitState)
        .> Stream.runStateT (pure parserInitState)

parseOneText :: Parser update -> Text -> Maybe update
parseOneText parser input = do
  (_, (result, _)) <- runIdentity $ Stream.head (parseStream parser (pure (encodeUtf8 input)))
  pure result

stripANSICodes :: Text -> Text
stripANSICodes = encodeUtf8 .> streamANSIChunks .> fmap fst .> Stream.toList .> runIdentity .> ByteString.concat .> decodeUtf8