module NOM.StreamParser (parseStreamAttoparsec, parseOneText, stripANSICodes) where

import Data.Attoparsec.ByteString (IResult (..), Parser, Result, parse)
import Data.ByteString qualified as ByteString
import Data.Word8 qualified as Word8
import Relude
import Streamly.Prelude ((.:))
import Streamly.Prelude qualified as Stream

type ContParser update = (ByteString -> Result update, ByteString)

parseChunk :: forall update m. Monad m => ContParser update -> (ByteString, ByteString) -> Stream.SerialT (StateT (ContParser update) m) (update, ByteString)
parseChunk initState (strippedInput, rawInput) = join $ state \(currentParser, consumed) ->
  case currentParser strippedInput of
    Done "" result -> (pure (result, consumed <> rawInput), initState)
    Done rest result ->
      let (consumedNow, rawLeft) = ByteString.splitAt (ByteString.length strippedInput - ByteString.length rest) rawInput
       in ((result, consumed <> consumedNow) .: parseChunk initState (rest, rawLeft), initState)
    Fail{} -> (Stream.nil, second (const (consumed <> rawInput)) initState)
    Partial cont -> (Stream.nil, (cont, consumed <> rawInput))

csi :: ByteString
csi = "\27["

breakOnANSIStartCode :: ByteString -> (ByteString, ByteString)
breakOnANSIStartCode = ByteString.breakSubstring csi

streamANSIChunks :: ByteString -> Stream.SerialT m (ByteString, ByteString)
streamANSIChunks input =
  let (filtered, unfiltered) = breakOnANSIStartCode input
      (codeParts, rest) = ByteString.break Word8.isLetter unfiltered
      (code, restOfStream) = case ByteString.uncons rest of
        Just (headOfRest, tailOfRest) -> (ByteString.snoc codeParts headOfRest, streamANSIChunks tailOfRest)
        Nothing -> (codeParts, Stream.nil)
   in (filtered, filtered <> code) .: restOfStream

parseStreamAttoparsec :: Monad m => Parser update -> Stream.SerialT m ByteString -> Stream.SerialT m (update, ByteString)
parseStreamAttoparsec parser =
  let parserInitState = (parse parser, mempty)
   in Stream.map snd . Stream.runStateT (pure parserInitState) . Stream.concatMap (parseChunk parserInitState) . Stream.liftInner . Stream.concatMap streamANSIChunks

parseOneText :: Parser update -> Text -> Maybe update
parseOneText parser input = do
  (result, _) <- runIdentity $ Stream.head (parseStreamAttoparsec parser (pure (encodeUtf8 input)))
  pure result

stripANSICodes :: Text -> Text
stripANSICodes = decodeUtf8 . ByteString.concat . runIdentity . Stream.toList . fmap fst . streamANSIChunks . encodeUtf8
