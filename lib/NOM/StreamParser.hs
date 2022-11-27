module NOM.StreamParser (parseStreamAttoparsec, stripANSICodes) where

import Data.Attoparsec.ByteString (IResult (..), Parser, Result, parse)
import Data.ByteString qualified as ByteString
import Data.Word8 qualified as Word8
import Relude
import Streamly.Prelude ((.:))
import Streamly.Prelude qualified as Stream

type ContParser update = ByteString -> Result update

parseChunk :: forall update m. Monad m => ContParser update -> (ByteString, ByteString) -> Stream.SerialT (StateT (ContParser update) m) (Maybe update, ByteString)
parseChunk fresh_parse_func = go
 where
  go (strippedInput, rawInput) = join $ state \currentParser ->
    case currentParser strippedInput of
      Done "" result -> (pure (Just result, rawInput), fresh_parse_func)
      Done rest result ->
        let (consumedNow, rawLeft) = ByteString.splitAt (ByteString.length strippedInput - ByteString.length rest) rawInput
         in ((Just result, consumedNow) .: go (rest, rawLeft), fresh_parse_func)
      Fail{} -> (pure (Nothing, rawInput), fresh_parse_func)
      Partial cont -> (pure (Nothing, rawInput), cont)

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

parseStreamAttoparsec :: Monad m => Parser update -> Stream.SerialT m ByteString -> Stream.SerialT m (Maybe update, ByteString)
parseStreamAttoparsec parser =
  Stream.map snd . Stream.runStateT (pure fresh_parse_func) . Stream.concatMap (parseChunk fresh_parse_func) . Stream.liftInner . Stream.concatMap streamANSIChunks
 where
  fresh_parse_func = parse parser

stripANSICodes :: Text -> Text
stripANSICodes = decodeUtf8 . ByteString.concat . runIdentity . Stream.toList . fmap fst . streamANSIChunks . encodeUtf8
