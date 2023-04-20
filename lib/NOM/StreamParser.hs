module NOM.StreamParser (parseStreamAttoparsec, stripANSICodes) where

import Data.Attoparsec.ByteString (IResult (..), Parser, Result, parse)
import Data.ByteString qualified as ByteString
import Data.Word8 qualified as Word8
import Relude
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.Unfold qualified as Unfold

type ContParser update = ByteString -> Result update

parseChunk ::
  Monad m =>
  ContParser update ->
  Unfold.Unfold (StateT (ContParser update) m) (ByteString, ByteString) (Maybe update, ByteString)
parseChunk fresh_parse_function = unfoldNext generate
 where
  generate input = state (transition_function input)
  transition_function (strippedInput, rawInput) current_parse_function =
    (
      ( (parsed_update, raw_parsed)
      , yet_to_parse
      )
    , next_parse_function
    )
   where
    parse_result = current_parse_function strippedInput
    parsed_update = case parse_result of
      Done _ result -> Just result
      _ -> Nothing
    split_raw_by_rest rest = ByteString.splitAt (ByteString.length strippedInput - ByteString.length rest) rawInput
    (raw_parsed, yet_to_parse) = case parse_result of
      Done rest _
        | not (ByteString.null rest)
        , (consumedNow, rawLeft) <- split_raw_by_rest rest ->
            (consumedNow, Just (rest, rawLeft))
      _ -> (rawInput, Nothing)
    next_parse_function = case parse_result of
      Partial cont -> cont
      _ -> fresh_parse_function

csi :: ByteString
csi = "\27["

breakOnANSIStartCode :: ByteString -> (ByteString, ByteString)
breakOnANSIStartCode = ByteString.breakSubstring csi

streamANSIChunks :: Monad m => Unfold.Unfold m ByteString (ByteString, ByteString)
streamANSIChunks = unfoldNext generate
 where
  generate :: Monad m => ByteString -> m ((ByteString, ByteString), Maybe ByteString)
  generate input = pure ((filtered, filtered <> code), restOfStream)
   where
    (filtered, unfiltered) = breakOnANSIStartCode input
    (codeParts, rest) = ByteString.break Word8.isLetter unfiltered
    (code, restOfStream) = case ByteString.uncons rest of
      Just (headOfRest, tailOfRest) -> (ByteString.snoc codeParts headOfRest, Just tailOfRest)
      Nothing -> (codeParts, Nothing)

{- | unfoldNext is like a normal unfold, but takes an (a, Maybe s) instead of Maybe (a, s)
this means that the generator function will definitely generate the next
stream item, but the stream can finish after that item.
-}
unfoldNext :: Monad m => (s -> m (a, Maybe s)) -> Unfold.Unfold m s a
unfoldNext =
  Unfold.lmap Just
    . Unfold.unfoldrM
    . mapM

parseStreamAttoparsec ::
  Monad m =>
  Parser update ->
  Stream.Stream m ByteString ->
  Stream.Stream m (Maybe update, ByteString)
parseStreamAttoparsec parser =
  fmap snd
    . Stream.runStateT (pure fresh_parse_func)
    . Stream.unfoldMany (Unfold.many (parseChunk fresh_parse_func) streamANSIChunks)
    . Stream.liftInner
 where
  fresh_parse_func = parse parser

stripANSICodes :: Text -> Text
stripANSICodes =
  decodeUtf8
    . ByteString.concat
    . toList @(Stream.Stream Identity)
    . Stream.unfold
      ( fmap fst
          . Unfold.lmap encodeUtf8
          $ streamANSIChunks
      )
