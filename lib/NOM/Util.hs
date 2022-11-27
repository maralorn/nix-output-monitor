module NOM.Util (foldMapEndo, forMaybeM, parseOne, parseOneText) where

import Data.Attoparsec.ByteString qualified as Parser
import Relude

foldMapEndo :: Foldable f => (b -> a -> a) -> f b -> a -> a
foldMapEndo f = appEndo . foldMap (Endo . f)

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM

parseOneText :: ConvertUtf8 a ByteString => Parser.Parser b -> a -> Maybe (ByteString, b)
parseOneText parser = parseOne parser . encodeUtf8

parseOne :: Parser.Parser b -> ByteString -> Maybe (ByteString, b)
parseOne parser input = case Parser.parse parser input of
  Parser.Done x a -> Just (x, a)
  _ -> Nothing
