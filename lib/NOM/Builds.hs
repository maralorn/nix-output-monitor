module NOM.Builds (
  forgetProto,
  parseHost,
  Derivation (..),
  StorePath (..),
  Host (..),
  FailType (..),
  parseStorePath,
  parseDerivation,
  storePathByteStringParser,
  derivationByteStringParser,
  parseIndentedStoreObject,
) where

import Data.Attoparsec.ByteString qualified as Parser
import Data.Attoparsec.ByteString.Char8 qualified as Parser.Char
import Data.Attoparsec.Text qualified as TextParser
import Data.Text qualified as Text
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude

data StorePath = StorePath
  { hash :: Text
  , name :: Text
  }
  deriving stock (Show, Ord, Eq)

makeFieldLabelsNoPrefix ''StorePath

storePathByteStringParser :: Parser.Parser StorePath
storePathByteStringParser =
  StorePath
    . decodeUtf8
    <$> (Parser.string storePrefixBS *> Parser.take 32)
    <*> (decodeUtf8 <$> (Parser.Char.char '-' *> Parser.takeWhile (Parser.inClass "a-zA-Z0-9?=_.+-")))

derivationByteStringParser :: Parser.Parser Derivation
derivationByteStringParser =
  storePathByteStringParser >>= storePathToDerivation

storePathTextParser :: TextParser.Parser StorePath
storePathTextParser =
  StorePath
    <$> (TextParser.string storePrefix *> TextParser.take 32)
    <*> (TextParser.char '-' *> TextParser.takeWhile (TextParser.inClass "a-zA-Z0-9?=_.+-"))

derivationTextParser :: TextParser.Parser Derivation
derivationTextParser =
  storePathTextParser >>= storePathToDerivation

storePathToDerivation :: (MonadFail m) => StorePath -> m Derivation
storePathToDerivation path = case Text.stripSuffix ".drv" path.name of
  Just realName -> pure $ Derivation $ path{name = realName}
  Nothing -> fail "StorePath is not a derivation."

indentedStoreObjectTextParser :: TextParser.Parser (Either Derivation StorePath)
indentedStoreObjectTextParser =
  ( StorePath
      <$> (TextParser.string ("  " <> storePrefix) *> TextParser.take 32)
      <*> (TextParser.char '-' *> TextParser.takeText)
  )
    <&> \path -> case storePathToDerivation path of
      Just drv -> Left drv
      Nothing -> Right path

parseDerivation :: (MonadFail m) => Text -> m Derivation
parseDerivation = either fail pure . TextParser.parseOnly (derivationTextParser <* TextParser.endOfInput)

parseStorePath :: (MonadFail m) => Text -> m StorePath
parseStorePath = either fail pure . TextParser.parseOnly (storePathTextParser <* TextParser.endOfInput)

parseIndentedStoreObject :: (MonadFail m) => Text -> m (Either Derivation StorePath)
parseIndentedStoreObject = either fail pure . TextParser.parseOnly indentedStoreObjectTextParser

parseHost :: Text -> Host True
parseHost host
  | host `elem` ["", "local", "local://", "unix", "unix://"] = Localhost
  | otherwise = uncurry Host $ second (Text.drop 3) $ Text.breakOn "://" host

forgetProto :: Host True -> Host False
forgetProto = \case
  Localhost -> Localhost
  Host _ x -> Hostname x

newtype Derivation = Derivation {storePath :: StorePath}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance ToText Derivation where
  toText drv = toText drv.storePath <> ".drv"

instance ToString Derivation where
  toString = toString . toText

storePrefixBS :: ByteString
storePrefixBS = encodeUtf8 storePrefix

storePrefix :: Text
storePrefix = "/nix/store/"

instance ToText StorePath where
  toText path = storePrefix <> path.hash <> "-" <> path.name

instance ToString StorePath where
  toString = toString . toText

data Host (a :: Bool) where
  Localhost :: Host a
  Host :: Text -> Text -> Host True
  Hostname :: Text -> Host False

deriving stock instance Ord (Host a)

deriving stock instance Eq (Host a)

deriving stock instance Show (Host a)

instance ToText (Host a) where
  toText (Host prot name) = prot <> "://" <> name
  toText (Hostname name) = name
  toText Localhost = "localhost"

instance ToString (Host a) where
  toString = toString . toText

data FailType = ExitCode Int | HashMismatch
  deriving stock (Show, Eq, Ord)
