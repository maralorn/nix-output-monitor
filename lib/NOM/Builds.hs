module NOM.Builds (parseHost, Derivation (..), StorePath (..), Host (..), FailType (..), parseStorePath, parseDerivation, storePathParser, derivation) where

import Relude

import Data.Attoparsec.ByteString qualified as Parser
import Data.Attoparsec.ByteString.Char8 qualified as CharParser
import Data.Text qualified as Text

import NOM.Util (hush)

data StorePath = StorePath
  { hash :: !Text
  , name :: !Text
  }
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (NFData)

storePathParser :: Parser.Parser StorePath
storePathParser =
  StorePath
    <$> (decodeUtf8 <$> (Parser.string storePrefixBS *> Parser.take 32))
    <*> (decodeUtf8 <$> (CharParser.char '-' *> Parser.takeWhile (Parser.inClass "a-zA-Z0-9?=_.+-")))

derivation :: Parser.Parser Derivation
derivation =
  storePathParser >>= \x -> case Text.stripSuffix ".drv" x.name of
    Just realName -> pure . Derivation $ x{name = realName}
    Nothing -> mzero

parseDerivation :: ConvertUtf8 a ByteString => a -> Maybe Derivation
parseDerivation = hush . Parser.parseOnly (derivation <* Parser.endOfInput) . encodeUtf8

parseStorePath :: ConvertUtf8 a ByteString => a -> Maybe StorePath
parseStorePath = hush . Parser.parseOnly (storePathParser <* Parser.endOfInput) . encodeUtf8

parseHost :: Text -> Host
parseHost = \case
  "" -> Localhost
  "local" -> Localhost
  host -> Host host

newtype Derivation = Derivation {storePath :: StorePath}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData)

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

data Host = Localhost | Host !Text
  deriving stock (Ord, Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToText Host where
  toText (Host name) = name
  toText Localhost = "localhost"

instance ToString Host where
  toString = toString . toText

data FailType = ExitCode Int | HashMismatch
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
