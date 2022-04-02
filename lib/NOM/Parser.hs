module NOM.Parser (parser, ParseResult (..), parseStorePath, parseDerivation) where

import Relude hiding (take, takeWhile)

import Data.Attoparsec.ByteString (
  Parser,
  choice,
  endOfInput,
  inClass,
  manyTill',
  parseOnly,
  string,
  take,
  takeWhile,
 )
import Data.Attoparsec.ByteString qualified as ParseW8
import Data.Attoparsec.ByteString.Char8 (
  anyChar,
  char,
  decimal,
  double,
  endOfLine,
  isEndOfLine,
  takeTill,
 )
import Data.Text (stripSuffix)

import NOM.Builds (
  Derivation (..),
  FailType (ExitCode, HashMismatch),
  Host (..),
  StorePath (..),
  storePrefix,
 )
import NOM.Error (NOMError (ParseInternalJSONError))
import NOM.Util (hush)

data InternalJSON = InternalJSON deriving stock (Show, Eq)

data ParseResult
  = Uploading !StorePath !Host
  | Downloading !StorePath !Host
  | PlanCopies !Int
  | Build Derivation !Host
  | PlanBuilds (Set Derivation) !Derivation
  | PlanDownloads !Double !Double (Set StorePath)
  | Checking !Derivation
  | Failed !Derivation !FailType
  | JSONMessage !(Either NOMError InternalJSON)
  deriving stock (Show, Eq)

parser :: Parser (Maybe ParseResult)
parser = Just <$> updateParser <|> Nothing <$ noMatch

updateParser :: Parser ParseResult
updateParser = jsonMessage <|> planBuilds <|> planDownloads <|> copying <|> building <|> failed <|> checking

jsonMessage :: Parser ParseResult
jsonMessage = JSONMessage (Left ParseInternalJSONError) <$ string "@nix " <* noMatch

storePrefixBS :: ByteString
storePrefixBS = encodeUtf8 storePrefix

noMatch :: Parser ()
noMatch = void $ ParseW8.takeTill isEndOfLine <* endOfLine

storePath :: Parser StorePath
storePath =
  StorePath
    <$> (decodeUtf8 <$> (string storePrefixBS *> take 32))
    <*> (decodeUtf8 <$> (char '-' *> takeWhile (inClass "a-zA-Z0-9?=_.+-")))

derivation :: Parser Derivation
derivation =
  storePath >>= \x -> case stripSuffix ".drv" x.name of
    Just realName -> pure . Derivation $ x{name = realName}
    Nothing -> mzero

inTicks :: Parser a -> Parser a
inTicks x = tick *> x <* tick

tick :: Parser ()
tick = () <$ char '\''

noTicks :: Parser ByteString
noTicks = takeTill (== '\'')

host :: Parser Host
host = Host . decodeUtf8 <$> inTicks noTicks

ellipsisEnd :: Parser ()
ellipsisEnd = string "..." >> endOfLine

indent :: Parser ()
indent = () <$ string "  "

-- these (<decimal> )?derivations will be built:
--  /nix/store/4lj96sc0pyf76p4w6irh52wmgikx8qw2-nix-output-monitor-0.1.0.3.drv
planBuilds :: Parser ParseResult
planBuilds =
  maybe mzero (\x -> pure (PlanBuilds (fromList (toList x)) (last x))) . nonEmpty
    =<< choice
      [ string "these derivations will be built:"
      , string "this derivation will be built:"
      , string "these " *> (decimal :: Parser Int) *> string " derivations will be built:"
      ]
      *> endOfLine
      *> many planBuildLine

planBuildLine :: Parser Derivation
planBuildLine = indent *> derivation <* endOfLine

planDownloads :: Parser ParseResult
planDownloads =
  PlanDownloads
    <$> ( choice
            [ string "these paths"
            , string "this path"
            , string "these " *> (decimal :: Parser Int) *> string " paths"
            ]
            *> string " will be fetched ("
            *> double
        )
    <*> (string " MiB download, " *> double)
    <*> (string " MiB unpacked):" *> endOfLine *> (fromList <$> many planDownloadLine))

planDownloadLine :: Parser StorePath
planDownloadLine = indent *> storePath <* endOfLine

failed :: Parser ParseResult
-- builder for '/nix/store/fbpdwqrfwr18nn504kb5jqx7s06l1mar-regex-base-0.94.0.1.drv' failed with exit code 1
failed =
  Failed
    <$> ( choice
            [ string "error: build of " <* inTicks derivation <* manyTill' anyChar (string "failed: error: ")
            , string "error: "
            , pure ""
            ]
            *> string "builder for "
            *> inTicks derivation
            <* string " failed with exit code "
        )
      <*> (ExitCode <$> decimal <* choice [endOfLine, char ';' *> endOfLine])
    <|>
    -- error: hash mismatch in fixed-output derivation '/nix/store/nrx4swgzs3iy049fqfx51vhnbb9kzkyv-source.drv':
    Failed
    <$> (choice [string "error: ", pure ""] *> string "hash mismatch in fixed-output derivation " *> inTicks derivation <* string ":") <*> pure HashMismatch <* endOfLine

-- checking outputs of '/nix/store/xxqgv6kwf6yz35jslsar0kx4f03qzyis-nix-output-monitor-0.1.0.3.drv'...
checking :: Parser ParseResult
checking = Checking <$> (string "checking outputs of " *> inTicks derivation <* ellipsisEnd)

-- copying 1 paths...
-- copying path '/nix/store/fzyahnw94msbl4ic5vwlnyakslq4x1qm-source' to 'ssh://maralorn@example.org'...
copying :: Parser ParseResult
copying =
  string "copying "
    *> (transmission <|> PlanCopies <$> decimal <* string " paths" <* ellipsisEnd)

transmission :: Parser ParseResult
transmission = do
  p <- string "path " *> inTicks storePath
  (Uploading p <$> toHost <|> Downloading p <$> fromHost) <* ellipsisEnd

fromHost :: Parser Host
fromHost = string " from " *> host

toHost :: Parser Host
toHost = string " to " *> host

onHost :: Parser Host
onHost = string " on " *> host

-- building '/nix/store/4lj96sc0pyf76p4w6irh52wmgikx8qw2-nix-output-monitor-0.1.0.3.drv' on 'ssh://maralorn@example.org'...
building :: Parser ParseResult
building = do
  p <- string "building " *> inTicks derivation
  Build p Localhost <$ ellipsisEnd <|> Build p <$> onHost <* ellipsisEnd

parseStorePath :: FilePath -> Maybe StorePath
parseStorePath = hush . parseOnly (storePath <* endOfInput) . encodeUtf8

parseDerivation :: FilePath -> Maybe Derivation
parseDerivation = hush . parseOnly (derivation <* endOfInput) . encodeUtf8
