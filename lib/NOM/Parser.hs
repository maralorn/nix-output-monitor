module NOM.Parser (parser, updateParser, planBuildLine, planDownloadLine, inTicks, ParseResult (..), parseStorePath, parseDerivation) where

import Relude hiding (take, takeWhile)

import Data.Aeson (eitherDecodeStrict')
import Data.Attoparsec.ByteString (
  Parser,
  choice,
  manyTill',
  string,
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

import NOM.Builds (
  Derivation (..),
  FailType (ExitCode, HashMismatch),
  Host (..),
  StorePath (..),
  derivation,
  parseDerivation,
  parseStorePath,
  storePathParser,
 )
import NOM.Error (NOMError (ParseInternalJSONError))
import NOM.Parser.JSON (InternalJson)
import NOM.Util ((<|>>))

data ParseResult
  = Uploading !StorePath !Host
  | Downloading !StorePath !Host
  | PlanCopies !Int
  | Build Derivation !Host
  | PlanBuilds (Set Derivation) !Derivation
  | PlanDownloads !Double !Double (Set StorePath)
  | Checking !Derivation
  | Failed !Derivation !FailType
  | JsonMessage !(Either NOMError InternalJson)
  deriving stock (Show, Eq)

parser :: Parser (Maybe ParseResult)
parser = Just <$> updateParser <|> Nothing <$ noMatch

updateParser :: Parser ParseResult
updateParser = jsonMessage <|> planBuilds <|> planDownloads <|> copying <|> building <|> failed <|> checking

jsonMessage :: Parser ParseResult
jsonMessage =
  (string "@nix " *> noMatch) <|>> \raw_json ->
    let json_parse_result = eitherDecodeStrict' raw_json
        translate_aeson_error_to_nom_error :: String -> NOMError
        translate_aeson_error_to_nom_error aeson_error =
          ParseInternalJSONError (toText aeson_error) raw_json
     in JsonMessage (first translate_aeson_error_to_nom_error json_parse_result)

noMatch :: Parser ByteString
noMatch = ParseW8.takeTill isEndOfLine <* endOfLine

inTicks :: Parser a -> Parser a
inTicks x = tick *> x <* tick

tick :: Parser ()
tick = void $ char '\''

noTicks :: Parser ByteString
noTicks = takeTill (== '\'')

host :: Parser Host
host = Host . decodeUtf8 <$> inTicks noTicks

ellipsisEnd :: Parser ()
ellipsisEnd = string "..." >> endOfLine

indent :: Parser ()
indent = void $ string "  "

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
planDownloadLine = indent *> storePathParser <* endOfLine

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
      <$> (choice [string "error: ", pure ""] *> string "hash mismatch in fixed-output derivation " *> inTicks derivation <* string ":")
      <*> pure HashMismatch
      <* endOfLine

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
  p <- string "path " *> inTicks storePathParser
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
