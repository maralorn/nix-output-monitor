module NOM.Parser (parser, oldStyleParser, planBuildLine, planDownloadLine, inTicks) where

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
  skipSpace,
  takeTill,
  try,
 )
import NOM.Builds (Derivation (..), FailType (ExitCode, HashMismatch), Host (..), HostContext (..), StorePath (..), derivationByteStringParser, parseHost, storePathByteStringParser)
import NOM.NixMessage.OldStyle (NixOldStyleMessage (..))
import Relude hiding (take, takeWhile)

parser :: Parser (Maybe NixOldStyleMessage)
parser = Just <$> oldStyleParser <|> Nothing <$ noMatch

oldStyleParser :: Parser NixOldStyleMessage
oldStyleParser = planBuilds <|> planDownloads <|> copying <|> building <|> failed <|> checking

noMatch :: Parser ByteString
noMatch = ParseW8.takeTill isEndOfLine <* endOfLine

inTicks :: Parser a -> Parser a
inTicks x = tick *> x <* tick

tick :: Parser ()
tick = void $ char '\''

noTicks :: Parser ByteString
noTicks = takeTill (== '\'')

host :: Parser (Host WithContext)
host = parseHost . decodeUtf8 <$> inTicks noTicks

ellipsisEnd :: Parser ()
ellipsisEnd = string "..." >> endOfLine

indent :: Parser ()
indent = void $ string "  "

-- these (<decimal> )?derivations will be built:
--  /nix/store/4lj96sc0pyf76p4w6irh52wmgikx8qw2-nix-output-monitor-0.1.0.3.drv
planBuilds :: Parser NixOldStyleMessage
planBuilds =
  maybe mzero (\x -> pure (PlanBuilds (fromList (toList x)) (last x)))
    . nonEmpty
    =<< choice
      [ string "these derivations will be built:"
      , string "this derivation will be built:"
      , string "these " *> (decimal :: Parser Int) *> string " derivations will be built:"
      ]
    *> endOfLine
    *> many planBuildLine

planBuildLine :: Parser Derivation
planBuildLine = indent *> derivationByteStringParser <* endOfLine

planDownloads :: Parser NixOldStyleMessage
planDownloads =
  PlanDownloads
    <$> ( choice
            [ string "these paths"
            , string "this path"
            , string "these " *> (decimal :: Parser Int) *> string " paths"
            ]
            *> string " will be fetched ("
            *> byteSize
        )
    <*> (" download, " *> byteSize)
    <*> (" unpacked):" *> endOfLine *> (fromList <$> many planDownloadLine))

byteSize :: Parser Double
byteSize = do
  num <- double
  skipSpace
  unit <- anyChar
  _ <- try (string "iB")
  power <- case unit of
    'K' -> pure 1
    'M' -> pure 2
    'G' -> pure 3
    'T' -> pure 4
    'P' -> pure 5
    'E' -> pure 6
    'Z' -> pure 7
    'Y' -> pure 8
    x -> fail $ "Unknown unit: " <> [x] <> "iB"
  pure $ num * (1024 ** power)

planDownloadLine :: Parser StorePath
planDownloadLine = indent *> storePathByteStringParser <* endOfLine

failed :: Parser NixOldStyleMessage
-- builder for '/nix/store/fbpdwqrfwr18nn504kb5jqx7s06l1mar-regex-base-0.94.0.1.drv' failed with exit code 1
failed =
  Failed
    <$> ( choice
            [ string "error: build of " <* inTicks derivationByteStringParser <* manyTill' anyChar (string "failed: error: ")
            , string "error: "
            , pure ""
            ]
            *> string "builder for "
            *> inTicks derivationByteStringParser
            <* string " failed with exit code "
        )
    <*> (ExitCode <$> decimal <* choice [endOfLine, char ';' *> endOfLine])
    <|>
    -- error: hash mismatch in fixed-output derivation '/nix/store/nrx4swgzs3iy049fqfx51vhnbb9kzkyv-source.drv':
    Failed
    <$> (choice [string "error: ", pure ""] *> string "hash mismatch in fixed-output derivation " *> inTicks derivationByteStringParser <* string ":")
    <*> pure HashMismatch
    <* endOfLine

-- checking outputs of '/nix/store/xxqgv6kwf6yz35jslsar0kx4f03qzyis-nix-output-monitor-0.1.0.3.drv'...
checking :: Parser NixOldStyleMessage
checking = Checking <$> (string "checking outputs of " *> inTicks derivationByteStringParser <* ellipsisEnd)

-- copying 1 paths...
-- copying path '/nix/store/fzyahnw94msbl4ic5vwlnyakslq4x1qm-source' to 'ssh://maralorn@example.org'...
copying :: Parser NixOldStyleMessage
copying =
  string "copying "
    *> (transmission <|> PlanCopies <$> decimal <* string " paths" <* ellipsisEnd)

transmission :: Parser NixOldStyleMessage
transmission = do
  p <- string "path " *> inTicks storePathByteStringParser
  (Uploading p <$> toHost <|> Downloading p <$> fromHost) <* ellipsisEnd

fromHost :: Parser (Host WithContext)
fromHost = string " from " *> host

toHost :: Parser (Host WithContext)
toHost = string " to " *> host

onHost :: Parser (Host WithContext)
onHost = string " on " *> host

-- building '/nix/store/4lj96sc0pyf76p4w6irh52wmgikx8qw2-nix-output-monitor-0.1.0.3.drv' on 'ssh://maralorn@example.org'...
building :: Parser NixOldStyleMessage
building = do
  p <- string "building " *> inTicks derivationByteStringParser
  Build p Localhost <$ ellipsisEnd <|> Build p <$> onHost <* ellipsisEnd
