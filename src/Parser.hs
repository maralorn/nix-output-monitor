module Parser where

import Relude hiding (take, takeWhile)
import Prelude ()

import Data.Attoparsec.Text (
  Parser,
  char,
  choice,
  decimal,
  double,
  endOfLine,
  inClass,
  isEndOfLine,
  match,
  string,
  take,
  takeTill,
  takeWhile,
 )
import Data.Text (stripSuffix)

data ParseResult
  = Uploading StorePath Host
  | Downloading StorePath Host
  | PlanCopies Int
  | Build Derivation Host
  | NotRecognized
  | PlanBuilds (Set Derivation) Derivation
  | PlanDownloads Double Double (Set StorePath)
  | Checking Derivation
  | Failed Derivation Int
  deriving (Show, Eq, Read)

parser :: Parser (ParseResult, Text)
parser = swap <$> match updateParser

updateParser :: Parser ParseResult
updateParser = planBuilds <|> planDownloads <|> copying <|> building <|> failed <|> checking <|> noMatch

data StorePath = StorePath
  { hash :: Text
  , name :: Text
  }
  deriving stock (Show, Ord, Eq, Read)

newtype Derivation = Derivation {toStorePath :: StorePath}
  deriving stock (Show, Ord, Eq, Read)

instance ToText Derivation where
  toText = (<> ".drv") . toText . toStorePath
instance ToString Derivation where
  toString = toString . toText

storePrefix :: Text
storePrefix = "/nix/store/"

instance ToText StorePath where
  toText (StorePath hash name) = storePrefix <> hash <> "-" <> name
instance ToString StorePath where
  toString = toString . toText

data Host = Localhost | Host Text
  deriving stock (Ord, Eq)
  deriving stock (Show, Read)
instance ToText Host where
  toText (Host name) = name
  toText Localhost = "localhost"
instance ToString Host where
  toString = toString . toText

noMatch :: Parser ParseResult
noMatch = NotRecognized <$ takeTill isEndOfLine <* endOfLine

storePath :: Parser StorePath
storePath =
  StorePath
    <$> (string storePrefix *> take 32)
    <*> (char '-' *> takeWhile (inClass "a-zA-Z0-9?=_.+-"))

derivation :: Parser Derivation
derivation =
  storePath >>= \x -> case stripSuffix ".drv" (name x) of
    Just realName -> pure . Derivation $ x{name = realName}
    Nothing -> mzero

inTicks :: Parser a -> Parser a
inTicks x = tick *> x <* tick

tick :: Parser ()
tick = () <$ char '\''

noTicks :: Parser Text
noTicks = takeTill (== '\'')

host :: Parser Host
host = Host <$> inTicks noTicks

ellipsisEnd :: Parser ()
ellipsisEnd = string "..." >> endOfLine

indent :: Parser ()
indent = () <$ string "  "

-- these (<decimal> )?derivations will be built:
--  /nix/store/4lj96sc0pyf76p4w6irh52wmgikx8qw2-nix-output-monitor-0.1.0.3.drv
planBuilds :: Parser ParseResult
planBuilds =
  maybe mzero (\x -> pure (PlanBuilds (fromList (toList x)) (last x))) . nonEmpty
    =<< ( choice
            [ string "these derivations will be built:"
            , string "this derivation will be built:"
            , string "these " *> (decimal :: _ Int) *> string " derivations will be built:"
            ]
            *> endOfLine
            *> many planBuildLine
        )

planBuildLine :: Parser Derivation
planBuildLine = indent *> derivation <* endOfLine

planDownloads :: Parser ParseResult
planDownloads =
  PlanDownloads
    <$> ( choice
            [ string "these paths"
            , string "this path"
            , string "these " *> (decimal :: _ Int) *> string " paths"
            ]
            *> string " will be fetched ("
            *> double
        )
    <*> (string " MiB download, " *> double)
    <*> (string " MiB unpacked):" *> endOfLine *> (fromList <$> many planDownloadLine))

planDownloadLine :: Parser StorePath
planDownloadLine = indent *> storePath <* endOfLine

-- builder for '/nix/store/fbpdwqrfwr18nn504kb5jqx7s06l1mar-regex-base-0.94.0.1.drv' failed with exit code 1
failed :: Parser ParseResult
failed = Failed <$> (string "builder for " *> inTicks derivation <* string " failed with exit code ") <*> (decimal <* endOfLine)

-- checking outputs of '/nix/store/xxqgv6kwf6yz35jslsar0kx4f03qzyis-nix-output-monitor-0.1.0.3.drv'...
checking :: Parser ParseResult
checking = Checking <$> (string "checking outputs of " *> inTicks derivation <* ellipsisEnd)

-- copying 1 paths...
-- copying path '/nix/store/fzyahnw94msbl4ic5vwlnyakslq4x1qm-source' to 'ssh://maralorn@example.org'...
copying :: Parser ParseResult
copying =
  string "copying "
    *> (transmission <|> (PlanCopies <$> decimal <* string " paths" <* ellipsisEnd))

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
