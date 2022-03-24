module NOM.Builds where

import Relude

data StorePath = StorePath
  { hash :: !Text
  , name :: !Text
  }
  deriving stock (Show, Ord, Eq, Read, Generic)
  deriving anyclass (NFData)

newtype Derivation = Derivation {toStorePath :: StorePath}
  deriving stock (Show, Read, Generic)
  deriving newtype (Eq, Ord, NFData)

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

data Host = Localhost | Host !Text
  deriving stock (Ord, Eq, Show, Read, Generic)
  deriving anyclass (NFData)

instance ToText Host where
  toText (Host name) = name
  toText Localhost = "localhost"

instance ToString Host where
  toString = toString . toText

data FailType = ExitCode !Int | HashMismatch
  deriving (Show, Eq, Ord, Read, Generic)
  deriving anyclass (NFData)
