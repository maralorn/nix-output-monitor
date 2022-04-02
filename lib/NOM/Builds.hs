module NOM.Builds (Derivation (..), StorePath (..), Host (..), FailType (..), storePrefix) where

import Relude

data StorePath = StorePath
  { hash :: !Text
  , name :: !Text
  }
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (NFData)

newtype Derivation = Derivation {storePath :: StorePath}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData)

{- ORMOLU_DISABLE -}
instance ToText Derivation where
  toText drv = toText drv.storePath <> ".drv"
{- ORMOLU_ENABLE -}

instance ToString Derivation where
  toString = toString . toText

storePrefix :: Text
storePrefix = "/nix/store/"

instance ToText StorePath where
  toText (StorePath hash name) = storePrefix <> hash <> "-" <> name

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

data FailType = ExitCode !Int | HashMismatch
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
