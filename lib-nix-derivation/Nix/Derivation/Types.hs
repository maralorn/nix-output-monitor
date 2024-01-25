{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Shared types
module Nix.Derivation.Types (
  -- * Types
  Derivation (..),
  DerivationInputs (..),
  DerivationOutput (..),
) where

import Control.DeepSeq (NFData)
import Data.Eq (Eq)
import Data.Map (Map)
import Data.Ord (Ord)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Text.Show (Show)

-- | A Nix derivation
data Derivation fp txt outputName drvOutput drvInputs = Derivation
  { outputs :: Map outputName (drvOutput fp)
  -- ^ Outputs produced by this derivation where keys are output names
  , inputs :: drvInputs fp outputName
  -- ^ Inputs (sources and derivations)
  , platform :: txt
  -- ^ Platform required for this derivation
  , builder :: txt
  -- ^ Code to build the derivation, which can be a path or a builtin function
  , args :: Vector txt
  -- ^ Arguments passed to the executable used to build to derivation
  , env :: Map txt txt
  -- ^ Environment variables provided to the executable used to build the
  -- derivation
  }
  deriving stock (Eq, Generic, Ord, Show)

instance
  ( NFData fp
  , NFData txt
  , NFData outputName
  , NFData (drvOutput fp)
  , NFData (drvInputs fp outputName)
  ) =>
  NFData (Derivation fp txt outputName drvOutput drvInputs)

data DerivationInputs fp drvOutput = DerivationInputs
  { drvs :: Map fp (Set drvOutput)
  -- ^ Inputs that are derivations where keys specify derivation paths and
  -- values specify which output names are used by this derivation
  , srcs :: Set fp
  -- ^ Inputs that are sources
  }
  deriving stock (Eq, Generic, Ord, Show)

instance (NFData a, NFData b) => NFData (DerivationInputs a b)

-- | An output of a Nix derivation
data DerivationOutput fp
  = DerivationOutput
      { path :: fp
      -- ^ Path where the output will be saved
      }
  | FixedDerivationOutput
      { path :: fp
      -- ^ Path where the output will be saved
      , hashAlgo :: Text
      -- ^ Hash used for expected hash computation
      , hash :: Text
      -- ^ Expected hash
      }
  | ContentAddressedDerivationOutput
      { hashAlgo :: Text
      -- ^ Hash used for expected hash computation
      }
  deriving stock (Eq, Generic, Ord, Show)

instance (NFData a) => NFData (DerivationOutput a)
