module Nix.Derivation (
  -- * Types
  Derivation (..),
  DerivationInputs (..),
  DerivationOutput (..),

  -- * Parse derivations
  parseDerivation,
  parseDerivationWith,
  textParser,
) where

import Nix.Derivation.Parser (
  parseDerivation,
  parseDerivationWith,
  textParser,
 )
import Nix.Derivation.Types (
  Derivation (..),
  DerivationInputs (..),
  DerivationOutput (..),
 )
