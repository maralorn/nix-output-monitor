module NOM.Update.Monad (
  UpdateMonad,
  MonadNow (..),
  MonadReadDerivation (..),
  MonadCheckStorePath (..),
  module NOM.Update.Monad.CacheBuildReports,
) where

import Relude

import Control.Exception (try)
import Control.Monad.Writer.Strict (WriterT)
import Data.Text.IO qualified as TextIO
import System.Directory (doesPathExist)

-- attoparsec
import Data.Attoparsec.Text (eitherResult, parse)

-- nix-derivation
import Nix.Derivation qualified as Nix

import NOM.Builds (Derivation, StorePath)
import NOM.Error (NOMError (..))
import NOM.Update.Monad.CacheBuildReports
import Streamly.Internal.Data.Time.Units (AbsTime)
import Streamly.Internal.Data.Time.Clock (getTime)
import Streamly.Internal.Data.Time.Clock.Type (Clock(Monotonic))

type UpdateMonad m = (Monad m, MonadNow m, MonadReadDerivation m, MonadCacheBuildReports m, MonadCheckStorePath m)

class Monad m => MonadNow m where
  getNow :: m AbsTime

instance MonadNow IO where
  getNow = getTime Monotonic

instance MonadNow m => MonadNow (StateT a m) where
  getNow = lift getNow
instance (Monoid a, MonadNow m) => MonadNow (WriterT a m) where
  getNow = lift getNow

class Monad m => MonadReadDerivation m where
  getDerivation :: Derivation -> m (Either NOMError (Nix.Derivation FilePath Text))

instance MonadReadDerivation IO where
  getDerivation =
    fmap
      ( first DerivationReadError
          >=> first (DerivationParseError . toText) . eitherResult . parse Nix.parseDerivation
      )
      . try
      . TextIO.readFile
      . toString

instance MonadReadDerivation m => MonadReadDerivation (StateT a m) where
  getDerivation = lift . getDerivation
instance MonadReadDerivation m => MonadReadDerivation (ExceptT a m) where
  getDerivation = lift . getDerivation
instance (Monoid a, MonadReadDerivation m) => MonadReadDerivation (WriterT a m) where
  getDerivation = lift . getDerivation

class Monad m => MonadCheckStorePath m where
  storePathExists :: StorePath -> m Bool

instance MonadCheckStorePath IO where
  storePathExists = doesPathExist . toString
instance MonadCheckStorePath m => MonadCheckStorePath (StateT a m) where
  storePathExists = lift . storePathExists
instance (Monoid a, MonadCheckStorePath m) => MonadCheckStorePath (WriterT a m) where
  storePathExists = lift . storePathExists
