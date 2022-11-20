module NOM.Update.Monad (
  UpdateMonad,
  MonadNow (..),
  MonadReadDerivation (..),
  MonadCheckStorePath (..),
  module NOM.Update.Monad.CacheBuildReports,
) where

import Control.Exception (try)
import Control.Monad.Writer.Strict (WriterT)
-- attoparsec
import Data.Attoparsec.Text (eitherResult, parse)
import Data.Text.IO qualified as TextIO
-- nix-derivation

import GHC.Clock qualified
import NOM.Builds (Derivation, StorePath)
import NOM.Error (NOMError (..))
import NOM.Update.Monad.CacheBuildReports
import Nix.Derivation qualified as Nix
import Relude
import System.Directory (doesPathExist)

type UpdateMonad m = (Monad m, MonadNow m, MonadReadDerivation m, MonadCacheBuildReports m, MonadCheckStorePath m)

class Monad m => MonadNow m where
  getNow :: m Double

instance MonadNow IO where
  getNow = GHC.Clock.getMonotonicTime

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
