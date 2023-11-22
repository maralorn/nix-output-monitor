{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NOM.Update.Monad (
  UpdateMonad,
  MonadNow (..),
  MonadReadDerivation (..),
  MonadCheckStorePath (..),
  module NOM.Update.Monad.CacheBuildReports,
) where

import Control.Exception (try)
import Control.Monad.Trans.Writer.CPS (WriterT)
import Data.Attoparsec.Text (eitherResult, parse)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import GHC.Clock qualified
import NOM.Builds (Derivation, StorePath, parseDerivation)
import NOM.Error (NOMError (..))
import NOM.Update.Monad.CacheBuildReports
import Nix.Derivation qualified as Nix
import Relude
import System.Directory (doesPathExist)
import System.Process.Typed qualified as Process

type UpdateMonad m = (Monad m, MonadNow m, MonadReadDerivation m, MonadCacheBuildReports m, MonadCheckStorePath m)

class (Monad m) => MonadNow m where
  getNow :: m Double

instance MonadNow IO where
  getNow = GHC.Clock.getMonotonicTime

instance (MonadNow m) => MonadNow (StateT a m) where
  getNow = lift getNow

instance (MonadNow m) => MonadNow (WriterT a m) where
  getNow = lift getNow

class (Monad m) => MonadReadDerivation m where
  getDerivation :: Derivation -> m (Either NOMError (Nix.Derivation FilePath Text))

instance MonadReadDerivation IO where
  getDerivation = do
    fmap
      ( first DerivationReadError
          >=> first (DerivationParseError . toText)
          . eitherResult
          . parse Nix.parseDerivation
      )
      . try
      . TextIO.readFile
      . toString

instance (MonadReadDerivation m) => MonadReadDerivation (StateT a m) where
  getDerivation = lift . getDerivation

instance (MonadReadDerivation m) => MonadReadDerivation (ExceptT a m) where
  getDerivation = lift . getDerivation

instance (MonadReadDerivation m) => MonadReadDerivation (WriterT a m) where
  getDerivation = lift . getDerivation

class (Monad m) => MonadCheckStorePath m where
  storePathExists :: StorePath -> m Bool
  getProducers :: [StorePath] -> m [Derivation]

instance MonadCheckStorePath IO where
  storePathExists = doesPathExist . toString
  getProducers = \case
    [] -> pure mempty
    paths -> do
      (exit_code, producer_bs) <-
        Process.readProcessStdout
          $ Process.setStdin Process.nullStream
          $ Process.setStderr Process.nullStream
          $ Process.proc "nix" (["path-info", "--derivation"] <> fmap toString paths)
      pure
        if Process.ExitSuccess == exit_code
          then
            producer_bs
              & decodeUtf8
              & Text.strip
              & Text.lines
              & mapMaybe parseDerivation
          else mempty

instance (MonadCheckStorePath m) => MonadCheckStorePath (StateT a m) where
  storePathExists = lift . storePathExists
  getProducers = lift . getProducers

instance (MonadCheckStorePath m) => MonadCheckStorePath (WriterT a m) where
  storePathExists = lift . storePathExists
  getProducers = lift . getProducers

instance (MonadState s m) => MonadState s (WriterT w m) where
  get = lift get
  put = lift . put
  state = lift . state
