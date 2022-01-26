module NOM.Update.Monad (
  UpdateMonad,
  MonadNow (..),
  MonadReadDerivation (..),
  MonadCheckStorePath (..),
  module NOM.Update.Monad.CacheBuildReports,
) where

import Relude

import Control.Exception (IOException, try)
import qualified Data.Text.IO as TextIO
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (doesPathExist)

-- attoparsec
import Data.Attoparsec.Text (eitherResult, parse)

-- nix-derivation
import qualified Nix.Derivation as Nix

import NOM.Parser (Derivation, StorePath)
import NOM.Update.Monad.CacheBuildReports
import NOM.Util ((.>), (<.>>))

type UpdateMonad m = (Monad m, MonadNow m, MonadReadDerivation m, MonadCacheBuildReports m, MonadCheckStorePath m)

class Monad m => MonadNow m where
  getNow :: m UTCTime

instance MonadNow IO where
  getNow = getCurrentTime

class Monad m => MonadReadDerivation m where
  getDerivation :: Derivation -> m (Either Text (Nix.Derivation FilePath Text))

instance MonadReadDerivation IO where
  getDerivation =
    toString
      .> TextIO.readFile
      .> try @IOException
      <.>> ( first show
              >=> parse Nix.parseDerivation
              .> eitherResult
              .> first toText
           )

class Monad m => MonadCheckStorePath m where
  storePathExists :: StorePath -> m Bool

instance MonadCheckStorePath IO where
  storePathExists = doesPathExist . toString
