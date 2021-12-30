module NOM.Update.Monad (
  UpdateMonad,
  MonadNow (..),
  MonadReadDerivation (..),
  MonadCheckStorePath (..),
  module NOM.Update.Monad.CacheBuildReports,
) where

import Relude

import Control.Exception (IOException, try)
import Data.Attoparsec.Text (eitherResult, parse)
import qualified Data.Text.IO as TextIO
import Data.Time (UTCTime, getCurrentTime)
import qualified Nix.Derivation as Nix
import System.Directory (doesPathExist)

import NOM.Parser (Derivation, StorePath)
import NOM.Update.Monad.CacheBuildReports

type UpdateMonad m = (Monad m, MonadNow m, MonadReadDerivation m, MonadCacheBuildReports m, MonadCheckStorePath m)

class Monad m => MonadNow m where
  getNow :: m UTCTime

instance MonadNow IO where
  getNow = getCurrentTime

class Monad m => MonadReadDerivation m where
  getDerivation :: Derivation -> m (Either Text (Nix.Derivation FilePath Text))

instance MonadReadDerivation IO where
  getDerivation drvName =
    try @IOException (TextIO.readFile (toString drvName))
      <&> ( first show
              >=> first toText . eitherResult . parse Nix.parseDerivation
          )

class Monad m => MonadCheckStorePath m where
  storePathExists :: StorePath -> m Bool

instance MonadCheckStorePath IO where
  storePathExists = doesPathExist . toString
