module NOM.Update.Monad (
  UpdateMonad,
  MonadNow (..),
  MonadReadDerivation (..),
  MonadCheckStorePath (..),
  Update,
  module NOM.Update.Monad.CacheBuildReports,
) where

import Control.Concurrent.STM (retry)
import Control.Exception (bracket, try)
import Control.Monad.Trans.Writer.CPS (WriterT)
import Data.Attoparsec.Text (eitherResult, parse)
import Data.Text.IO qualified as TextIO
import Data.Time (UTCTime, getCurrentTime)
import GHC.Clock qualified
import NOM.Builds (Derivation, StorePath)
import NOM.Error (NOMError (..))
import NOM.Update.Monad.CacheBuildReports
import Nix.Derivation qualified as Nix
import Relude
import System.Directory (doesPathExist)
import System.FilePath (takeDirectory)
import System.INotify

-- the MonadIO kind of defeats the point here, but we need it to partially "unlift" back into IO
type UpdateMonad m = (Monad m, MonadIO m, MonadNow m, MonadReadDerivation m, MonadCacheBuildReports m, MonadCheckStorePath m)

class (Monad m) => MonadNow m where
  getNow :: m Double
  getUTC :: m UTCTime

instance MonadNow IO where
  getNow = GHC.Clock.getMonotonicTime
  getUTC = getCurrentTime

instance (MonadNow m) => MonadNow (StateT a m) where
  getNow = lift getNow
  getUTC = lift getUTC

instance (MonadNow m) => MonadNow (WriterT a m) where
  getNow = lift getNow
  getUTC = lift getUTC

instance (MonadNow m) => MonadNow (ReaderT a m) where
  getNow = lift getNow
  getUTC = lift getUTC

class (Monad m) => MonadReadDerivation m where
  getDerivation :: Derivation -> m (Either NOMError (Nix.Derivation FilePath Text))

instance MonadReadDerivation IO where
  getDerivation =
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

instance (MonadReadDerivation m) => MonadReadDerivation (ReaderT a m) where
  getDerivation = lift . getDerivation

type Update = ReaderT INotify IO

class (Monad m) => MonadCheckStorePath m where
  storePathExists :: StorePath -> m Bool

  -- | WILL block indefinitely if the store path is not going to exist. Use with a timeout.
  waitForStorePath :: StorePath -> m ()

  -- | Implementation detail. Added only because of the monad transformer stack being annoying
  theInotify :: m INotify

storePathExistsIO :: StorePath -> IO Bool
storePathExistsIO = doesPathExist . toString

instance MonadCheckStorePath Update where
  theInotify = ask
  storePathExists = liftIO . storePathExistsIO
  waitForStorePath path = do
    inotify <- ask
    let rawStorePath = encodeUtf8 (toString path)
        rawStore = encodeUtf8 . takeDirectory $ toString path
    found <- newTVarIO False

    liftIO $ bracket
      ( addWatch inotify [AllEvents] rawStore \case
          MovedIn{filePath}
            | rawStore <> "/" <> filePath == rawStorePath ->
                atomically $ writeTVar found True
          _ -> pure ()
      )
      removeWatch
      \_ -> do
        there <- storePathExistsIO path
        when there $ atomically $ writeTVar found True
        atomically do
          found1 <- readTVar found
          unless found1 retry

instance (MonadCheckStorePath m) => MonadCheckStorePath (StateT a m) where
  storePathExists = lift . storePathExists
  waitForStorePath = lift . waitForStorePath
  theInotify = lift theInotify

instance (MonadCheckStorePath m) => MonadCheckStorePath (WriterT a m) where
  storePathExists = lift . storePathExists
  waitForStorePath = lift . waitForStorePath
  theInotify = lift theInotify
