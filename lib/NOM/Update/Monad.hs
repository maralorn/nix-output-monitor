module NOM.Update.Monad (
  UpdateMonad,
  MonadNow (..),
  MonadReadDerivation (..),
  MonadCheckStorePath (..),
  CheckStorePathEnv (..),
  Update,
  module NOM.Update.Monad.CacheBuildReports,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, withAsync)
import Control.Concurrent.STM (TChan, retry, tryReadTChan, writeTChan)
import Control.Exception (bracket, try)
import Control.Monad.Trans.Writer.CPS (WriterT)
import Data.Attoparsec.Text (eitherResult, parse)
import Data.Text.IO qualified as TextIO
import Data.Time (UTCTime, getCurrentTime)
import GHC.Clock qualified
import NOM.Builds (Derivation, Host, HostContext (WithContext), StorePath, storePrefix)
import NOM.Error (NOMError (..))
import NOM.State (DerivationId)
import NOM.Update.Monad.CacheBuildReports
import Nix.Derivation qualified as Nix
import Relude
import System.Directory (doesPathExist)
import System.FSNotify (Event (..), WatchManager, watchDir)

type UpdateMonad m = (MonadNow m, MonadReadDerivation m, MonadCacheBuildReports m, MonadCheckStorePath m)

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

type Update = ReaderT CheckStorePathEnv IO

data CheckStorePathEnv = MkCheckStorePathEnv
  { watchManager :: WatchManager
  , pathFoundChannel :: TChan (Host WithContext, DerivationId)
  }

class (Monad m) => MonadCheckStorePath m where
  subscribeStorePath :: StorePath -> (Host WithContext, DerivationId) -> m ()
  foundStorePaths :: m [(Host WithContext, DerivationId)]

storePathExistsIO :: StorePath -> IO Bool
storePathExistsIO = doesPathExist . toString

flushChan :: TChan a -> STM [a]
flushChan chan = go
 where
  go =
    tryReadTChan chan >>= \case
      Nothing -> pure []
      Just next -> (next :) <$> go

instance MonadCheckStorePath Update where
  foundStorePaths = do
    check_env <- ask
    let chan = check_env.pathFoundChannel
    atomically $ flushChan chan

  subscribeStorePath path payload = do
    check_env <- ask
    let path_string = toString path
    found <- newTVarIO False
    let poll_path = do
          threadDelay 1_000_000
          storePathExistsIO path >>= \case
            True -> atomically $ writeTVar found True
            False -> poll_path

    void $ liftIO $ async $ bracket
      ( watchDir
          check_env.watchManager
          (toString storePrefix)
          ( \case
              Added{eventPath} -> eventPath == path_string
              _ -> False
          )
          (\_ -> atomically $ writeTVar found True)
      )
      id
      \_ -> do
        there <- storePathExistsIO path
        when there $ atomically $ writeTVar found True
        withAsync poll_path $ \_ -> atomically do
          found1 <- readTVar found
          unless found1 retry
          writeTChan (check_env.pathFoundChannel) payload

instance (MonadCheckStorePath m) => MonadCheckStorePath (StateT a m) where
  foundStorePaths = lift foundStorePaths
  subscribeStorePath path payload = lift $ subscribeStorePath path payload

instance (MonadCheckStorePath m) => MonadCheckStorePath (WriterT a m) where
  foundStorePaths = lift foundStorePaths
  subscribeStorePath path payload = lift $ subscribeStorePath path payload
