module NOM.Update.Monad (
  UpdateMonad,
  MonadNow (..),
  MonadReadDerivation (..),
  MonadCheckStorePath (..),
  CheckStorePathEnv (..),
  Update,
  module NOM.Update.Monad.CacheBuildReports,
) where

import Control.Concurrent.Async (async, link)
import Control.Concurrent.STM (TChan, retry, tryReadTChan, writeTChan)
import Control.Exception (mask, try)
import Control.Monad.Trans.Writer.CPS (WriterT)
import Data.Attoparsec.Text (eitherResult, parse)
import Data.Set qualified as Set
import Data.Text.IO qualified as TextIO
import Data.Time (UTCTime, getCurrentTime)
import GHC.Clock qualified
import NOM.Builds (Derivation, Host, HostContext (WithContext), StorePath)
import NOM.Error (NOMError (..))
import NOM.State (DerivationId)
import NOM.Update.Monad.CacheBuildReports
import Nix.Derivation qualified as Nix
import Relude
import System.Directory (doesPathExist)

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
  { isWatchActive :: TVar Bool
  -- ^ Needed to prevent blocking indefinitely on STM when the watch is stopped
  , subscribedStorePaths :: TVar (Set StorePath)
  {- ^ As long as isWatchActive is True, a path will be deleted from the set
  when it is added to the nix store.
  -}
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
    liftIO $ atomically $ modifyTVar' check_env.subscribedStorePaths (Set.insert path)
    b1 <- liftIO $ storePathExistsIO path
    if b1
      then liftIO $ do
        atomically $ modifyTVar' check_env.subscribedStorePaths (Set.delete path)
        atomically $ writeTChan check_env.pathFoundChannel payload
      else liftIO $ mask \restore -> do
        a <- async $ restore do
          -- Terminates either when the path is no longer in
          -- subscribedStorePaths, or the watch is inactive.
          atomically $ do
            watchActive <- readTVar check_env.isWatchActive
            when watchActive do
              paths <- readTVar check_env.subscribedStorePaths
              when (Set.member path paths) retry
          b2 <- storePathExistsIO path
          when b2
            $ atomically
            $ writeTChan check_env.pathFoundChannel payload
        link a

instance (MonadCheckStorePath m) => MonadCheckStorePath (StateT a m) where
  foundStorePaths = lift foundStorePaths
  subscribeStorePath path payload = lift $ subscribeStorePath path payload

instance (MonadCheckStorePath m) => MonadCheckStorePath (WriterT a m) where
  foundStorePaths = lift foundStorePaths
  subscribeStorePath path payload = lift $ subscribeStorePath path payload
