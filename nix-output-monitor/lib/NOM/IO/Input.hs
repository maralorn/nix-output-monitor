module NOM.IO.Input (
  NOMInput (..),
  inputStream,
  UpdateResult (..),
  statelessUnfoldM,
) where

import NOM.Error (NOMError)
import NOM.IO (Stream, StreamParser)
import NOM.State (NOMState)
import NOM.Update.Monad (UpdateMonad)
import Relude
import Streamly.Data.Stream qualified as Stream

statelessUnfoldM :: (Monad m) => m (Maybe a) -> Stream.Stream m a
statelessUnfoldM generator =
  Stream.repeatM generator
    & Stream.takeWhile isJust
    & Stream.catMaybes

data UpdateResult = MkUpdateResult
  { errors :: [NOMError]
  , output :: ByteString
  , newStateToPrint :: Maybe NOMState
  , newState :: NOMState
  }

class NOMInput a where
  updateState :: (UpdateMonad m) => a -> NOMState -> m UpdateResult
  inputStreamImpl :: Handle -> Stream (Either NOMError ByteString)
  withParser :: (StreamParser a -> IO t) -> IO t

inputStream :: forall a -> (NOMInput a) => Handle -> Stream (Either NOMError ByteString)
inputStream a = inputStreamImpl @a
