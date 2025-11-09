module NOM.IO.Input (
  NOMInput (..),
  UpdateResult (..),
  statelessUnfoldM,
) where

import NOM.Error (NOMError)
import NOM.IO (Stream, StreamParser)
import NOM.State (NOMV1State)
import NOM.Update.Monad (UpdateMonad)
import Optics (Lens')
import Relude
import Streamly.Data.Stream qualified as Stream

statelessUnfoldM :: (Monad m) => m (Maybe a) -> Stream.Stream m a
statelessUnfoldM generator =
  Stream.repeatM generator
    & Stream.takeWhile isJust
    & Stream.catMaybes

data UpdateResult a = MkUpdateResult
  { errors :: [NOMError]
  , output :: ByteString
  , newStateToPrint :: Maybe NOMV1State
  , newState :: UpdaterState a
  }

class NOMInput a where
  type UpdaterState a
  firstState :: NOMV1State -> UpdaterState a
  updateState :: (UpdateMonad m) => a -> UpdaterState a -> m (UpdateResult a)
  nomState :: Lens' (UpdaterState a) NOMV1State
  inputStream :: Handle -> Stream (Either NOMError ByteString)
  withParser :: (StreamParser a -> IO t) -> IO t
