module NOM.IO.Input (
  NOMInput (..),
  UpdateResult (..),
) where

import NOM.Error (NOMError)
import NOM.IO (Stream, StreamParser)
import NOM.State (NOMV1State)
import NOM.Update.Monad (UpdateMonad)
import Optics (Lens')
import Relude

data UpdateResult a = MkUpdateResult
  { errors :: [NOMError]
  , output :: ByteString
  , newStateToPrint :: Maybe NOMV1State
  , newState :: UpdaterState a
  }
  deriving stock (Generic)

class NOMInput a where
  type UpdaterState a
  firstState :: NOMV1State -> UpdaterState a
  updateState :: UpdateMonad m => a -> UpdaterState a -> m (UpdateResult a)
  nomState :: Lens' (UpdaterState a) NOMV1State
  inputStream :: Handle -> Stream (Either NOMError ByteString)
  withParser :: (StreamParser a -> IO t) -> IO t
