{-# OPTIONS_GHC -Wno-orphans #-}

module NOM.IO.Input.JSON (parseStreamSimple) where

import Control.Exception qualified as Exception
import Data.ByteString qualified as ByteString
import Data.Hermes qualified as JSON
import NOM.Error (NOMError (..))
import NOM.IO (Stream)
import NOM.IO.Input (NOMInput (..), UpdateResult (..))
import NOM.NixMessage.JSON (NixJSONMessage)
import NOM.Parser.JSON (parseJSONLine)
import NOM.State (NOMV1State)
import NOM.Update (updateStateNixJSONMessage)
import Optics qualified
import Relude
import Streamly.Prelude ((.:))
import Streamly.Prelude qualified as Stream
import System.IO.Error qualified as IOError

readLines :: Handle -> Stream (Either NOMError ByteString)
readLines handle = loop
 where
  tryRead :: Stream (Either Exception.IOException ByteString)
  tryRead = liftIO $ Exception.try $ ByteString.hGetLine handle
  loop :: Stream (Either NOMError ByteString)
  loop =
    tryRead >>= \case
      Left err | IOError.isEOFError err -> mempty
      Left err -> Left (InputError err) .: loop -- Forward Exceptions, when we encounter them
      Right input -> Right input .: loop

parseStreamSimple :: Stream.MonadAsync m => (ByteString -> update) -> Stream.SerialT m ByteString -> Stream.SerialT m update
parseStreamSimple parser = Stream.fromAhead . Stream.map parser . Stream.fromSerial

instance NOMInput NixJSONMessage where
  withParser body = JSON.withHermesEnv_ (body . parseStreamSimple . parseJSONLine)
  type UpdaterState NixJSONMessage = NOMV1State
  inputStream = readLines
  nomState = Optics.equality'
  firstState = id
  {-# INLINE updateState #-}
  updateState input old_state = mkUpdateResult <$> updateStateNixJSONMessage input old_state
   where
    mkUpdateResult ((errors, output), new_state) =
      MkUpdateResult
        { errors
        , output
        , newStateToPrint = new_state
        , newState = fromMaybe old_state new_state
        }
