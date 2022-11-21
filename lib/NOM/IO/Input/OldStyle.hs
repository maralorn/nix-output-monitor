module NOM.IO.Input.OldStyle (OldStyleInput) where

import Control.Exception qualified as Exception
import Data.ByteString qualified as ByteString
import Data.Strict qualified as Strict
import NOM.Error (NOMError (..))
import NOM.IO (Stream)
import NOM.IO.Input (NOMInput (..), UpdateResult (..))
import NOM.NixMessage.OldStyle (NixOldStyleMessage)
import NOM.Parser (parser)
import NOM.State (NOMV1State)
import NOM.StreamParser (parseStreamAttoparsec)
import NOM.Update (updateStateNixOldStyleMessage)
import Optics (gfield)
import Relude
import Streamly.Prelude ((.:))

readTextChunks :: Handle -> Stream (Either NOMError ByteString)
readTextChunks handle = loop
 where
  -- We read up-to 4kb of input at once. We will rarely need more than that for one successful parse (i.e. a line).
  -- I don‘t know much about computers, but 4k seems like something which would be cached efficiently.
  bufferSize :: Int
  bufferSize = 4096
  tryRead :: Stream (Either Exception.IOException ByteString)
  tryRead = liftIO $ Exception.try $ ByteString.hGetSome handle bufferSize
  loop :: Stream (Either NOMError ByteString)
  loop =
    tryRead >>= \case
      Left err -> Left (InputError err) .: loop -- Forward Exceptions, when we encounter them
      Right "" -> mempty -- EOF
      Right input -> Right input .: loop

data OldStyleState = MkOldStyleState
  { state :: NOMV1State
  , lastRead :: Strict.Maybe Double
  }
  deriving stock (Generic)

data OldStyleInput = MkOldStyleInput
  { parseResult :: Maybe NixOldStyleMessage
  , parsedInput :: ByteString
  }

instance NOMInput OldStyleInput where
  withParser body = body (fmap (uncurry MkOldStyleInput) <$> parseStreamAttoparsec parser)
  type UpdaterState OldStyleInput = OldStyleState
  inputStream = readTextChunks
  nomState = gfield @"state"
  firstState state' = MkOldStyleState{state = state', lastRead = Strict.Nothing}
  {-# INLINE updateState #-}
  updateState input old_state = mkUpdateResult <$> updateStateNixOldStyleMessage (input.parseResult, input.parsedInput) (Strict.toLazy old_state.lastRead, old_state.state)
   where
    mkUpdateResult ((errors, output), (new_timestamp, new_state)) =
      MkUpdateResult
        { errors
        , output
        , newStateToPrint = new_state
        , newState = MkOldStyleState (fromMaybe (old_state.state) new_state) (Strict.toStrict new_timestamp)
        }
