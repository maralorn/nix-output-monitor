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
import Streamly.Data.Stream qualified as Stream

readTextChunks :: Handle -> Stream (Either NOMError ByteString)
readTextChunks handle =
  Stream.repeatM (Exception.try (ByteString.hGetSome handle bufferSize))
    & fmap \case
      Left err -> Just (Left (InputError err)) -- Forward Exceptions, when we encounter them
      Right "" -> Nothing -- EOF
      Right input -> Just (Right input)
    & Stream.takeWhile isJust
    & Stream.catMaybes
 where
  bufferSize :: Int
  bufferSize = 4096 * 16

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
  withParser body = body (fmap (uncurry MkOldStyleInput . first join) <$> parseStreamAttoparsec parser)
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
