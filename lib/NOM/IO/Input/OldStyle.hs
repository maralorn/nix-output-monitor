module NOM.IO.Input.OldStyle (OldStyleInput) where

import Control.Exception qualified as Exception
import Data.ByteString qualified as ByteString
import Data.Strict qualified as Strict
import NOM.Error (NOMError (..))
import NOM.IO (Stream)
import NOM.IO.Input (NOMInput (..), UpdateResult (..), statelessUnfoldM)
import NOM.NixMessage.OldStyle (NixOldStyleMessage)
import NOM.Parser (parser)
import NOM.State (NOMState)
import NOM.StreamParser (parseStreamAttoparsec)
import NOM.Update (updateStateNixOldStyleMessage)
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude

readTextChunks :: Handle -> Stream (Either NOMError ByteString)
readTextChunks handle =
  statelessUnfoldM
    $ Exception.try (ByteString.hGetSome handle bufferSize)
    <&> \case
      Left err -> Just (Left (InputError err)) -- Forward Exceptions, when we encounter them
      Right "" -> Nothing -- EOF
      Right input -> Just (Right input)
 where
  bufferSize :: Int
  bufferSize = 4096 * 16

data OldStyleState = MkOldStyleState
  { state :: NOMState
  , -- Because old style human nix logs don't include information for when a
    -- build finishes we monitor the existence of the output paths.
    --  This variable saves when we last polled the disc for
    -- output paths of currently running builds.
    lastRead :: Strict.Maybe Double
  }

makeFieldLabelsNoPrefix ''OldStyleState

data OldStyleInput = MkOldStyleInput
  { parseResult :: Maybe NixOldStyleMessage
  , parsedInput :: ByteString
  }

instance NOMInput OldStyleInput where
  withParser body = body (fmap (uncurry MkOldStyleInput . first join) <$> parseStreamAttoparsec parser)
  type UpdaterState OldStyleInput = OldStyleState
  inputStreamImpl = readTextChunks
  nomState = #state
  firstState state' = MkOldStyleState{state = state', lastRead = Strict.Nothing}
  {-# INLINE updateState #-}
  updateState input old_state = mkUpdateResult <$> updateStateNixOldStyleMessage (input.parseResult, input.parsedInput) (Strict.toLazy old_state.lastRead, old_state.state)
   where
    mkUpdateResult ((errors, output), (new_timestamp, new_state)) =
      MkUpdateResult
        { errors
        , output
        , newStateToPrint = new_state
        , newState = MkOldStyleState (fromMaybe old_state.state new_state) (Strict.toStrict new_timestamp)
        }
