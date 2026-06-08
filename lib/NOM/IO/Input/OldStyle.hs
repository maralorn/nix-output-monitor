module NOM.IO.Input.OldStyle (OldStyleInput) where

import Control.Exception qualified as Exception
import Data.ByteString qualified as ByteString
import NOM.Error (NOMError (..))
import NOM.IO (Stream)
import NOM.IO.Input (NOMInput (..), UpdateResult (..), statelessUnfoldM)
import NOM.NixMessage.OldStyle (NixOldStyleMessage)
import NOM.Parser (parser)
import NOM.StreamParser (parseStreamAttoparsec)
import NOM.Update (updateStateNixOldStyleMessage)
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

data OldStyleInput = MkOldStyleInput
  { parseResult :: Maybe NixOldStyleMessage
  , parsedInput :: ByteString
  }

instance NOMInput OldStyleInput where
  withParser body = body (fmap (uncurry MkOldStyleInput . first join) <$> parseStreamAttoparsec parser)
  inputStreamImpl = readTextChunks
  {-# INLINE updateState #-}
  updateState input old_state = mkUpdateResult <$> updateStateNixOldStyleMessage (input.parseResult, input.parsedInput) old_state
   where
    mkUpdateResult ((errors, output), new_state) =
      MkUpdateResult
        { errors
        , output
        , newStateToPrint = new_state
        , newState = fromMaybe old_state new_state
        }
