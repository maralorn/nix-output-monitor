{-# OPTIONS_GHC -Wno-orphans #-}

module NOM.IO.Input.JSON () where

import Control.Exception (throwIO)
import Control.Exception qualified as Exception
import Data.ByteString qualified as ByteString
import Data.Hermes qualified as JSON
import Data.Time (getCurrentTime)
import NOM.Error (NOMError (..))
import NOM.IO (Stream)
import NOM.IO.Input (NOMInput (..), UpdateResult (..), statelessUnfoldM)
import NOM.NixMessage.JSON (NixJSONMessage)
import NOM.Parser.JSON (parseJSONLine)
import NOM.State (NOMV1State)
import NOM.Update (updateStateNixJSONMessage)
import Optics qualified
import Relude
import System.IO.Error qualified as IOError

readLines :: Handle -> Stream (Either NOMError ByteString)
readLines handle =
  statelessUnfoldM
    $ Exception.try (ByteString.hGetLine handle)
    <&> \case
      Left err | IOError.isEOFError err -> Nothing
      Left err -> Just (Left (InputError err)) -- Forward Exceptions, when we encounter them
      Right input -> Just (Right input)

instance NOMInput NixJSONMessage where
  withParser body = JSON.withHermesEnv_ (body . parseJSONLine)
  type UpdaterState NixJSONMessage = NOMV1State

  -- inputStream = readLines
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
