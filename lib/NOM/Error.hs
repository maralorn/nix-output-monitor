module NOM.Error (NOMError (..)) where

import Control.Exception (IOException)
import Relude

data NOMError
  = InputError IOException
  | DerivationReadError IOException
  | DerivationParseError Text
  | ParseNixJSONMessageError Text ByteString
  deriving stock (Show, Eq)
