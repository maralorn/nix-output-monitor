module NOM.Error (NOMError (..)) where

import Relude

import Control.Exception (IOException)

data NOMError = 
     InputError IOException 
   | DerivationReadError IOException 
   | DerivationParseError Text 
   | ParseInternalJSONError Text ByteString
  deriving stock (Show, Eq)
