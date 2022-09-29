module NOM.NixEvent (ParseResult(..)) where

import Relude

import NOM.Builds (StorePath(..), Host (..), Derivation (..), FailType)
import NOM.Error (NOMError)
import NOM.NixEvent.Action (InternalJson)

data ParseResult
  = Uploading !StorePath !Host
  | Downloading !StorePath !Host
  | PlanCopies !Int
  | Build Derivation !Host
  | PlanBuilds (Set Derivation) !Derivation
  | PlanDownloads !Double !Double (Set StorePath)
  | Checking !Derivation
  | Failed !Derivation !FailType
  | JsonMessage !(Either NOMError InternalJson)
  deriving stock (Show, Eq)