module NOM.NixEvent (NixEvent (..)) where

import Relude

import NOM.Builds (Derivation (..), FailType, Host (..), StorePath (..))
import NOM.Error (NOMError)
import NOM.NixEvent.Action (NixAction)

data NixEvent
  = Uploading !StorePath !Host
  | Downloading !StorePath !Host
  | PlanCopies !Int
  | Build Derivation !Host
  | PlanBuilds (Set Derivation) !Derivation
  | PlanDownloads !Double !Double (Set StorePath)
  | Checking !Derivation
  | Failed !Derivation !FailType
  | JsonMessage !(Either NOMError NixAction)
  deriving stock (Show, Eq)
