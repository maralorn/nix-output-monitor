module NOM.NixMessage.OldStyle (NixOldStyleMessage (..)) where

import Relude

import NOM.Builds (Derivation (..), FailType, Host (..), StorePath (..))

data NixOldStyleMessage
  = Uploading !StorePath !Host
  | Downloading !StorePath !Host
  | PlanCopies !Int
  | Build Derivation !Host
  | PlanBuilds (Set Derivation) !Derivation
  | PlanDownloads !Double !Double (Set StorePath)
  | Checking !Derivation
  | Failed !Derivation !FailType
  deriving stock (Show, Eq)
