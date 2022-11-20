module NOM.NixMessage.OldStyle (NixOldStyleMessage (..)) where

import NOM.Builds (Derivation (..), FailType, Host (..), StorePath (..))
import Relude

data NixOldStyleMessage
  = Uploading StorePath Host
  | Downloading StorePath Host
  | PlanCopies Int
  | Build Derivation Host
  | PlanBuilds (Set Derivation) Derivation
  | PlanDownloads Double Double (Set StorePath)
  | Checking Derivation
  | Failed Derivation FailType
  deriving stock (Show, Eq)
