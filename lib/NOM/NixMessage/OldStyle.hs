module NOM.NixMessage.OldStyle (NixOldStyleMessage (..)) where

import NOM.Builds (Derivation (..), FailType, Host (..), HostContext (..), StorePath (..))
import Relude

data NixOldStyleMessage where
  Uploading :: StorePath -> Host WithContext -> NixOldStyleMessage
  Downloading :: StorePath -> Host WithContext -> NixOldStyleMessage
  PlanCopies :: Int -> NixOldStyleMessage
  Build :: Derivation -> Host WithContext -> NixOldStyleMessage
  PlanBuilds :: Set Derivation -> Derivation -> NixOldStyleMessage
  PlanDownloads :: Double -> Double -> Set StorePath -> NixOldStyleMessage
  Checking :: Derivation -> NixOldStyleMessage
  Failed :: Derivation -> FailType -> NixOldStyleMessage
  deriving stock (Show, Eq)
