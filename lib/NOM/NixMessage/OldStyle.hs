module NOM.NixMessage.OldStyle (NixOldStyleMessage (..)) where

import NOM.Builds (Derivation (..), FailType, Host (..), StorePath (..))
import Relude

data NixOldStyleMessage where
  Uploading :: StorePath -> Host -> NixOldStyleMessage
  Downloading :: StorePath -> Host -> NixOldStyleMessage
  PlanCopies :: Int -> NixOldStyleMessage
  Build :: Derivation -> Host -> NixOldStyleMessage
  PlanBuilds :: Set Derivation -> Derivation -> NixOldStyleMessage
  PlanDownloads :: Double -> Double -> Set StorePath -> NixOldStyleMessage
  Checking :: Derivation -> NixOldStyleMessage
  Failed :: Derivation -> FailType -> NixOldStyleMessage
  deriving stock (Show, Eq)
