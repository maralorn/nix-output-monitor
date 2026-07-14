module NOM.State.CacheId (CacheId (..)) where

import Relude

type CacheId :: Type -> Type
newtype CacheId b = MkCacheId {unCacheId :: Int}
  deriving newtype (Show, Eq, Ord, Read)
