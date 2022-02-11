module NOM.State.CacheId.Set where

import Relude

newtype CacheIdSet b = MkCacheIdSet {cidSet :: IntSet}
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving newtype (Semigroup, Monoid)
