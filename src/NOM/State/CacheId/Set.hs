module NOM.State.CacheId.Set where

import Relude
import NOM.State.CacheId (CacheId (MkCacheId))
import qualified Data.IntSet as IntSet
import NOM.Util ((.>), (<.>>))

newtype CacheIdSet b = MkCacheIdSet {cidSet :: IntSet}
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving newtype (Semigroup, Monoid)

insert :: CacheId b -> CacheIdSet b -> CacheIdSet b
insert (MkCacheId x) = cidSet .> IntSet.insert x .> MkCacheIdSet

toList :: CacheIdSet b -> [CacheId b]
toList = cidSet .> IntSet.toList <.>> MkCacheId
