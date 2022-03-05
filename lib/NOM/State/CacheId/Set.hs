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

fromFoldable :: Foldable f => f (CacheId b) -> CacheIdSet b
fromFoldable = foldl' (flip insert) mempty

null :: CacheIdSet b -> Bool
null = cidSet .> IntSet.null

maxView :: CacheIdSet b -> Maybe (CacheId b, CacheIdSet b)
maxView = cidSet .> IntSet.maxView .> coerce

union :: CacheIdSet b -> CacheIdSet b -> CacheIdSet b
union = coerce IntSet.union

difference :: CacheIdSet b -> CacheIdSet b -> CacheIdSet b
difference = coerce IntSet.difference

delete :: CacheId b -> CacheIdSet b -> CacheIdSet b
delete = coerce IntSet.delete

size :: CacheIdSet b -> Int
size = coerce IntSet.size

isSubsetOf :: CacheIdSet b -> CacheIdSet b -> Bool
isSubsetOf = coerce IntSet.isSubsetOf