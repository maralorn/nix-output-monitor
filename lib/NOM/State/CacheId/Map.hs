module NOM.State.CacheId.Map where

import qualified Data.IntMap.Strict as IntMap
import NOM.State.CacheId (CacheId (MkCacheId))
import NOM.Util ((.>))
import Relude
import qualified NOM.State.CacheId.Set as CSet

newtype CacheIdMap b a = MkCacheIdMap {unCacheIdMap :: IntMap a}
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving newtype (Semigroup, Monoid, Foldable, Functor)

filter :: (a -> Bool) -> CacheIdMap b a -> CacheIdMap b a
filter p = unCacheIdMap .> IntMap.filter p .> MkCacheIdMap

lookup :: Ord a => CacheId b -> CacheIdMap b a -> Maybe a
lookup i = unCacheIdMap .> IntMap.lookup (coerce i)

toList :: CacheIdMap b a -> [(CacheId b, a)]
toList = unCacheIdMap .> IntMap.toList .> fmap (first MkCacheId)

insert :: Ord a => CacheId b -> a -> CacheIdMap b a -> CacheIdMap b a
insert (MkCacheId i) val = unCacheIdMap .> IntMap.insert (coerce i) val .> MkCacheIdMap

keysSet :: CacheIdMap b a -> CSet.CacheIdSet b
keysSet = unCacheIdMap .> IntMap.keysSet .> coerce

nextKey :: CacheIdMap b a -> CacheId b
nextKey = unCacheIdMap .> IntMap.lookupMax .> maybe 0 (fst .> (+ 1)) .> MkCacheId

adjust :: (a -> a) -> CacheId b -> CacheIdMap b a -> CacheIdMap b a
adjust f (MkCacheId key) = unCacheIdMap .> IntMap.adjust f key .> MkCacheIdMap

delete :: CacheId b -> CacheIdMap b a -> CacheIdMap b a
delete (MkCacheId key) = unCacheIdMap .> IntMap.delete key .> coerce

size :: CacheIdMap b a -> Int
size = unCacheIdMap .> IntMap.size

null :: CacheIdMap b a -> Bool
null = unCacheIdMap .> IntMap.null