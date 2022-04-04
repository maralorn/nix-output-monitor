module NOM.State.CacheId.Map (
  NOM.State.CacheId.Map.filter,
  NOM.State.CacheId.Map.toList,
  lookup,
  insert,
  keysSet,
  nextKey,
  adjust,
  delete,
  elems,
  size,
  NOM.State.CacheId.Map.null,
  CacheIdMap,
) where

import Relude

import Data.IntMap.Strict qualified as IntMap

import NOM.State.CacheId (CacheId (MkCacheId))
import NOM.State.CacheId.Set qualified as CSet
import NOM.Util ((.>), (|>))

newtype CacheIdMap b a = MkCacheIdMap {intMap :: IntMap a}
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving newtype (Semigroup, Monoid, Foldable, Functor, NFData)

filter :: (a -> Bool) -> CacheIdMap b a -> CacheIdMap b a
filter p = (.intMap) .> IntMap.filter p .> MkCacheIdMap

lookup :: CacheId b -> CacheIdMap b a -> Maybe a
lookup (MkCacheId index) cmap = IntMap.lookup index cmap.intMap

toList :: CacheIdMap b a -> [(CacheId b, a)]
toList = (.intMap) .> IntMap.toList .> fmap (first MkCacheId)

insert :: CacheId b -> a -> CacheIdMap b a -> CacheIdMap b a
insert (MkCacheId index) value cmap = IntMap.insert index value cmap.intMap |> MkCacheIdMap

keysSet :: CacheIdMap b a -> CSet.CacheIdSet b
keysSet = (.intMap) .> IntMap.keysSet .> coerce

elems :: CacheIdMap b a -> [a]
elems = (.intMap) .> IntMap.elems

nextKey :: CacheIdMap b a -> CacheId b
nextKey = (.intMap) .> IntMap.lookupMax .> maybe 0 (fst .> (+ 1)) .> MkCacheId

adjust :: (a -> a) -> CacheId b -> CacheIdMap b a -> CacheIdMap b a
adjust f (MkCacheId key) = (.intMap) .> IntMap.adjust f key .> MkCacheIdMap

delete :: CacheId b -> CacheIdMap b a -> CacheIdMap b a
delete (MkCacheId key) = (.intMap) .> IntMap.delete key .> coerce

size :: CacheIdMap b a -> Int
size = (.intMap) .> IntMap.size

null :: CacheIdMap b a -> Bool
null = (.intMap) .> IntMap.null
