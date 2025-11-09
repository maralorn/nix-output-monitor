module NOM.State.CacheId.Map (
  NOM.State.CacheId.Map.filter,
  NOM.State.CacheId.Map.toList,
  lookup,
  insert,
  keysSet,
  nextKey,
  adjust,
  delete,
  size,
  NOM.State.CacheId.Map.null,
  CacheIdMap,
) where

import Data.IntMap.Strict qualified as IntMap
import NOM.State.CacheId (CacheId (MkCacheId))
import NOM.State.CacheId.Set qualified as CSet
import Relude

type CacheIdMap :: Type -> Type -> Type
newtype CacheIdMap b a = MkCacheIdMap {intMap :: IntMap a}
  deriving stock (Show, Eq, Ord, Read)
  deriving newtype (Semigroup, Monoid, Foldable, Functor)

filter :: (a -> Bool) -> CacheIdMap b a -> CacheIdMap b a
filter p = MkCacheIdMap . IntMap.filter p . (.intMap)

lookup :: CacheId b -> CacheIdMap b a -> Maybe a
lookup (MkCacheId index) cmap = IntMap.lookup index cmap.intMap

toList :: CacheIdMap b a -> [(CacheId b, a)]
toList = fmap (first MkCacheId) . IntMap.toList . (.intMap)

insert :: CacheId b -> a -> CacheIdMap b a -> CacheIdMap b a
insert (MkCacheId index) value cmap = MkCacheIdMap $ IntMap.insert index value cmap.intMap

keysSet :: CacheIdMap b a -> CSet.CacheIdSet b
keysSet = coerce . IntMap.keysSet . (.intMap)

nextKey :: CacheIdMap b a -> CacheId b
nextKey = MkCacheId . maybe 0 ((+ 1) . fst) . IntMap.lookupMax . (.intMap)

adjust :: (a -> a) -> CacheId b -> CacheIdMap b a -> CacheIdMap b a
adjust f (MkCacheId key) = MkCacheIdMap . IntMap.adjust f key . (.intMap)

delete :: CacheId b -> CacheIdMap b a -> CacheIdMap b a
delete (MkCacheId key) = coerce . IntMap.delete key . (.intMap)

size :: CacheIdMap b a -> Int
size = IntMap.size . (.intMap)

null :: CacheIdMap b a -> Bool
null = IntMap.null . (.intMap)
