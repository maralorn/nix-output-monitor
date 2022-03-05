module NOM.State.CacheId.Map where

import qualified Data.IntMap.Strict as IntMap
import NOM.State.CacheId (CacheId (MkCacheId))
import NOM.Util ((.>))
import Relude

newtype CacheIdMap b a = MkCacheIdMap {cidMap :: IntMap a}
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving newtype (Semigroup, Monoid, Foldable, Functor)

filter :: (a -> Bool) -> CacheIdMap b a -> CacheIdMap b a
filter p = cidMap .> IntMap.filter p .> MkCacheIdMap

lookup :: Ord a => CacheId b -> CacheIdMap b a -> Maybe a
lookup i = cidMap .> IntMap.lookup (coerce i)

toList :: CacheIdMap b a -> [(CacheId b, a)]
toList = cidMap .> IntMap.toList .> fmap (first MkCacheId)

insert :: Ord a => CacheId b -> a -> CacheIdMap b a -> CacheIdMap b a
insert (MkCacheId i) val = cidMap .> IntMap.insert (coerce i) val .> MkCacheIdMap

nextKey :: CacheIdMap b a -> CacheId b
nextKey = cidMap .> IntMap.lookupMax .> maybe 0 (fst .> (+ 1)) .> MkCacheId

adjust :: (a -> a) -> CacheId b -> CacheIdMap b a -> CacheIdMap b a
adjust f (MkCacheId key) = cidMap .> IntMap.adjust f key .> MkCacheIdMap

delete :: CacheId b -> CacheIdMap b a -> CacheIdMap b a
delete (MkCacheId key) = cidMap .> IntMap.delete key .> coerce

size :: CacheIdMap b a -> Int
size = cidMap .> IntMap.size