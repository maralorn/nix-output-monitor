module NOM.State.CacheId.Set (
  insert,
  CacheIdSet (MkCacheIdSet),
  NOM.State.CacheId.Set.toList,
  NOM.State.CacheId.Set.null,
  fromFoldable,
  maxView,
  union,
  difference,
  intersection,
  delete,
  size,
  isSubsetOf,
  member,
  head,
) where

import Relude hiding (head)

import Data.IntSet qualified as IntSet

import NOM.State.CacheId (CacheId (MkCacheId))

newtype CacheIdSet b = MkCacheIdSet {ints :: IntSet}
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving newtype (Semigroup, Monoid)

insert :: CacheId b -> CacheIdSet b -> CacheIdSet b
insert (MkCacheId x) = MkCacheIdSet . IntSet.insert x . (.ints)

toList :: CacheIdSet b -> [CacheId b]
toList = fmap MkCacheId . IntSet.toList . (.ints)

fromFoldable :: Foldable f => f (CacheId b) -> CacheIdSet b
fromFoldable = foldl' (flip insert) mempty

null :: CacheIdSet b -> Bool
null = IntSet.null . (.ints)

maxView :: CacheIdSet b -> Maybe (CacheId b, CacheIdSet b)
maxView = coerce . IntSet.maxView . (.ints)

head :: CacheIdSet b -> Maybe (CacheId b)
head = coerce . fmap fst . IntSet.maxView . (.ints)

union :: CacheIdSet b -> CacheIdSet b -> CacheIdSet b
union = coerce IntSet.union

intersection :: CacheIdSet b -> CacheIdSet b -> CacheIdSet b
intersection = coerce IntSet.intersection

difference :: CacheIdSet b -> CacheIdSet b -> CacheIdSet b
difference = coerce IntSet.difference

delete :: CacheId b -> CacheIdSet b -> CacheIdSet b
delete = coerce IntSet.delete

size :: CacheIdSet b -> Int
size = coerce IntSet.size

isSubsetOf :: CacheIdSet b -> CacheIdSet b -> Bool
isSubsetOf = coerce IntSet.isSubsetOf

member :: CacheId b -> CacheIdSet b -> Bool
member = coerce IntSet.member
