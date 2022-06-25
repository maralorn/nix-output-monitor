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
import NOM.Util ((.>), (<.>>))

newtype CacheIdSet b = MkCacheIdSet {ints :: IntSet}
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving newtype (Semigroup, Monoid, NFData)

insert :: CacheId b -> CacheIdSet b -> CacheIdSet b
insert (MkCacheId x) = (.ints) .> IntSet.insert x .> MkCacheIdSet

toList :: CacheIdSet b -> [CacheId b]
toList = (.ints) .> IntSet.toList <.>> MkCacheId

fromFoldable :: Foldable f => f (CacheId b) -> CacheIdSet b
fromFoldable = foldl' (flip insert) mempty

null :: CacheIdSet b -> Bool
null = (.ints) .> IntSet.null

maxView :: CacheIdSet b -> Maybe (CacheId b, CacheIdSet b)
maxView = (.ints) .> IntSet.maxView .> coerce

head :: CacheIdSet b -> Maybe (CacheId b)
head = (.ints) .> IntSet.maxView .> fmap fst .> coerce

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
