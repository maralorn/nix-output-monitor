module NOM.State.CacheId (CacheId (..)) where

import Data.MemoTrie (HasTrie (..))
import Relude

newtype CacheId b = MkCacheId {unCacheId :: Int}
  deriving stock (Show, Eq, Ord, Read, Generic)

instance HasTrie (CacheId b) where
  newtype CacheId b :->: c = CacheIdTrie (Int :->: c)
  trie = coerce (trie @Int)
  untrie = coerce (untrie @Int)
  enumerate = coerce (enumerate @Int)
