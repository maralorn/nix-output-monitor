module NOM.State.CacheId where

import Relude

newtype CacheId b = MkCacheId {cidToInt :: Int}
  deriving stock (Show, Eq, Ord, Read, Generic)
