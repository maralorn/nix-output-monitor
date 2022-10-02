module NOM.Util (foldMapEndo, forMaybeM, addPrintCache, hush) where

import Relude
import Relude.Extra (toSnd)

foldMapEndo :: Foldable f => (b -> a -> a) -> f b -> a -> a
foldMapEndo f = appEndo . foldMap (Endo . f)

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM

-- Like in 'flow'
{-# INLINE addPrintCache #-}
addPrintCache :: Functor m => (update -> (istate, state) -> m (errors, (istate, Maybe state))) -> (state -> cache) -> update -> (istate, state, cache) -> m (errors, (istate, state, cache))
addPrintCache updater cacher update (!oldIState, !oldState, oldCache) =
  updater update (oldIState, oldState) <&> \(errors, (istate, stateMay)) ->
    let (!newState, newCache) = maybe (oldState, oldCache) (toSnd cacher) stateMay
     in (errors, (istate, newState, newCache))

-- Like in  'errors'
{-# INLINE hush #-}
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
