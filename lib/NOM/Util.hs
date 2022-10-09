module NOM.Util (foldMapEndo, forMaybeM, addPrintCache, hush, diffTime, relTimeToSeconds) where

import Data.Time (NominalDiffTime)
import Relude
import Relude.Extra (toSnd)
import Streamly.Internal.Data.Time.Units (AbsTime, MilliSecond64 (..), RelTime, diffAbsTime, fromRelTime)

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

diffTime :: AbsTime -> AbsTime -> NominalDiffTime
diffTime = fmap relTimeToSeconds . diffAbsTime

relTimeToSeconds :: RelTime -> NominalDiffTime
relTimeToSeconds rel_time = case fromRelTime rel_time of
  MilliSecond64 milli_sec -> fromInteger $ toInteger milli_sec `div` 1000