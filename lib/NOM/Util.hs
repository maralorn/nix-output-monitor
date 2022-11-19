module NOM.Util (foldMapEndo, forMaybeM, diffTime, relTimeToSeconds) where

import Data.Time (NominalDiffTime)
import Relude
import Streamly.Internal.Data.Time.Units (AbsTime, MilliSecond64 (..), RelTime, diffAbsTime, fromRelTime)

foldMapEndo :: Foldable f => (b -> a -> a) -> f b -> a -> a
foldMapEndo f = appEndo . foldMap (Endo . f)

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM

relTimeToSeconds :: RelTime -> NominalDiffTime
relTimeToSeconds rel_time = case fromRelTime rel_time of
  MilliSecond64 milli_sec -> fromInteger $ toInteger milli_sec `div` 1000
