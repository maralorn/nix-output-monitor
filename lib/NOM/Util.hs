module NOM.Util (foldMapEndo, forMaybeM) where

import Relude

foldMapEndo :: Foldable f => (b -> a -> a) -> f b -> a -> a
foldMapEndo f = appEndo . foldMap (Endo . f)

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM
