module NOM.Util (foldMapEndo, forMaybeM, addPrintCache, (<||>), hush, (<|>>), (|>), (<.>>), (.>)) where

import Relude
import Relude.Extra (toSnd)

foldMapEndo :: Foldable f => (b -> a -> a) -> f b -> a -> a
foldMapEndo f = foldMap (f .> Endo) .> appEndo

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM

addPrintCache :: Functor m => (update -> (istate, state) -> m (errors, (istate, Maybe state))) -> (state -> cache) -> update -> (istate, state, cache) -> m (errors, (istate, state, cache))
addPrintCache updater cacher update (oldIState, oldState, oldCache) =
  updater update (oldIState, oldState) <|>> \(errors, (istate, stateMay)) ->
    let (newState, newCache) = maybe (oldState, oldCache) (toSnd cacher) stateMay
     in (errors, (istate, newState, newCache))

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

-- Like in  'errors'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-- Like in 'flow'
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

-- Like in 'flow'
infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) = (&)

-- Functorial version of .>
infixl 8 <.>>
(<.>>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f <.>> g = f .> fmap g

-- Functorial version of |>
infixl 8 <|>>
(<|>>) :: Functor f => f a -> (a -> b) -> f b
f <|>> g = f |> fmap g
