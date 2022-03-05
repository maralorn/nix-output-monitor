module NOM.Util where

import Relude
import Relude.Extra (toSnd)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

foldMapEndo :: Foldable f => (b -> a -> a) -> f b -> a -> a
foldMapEndo f = foldMap (f .> Endo) .> appEndo

foldEndo :: Foldable f => f (a -> a) -> a -> a
foldEndo = foldMapEndo id

passThroughBuffer :: Functor m => (update -> state -> m state) -> (update, buffer) -> state -> m (state, buffer)
passThroughBuffer updater (update, buffer) = updater update <.>> (,buffer)

secondM :: Functor m => (b -> m c) -> (a, b) -> m (a, c)
secondM f (a, b) = f b <|>> (a,)

firstFF :: (Functor f, Functor g) => (a -> f (g c)) -> (a, b) -> f (g (c, b))
firstFF f (a, b) = f a <<|>>> (,b)

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM

addPrintCache :: Functor m => (update -> (istate, state) -> m (istate, Maybe state)) -> (state -> cache) -> update -> (istate, state, cache) -> m (istate, state, cache)
addPrintCache updater cacher update (oldIState, oldState, oldCache) =
  updater update (oldIState, oldState)
    <|>> second (maybe (oldState, oldCache) (toSnd cacher)) .> (\(a, (b, c)) -> (a, b, c))

insertMultiMap :: (Ord k, Ord a) => k -> Set a -> Map k (Set a) -> Map k (Set a)
insertMultiMap = Map.insertWith Set.union

insertMultiMapOne :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
insertMultiMapOne k v = Map.insertWith Set.union k (one v)

collapseMultimap :: Ord b => Map a (Set b) -> Set b
collapseMultimap = Map.foldl' (<>) mempty

countPaths :: Ord b => Map a (Set b) -> Int
countPaths = Set.size . collapseMultimap

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

-- Double functorial version of .>
infixl 8 <<.>>>
(<<.>>>) :: (Functor f, Functor g) => (a -> f (g b)) -> (b -> c) -> a -> f (g c)
f <<.>>> g = f <.>> fmap g

-- Functorial version of |>
infixl 8 <|>>
(<|>>) :: Functor f => f a -> (a -> b) -> f b
f <|>> g = f |> fmap g

-- Functorial version of |>
infixl 8 <<|>>>
(<<|>>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
f <<|>>> g = f <|>> fmap g
