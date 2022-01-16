module NOM.Util where

import Relude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
(<<|>>>) :: (Functor f, Functor g)  => f (g a) -> (a -> b) -> f (g b)
f <<|>>> g = f <|>> fmap g
