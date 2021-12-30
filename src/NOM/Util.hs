module NOM.Util where

import Relude

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
