module NOM.State.Tree (Tree (..), mergeForest, reverseForest, filterDoubles, sortForest) where

import Relude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Tree a b
  = Node !a !(NonEmpty (Tree a b))
  | Leaf !b
  | Link !a
  deriving (Eq, Show, Ord, Read)

instance Bifunctor Tree where
  bimap f g = \case
    Node x xs -> Node (f x) (bimap f g <$> xs)
    Leaf x -> Leaf (g x)
    Link x -> Link (f x)

sortForest :: Ord c => (Tree a b -> c) -> NonEmpty (Tree a b) -> NonEmpty (Tree a b)
sortForest order = go
 where
  go = fmap sortTree . sort'
  sortTree = \case
    Node x c -> Node x (go c)
    a -> a
  sort' = NonEmpty.sortBy (on compare order)

mergeForest :: Eq a => NonEmpty (Tree a b) -> NonEmpty (Tree a b)
mergeForest (x :| xs) = foldl' (flip mergeIntoForest . toList) (pure x) xs

mergeIntoForest :: Eq a => Tree a b -> [Tree a b] -> NonEmpty (Tree a b)
mergeIntoForest x [] = pure x
mergeIntoForest x (y : ys) = maybe (y :| toList (mergeIntoForest x ys)) (:| ys) (mergeTrees x y)

mergeTrees :: Eq a => Tree a b -> Tree a b -> Maybe (Tree a b)
mergeTrees (Node x xs) (Node y ys) | x == y = Just (Node x (mergeForest (xs <> ys)))
mergeTrees _ _ = Nothing

-- >>> mergeForest $ (Node 1 (pure (Leaf "a"))) :| [Node 3 (pure (Leaf "b")), Node 3 (pure (Leaf "c"))]
-- Node 1 (Leaf "a" :| []) :| [Node 3 (Leaf "b" :| [Leaf "c"])]

reverseForest :: forall a b. (Ord a, Ord b) => (a -> b) -> Map b (Set b) -> NonEmpty a -> NonEmpty (Tree b a)
reverseForest f parents = (start =<<)
 where
  start :: a -> NonEmpty (Tree b a)
  start x = reverseTree (f x) (pure (Leaf x))
  reverseTree :: b -> NonEmpty (Tree b a) -> NonEmpty (Tree b a)
  reverseTree x t = case lookup x of
    Nothing -> t
    Just pars -> (\y -> reverseTree y (pure $ Node y t)) =<< pars
  lookup x = nonEmpty . toList =<< Map.lookup x parents

filterDoubles :: (Ord a, Ord b) => (b -> a) -> NonEmpty (Tree a b) -> NonEmpty (Tree a b)
filterDoubles f = snd . filterNonEmpty mempty
 where
  filterNonEmpty seen (x :| xs) =
    let (seen', x') = if Set.member x seen then (seen, substitute x) else filterTree seen x
        (seen'', xs') = filterList (Set.insert x seen') xs
     in (seen'', x' :| xs')
  filterList seen [] = (seen, [])
  filterList seen (x : xs) = second toList (filterNonEmpty seen (x :| xs))
  filterTree seen = \case
    Node x t -> second (Node x) (filterNonEmpty seen t)
    x -> (seen, x)
  substitute = \case
    Leaf x -> Link (f x)
    Node x _ -> Link x
    x -> x
