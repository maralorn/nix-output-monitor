module NOM.Print.Tree where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Relude

data Tree a b = Node a (NonEmpty (Tree a b)) | Leaf b | Link a deriving (Eq, Show, Ord)

mergeForest :: (Ord a, Ord b) => NonEmpty (Tree a b) -> NonEmpty (Tree a b)
mergeForest (x :| xs) = NonEmpty.sort $ foldl' (flip mergeIntoForest . toList) (pure x) xs

mergeIntoForest :: (Ord a, Ord b) => Tree a b -> [Tree a b] -> NonEmpty (Tree a b)
mergeIntoForest x [] = pure x
mergeIntoForest x (y : ys) = maybe (y :| toList (mergeIntoForest x ys)) (:| ys) (mergeTrees x y)

mergeTrees :: (Ord a, Ord b) => Tree a b -> Tree a b -> Maybe (Tree a b)
mergeTrees (Node x xs) (Node y ys) | x == y = Just (Node x (mergeForest (xs <> ys)))
mergeTrees _ _ = Nothing

-- >>> mergeForest $ (Node 1 (pure (Leaf "a"))) :| [Node 3 (pure (Leaf "b")), Node 3 (pure (Leaf "c"))]
-- Node 1 (Leaf "a" :| []) :| [Node 3 (Leaf "c" :| [Leaf "b"])]

mapTree :: (a -> b) -> (c -> d) -> Tree a c -> Tree b d
mapTree f g = \case
  Node x xs -> Node (f x) (mapTree f g <$> xs)
  Leaf x -> Leaf (g x)
  Link x -> Link (f x)

showForest :: NonEmpty (Tree Text Text) -> NonEmpty Text
showForest = fmap (Text.drop 3) . go
 where
  go = join . onLastAndRest (onFirstAndRest ("└─ " <>) ("   " <>)) (onFirstAndRest ("├─ " <>) ("│  " <>)) . fmap showTree
  showTree = \case
    Leaf text -> pure text
    Node label (single :| []) -> onFirstAndRest ((label <> " ── ") <>) id (showTree single)
    Node label content -> label :| toList (go content)
    Link text -> pure (text <> " ⤴")
  onFirstAndRest f g (x :| xs) = f x :| (g <$> xs)
  onLastAndRest f _ (x :| []) = f x :| []
  onLastAndRest f g (x :| (y : ys)) = g x :| toList (onLastAndRest f g (y :| ys))

exampleForest :: NonEmpty (Tree Text Text)
exampleForest = Node "1" (Leaf "a" :| [Node "5" (Leaf "b" :| [Leaf "uiae"]), Node "9" (pure (Leaf "e"))]) :| [Node "3" (Leaf "b" :| [Leaf "uiae"]), Node "4" (pure (Leaf "c"))]

-- >>> showForest $ exampleForest
-- "1" :| ["\9500\9472 a","\9500\9472 5","\9474  \9500\9472 b","\9474  \9492\9472 uiae","\9492\9472 9 \9472 e","3","\9500\9472 b","\9492\9472 uiae","4 \9472 c"]

reverseForest :: forall a b. (Ord a, Ord b) => (a -> b) -> Map b (Set b) -> NonEmpty a -> NonEmpty (Tree b a)
reverseForest f parents = mergeForest . (start =<<)
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
