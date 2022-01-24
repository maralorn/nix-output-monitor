module NOM.State.Tree (
  ForestUpdate (..),
  replaceDuplicates,
  updateForest,
  sortForest,
  aggregateTree,
  collapseForestN,
  mapTwigsAndLeafs,
) where

import Relude

import qualified Data.Set as Set
import Data.Tree (Forest, Tree (Node, subForest), rootLabel)

import NOM.Util ((.>), (|>))

subTrees :: Ord a => Tree a -> Set (Tree a)
subTrees t = Set.insert t (foldMap subTrees (subForest t))

data ForestUpdate a = ForestUpdate
  { match :: a -> Bool
  , isChild :: a -> Bool
  , isParent :: a -> Bool
  , update :: a -> a
  , def :: a
  }

updateForest :: forall a. Ord a => ForestUpdate a -> Forest a -> Forest a
updateForest ForestUpdate{..} forest =
  updateIfPresent forest
    |> fromMaybe
      ( forest
          |> filter (not . isChild . rootLabel)
          .> appendWhereMatching (Node def children)
      )
 where
  children :: [Tree a]
  children = filter (isChild . rootLabel) (toList (foldMap subTrees forest))

  updateIfPresent :: Forest a -> Maybe (Forest a)
  updateIfPresent = go .> \(found, result) -> if found then Just result else Nothing
   where
    go f = let f' = updateTree <$> f in (or $ fst <$> f', snd <$> f')
    updateTree (Node label (go -> (subMatch, subForest))) =
      (match label || subMatch, Node (if match label then update label else label) subForest)

  appendWhereMatching :: Tree a -> Forest a -> Forest a
  appendWhereMatching treeToInsert = uncurry prependIfNoMatch . go
   where
    prependIfNoMatch :: Bool -> Forest a -> Forest a
    prependIfNoMatch found = if found then id else (treeToInsert :)
    go :: Forest a -> (Bool, Forest a)
    go (fmap goTree -> f) = (or $ fst <$> f, snd <$> f)
    goTree :: Tree a -> (Bool, Tree a)
    goTree (Node label subForest) =
      let matches = isParent label
          (subMatch, subForest') = go subForest
       in (matches || subMatch, Node label ((if matches then (treeToInsert :) else id) subForest'))

mapTwigsAndLeafs :: (a -> b) -> (a -> b) -> Tree a -> Tree b
mapTwigsAndLeafs mapTwig mapLeaf = go
 where
  go = \case
    Node l [] -> Node (mapLeaf l) []
    Node l rest -> Node (mapTwig l) (go <$> rest)

aggregateTree :: Monoid b => (a -> b) -> Tree a -> Tree (a, b)
aggregateTree summary = go
 where
  go (Node x (fmap go -> xs)) = Node (x, foldMap (uncurry (<>) . first summary . rootLabel) xs) xs

collapseForestN :: forall a b. Semigroup b => (a -> Bool) -> Int -> Forest (Maybe a, b) -> (Int, Forest (Maybe a, b))
collapseForestN elider = go
 where
  canElide :: Tree (Maybe a, b) -> Maybe b
  canElide = \case
    Node (a, b) [] -> a |> maybe True elider .> bool Nothing (Just b)
    _ -> Nothing
  go :: Int -> Forest (Maybe a, b) -> (Int, Forest (Maybe a, b))
  go n forest | n <= 0 = (n, forest)
  go n [] = (n, [])
  go n (x : (go n -> (nAfterXs, xs)))
    | nAfterXs <= 0 = (nAfterXs, x : xs)
    | Just b <- canElide x', ((canElide -> Just b') : rest) <- xs = (nAfterX - 1, pure (Nothing, b <> b') : rest)
    | otherwise = (nAfterX, x' : xs)
   where
    (nAfterX, x') = collapseNTree nAfterXs x
  collapseNTree :: Int -> Tree (Maybe a, b) -> (Int, Tree (Maybe a, b))
  collapseNTree n tree
    | n <= 0 = (n, tree)
  collapseNTree n (Node label (go n -> (nAfterForest, subForest)))
    | nAfterForest > 0, (a, b) <- label, [canElide -> Just b'] <- subForest = (nAfterForest - 1, pure (a, b <> b'))
    | otherwise = (nAfterForest, Node label subForest)

sortForest :: Ord c => (Tree a -> c) -> Forest a -> Forest a
sortForest order = go
 where
  go = fmap sortTree . sort'
  sortTree (Node x c) = Node x (go c)
  sort' = sortOn order

replaceDuplicates :: forall a b. Ord a => (a -> b) -> Forest a -> Forest (Either a b)
replaceDuplicates link = snd . filterList mempty
 where
  filterList :: Set (Tree a) -> Forest a -> (Set (Tree a), Forest (Either a b))
  filterList seen [] = (seen, [])
  filterList seen (x : xs) =
    let (seen', x') = if Set.member x seen then (seen, substitute x) else filterTree seen x
        (seen'', xs') = filterList (Set.insert x seen') xs
     in (seen'', x' : xs')
  filterTree :: Set (Tree a) -> Tree a -> (Set (Tree a), Tree (Either a b))
  filterTree seen (Node x t) = second (Node (Left x)) (filterList seen t)
  substitute :: Tree a -> Tree (Either a b)
  substitute (Node a _) = pure (Right (link a))
