module NOM.State.Tree (
  mapRootsTwigsAndLeafs,
) where

import Relude

import Data.Tree (Tree (Node))

mapRootsTwigsAndLeafs :: (a -> b) -> (a -> b) -> (a -> b) -> Tree a -> Tree b
mapRootsTwigsAndLeafs mapRoot mapTwig mapLeaf = go True
 where
  go top = \case
    Node l [] -> Node (mapLeaf l) []
    Node l rest | top -> Node (mapRoot l) (go False <$> rest)
    Node l rest -> Node (mapTwig l) (go False <$> rest)
