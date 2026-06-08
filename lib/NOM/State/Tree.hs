module NOM.State.Tree (
  mapRootsTwigsAndLeaves,
) where

import Data.Tree (Tree (Node))
import Relude

mapRootsTwigsAndLeaves :: (a -> b) -> (a -> b) -> (a -> b) -> Tree a -> Tree b
mapRootsTwigsAndLeaves mapRoot mapTwig mapLeaf = go True
 where
  go top = \case
    Node l [] -> Node (mapLeaf l) []
    Node l rest | top -> Node (mapRoot l) (go False <$> rest)
    Node l rest -> Node (mapTwig l) (go False <$> rest)
