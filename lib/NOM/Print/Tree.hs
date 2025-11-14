module NOM.Print.Tree (showForest) where

import Data.Tree (Forest, Tree (..))
import NOM.Print.Table (blue, markup)
import Relude

showForest :: Forest (Text, a) -> [(Text, a)]
showForest = reverse . go False
 where
  go :: Bool -> Forest (Text, a) -> [(Text, a)]
  go indent =
    join
      . ( if indent
            then
              onLastAndRest
                (onFirstAndRest (first (markup blue "┌─ " <>)) (first ("   " <>)))
                (onFirstAndRest (first (markup blue "├─ " <>)) (first (markup blue "│  " <>)))
            else id
        )
      . fmap showTree
  showTree :: Tree (Text, a) -> [(Text, a)]
  showTree (Node label' content) = label' : go True content
  onFirstAndRest :: (a -> b) -> (a -> b) -> [a] -> [b]
  onFirstAndRest _ _ [] = []
  onFirstAndRest f g (x : xs) = f x : (g <$> xs)
  onLastAndRest :: (a -> b) -> (a -> b) -> [a] -> [b]
  onLastAndRest _ _ [] = []
  onLastAndRest f _ [x] = [f x]
  onLastAndRest f g (x : xs) = g x : onLastAndRest f g xs
