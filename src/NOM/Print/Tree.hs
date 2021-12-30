module NOM.Print.Tree where

import Relude

import NOM.Print.Table ( blue, bold, markup )
import NOM.State.Tree ( Tree(..) )

showForest :: NonEmpty (Tree Text Text) -> NonEmpty Text
showForest = go False
 where
  go indent = join . (if indent then onLastAndRest (onFirstAndRest (markup blue "└─ " <>) (markup blue "   " <>)) (onFirstAndRest (markup blue "├─ " <>) (markup blue "│  " <>)) else id) . fmap showTree
  showTree = \case
    Leaf text' -> pure text'
    Node label' (single :| []) -> onFirstAndRest ((label' <> markup blue " ── ") <>) id (showTree single)
    Node label' content -> label' :| toList (go True content)
    Link text' -> pure (markup blue (text' <> markup bold " ⤴"))
  onFirstAndRest f g (x :| xs) = f x :| (g <$> xs)
  onLastAndRest f _ (x :| []) = f x :| []
  onLastAndRest f g (x :| (y : ys)) = g x :| toList (onLastAndRest f g (y :| ys))

--exampleForest :: NonEmpty (Tree Text Text)
--exampleForest = Node "1" (Leaf "a" :| [Node "5" (Leaf "b" :| [Leaf "uiae"]), Node "9" (pure (Leaf "e"))]) :| [Node "3" (Leaf "b" :| [Leaf "uiae"]), Node "4" (pure (Leaf "c"))]

-- >>> showForest $ exampleForest
-- "1" :| ["\ESC[34m\9500\9472 \ESC[0ma","\ESC[34m\9500\9472 \ESC[0m5","\ESC[34m\9474  \ESC[0m\ESC[34m\9500\9472 \ESC[0mb","\ESC[34m\9474  \ESC[0m\ESC[34m\9492\9472 \ESC[0muiae","\ESC[34m\9492\9472 \ESC[0m9\ESC[34m \9472\9472 \ESC[0me","3","\ESC[34m\9500\9472 \ESC[0mb","\ESC[34m\9492\9472 \ESC[0muiae","4\ESC[34m \9472\9472 \ESC[0mc"]
