module Data.Sequence.Strict (
  Seq.sortOn,
  Seq.filter,
  (<|),
  Data.Sequence.Strict.fromList,
  Seq.null,
  Seq.length,
  Seq ((Seq.:<|)),
) where

import Data.Sequence qualified as Seq
import Relude

(<|) :: a -> Seq a -> Seq a
!item <| rest = item Seq.<| rest

fromList :: [a] -> Seq.Seq a
fromList = foldr (<|) mempty
