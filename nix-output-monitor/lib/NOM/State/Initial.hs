module NOM.State.Initial (initialStateFromBuildPlatform) where

import Data.Strict qualified as Strict
import NOM.State (EvalInfo (..), NOMState (..), ProgressState (..))
import NOM.Update.Monad (MonadCacheBuildReports (..), MonadNow (..))
import Relude

initialStateFromBuildPlatform :: (MonadCacheBuildReports m, MonadNow m) => Maybe Text -> m NOMState
initialStateFromBuildPlatform platform = do
  now <- getNow
  buildReports <- getCachedBuildReports
  pure
    $ MkNOMState
      mempty
      mempty
      mempty
      mempty
      buildReports
      now
      JustStarted
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (Strict.toStrict platform)
      mempty
      MkEvalInfo{count = 0, at = 0, lastFileName = Strict.Nothing}
