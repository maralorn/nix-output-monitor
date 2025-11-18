module NOM.NixMessage.JSON (NixJSONMessage (..), StartAction (..), StopAction (..), MessageAction (..), ResultAction (..), ActivityResult (..), Activity (..), ActivityId (..), Verbosity (..), ActivityProgress (..), ActivityType (..)) where

import NOM.Builds (Derivation (..), Host (..), HostContext (..), StorePath (..))
import NOM.Error (NOMError)
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude

newtype ActivityId = MkId {value :: Int}
  deriving newtype (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''ActivityId

newtype StopAction = MkStopAction {id :: ActivityId}
  deriving newtype (Eq)
  deriving stock (Show)

data Verbosity where
  Error :: Verbosity
  Warn :: Verbosity
  Notice :: Verbosity
  Info :: Verbosity
  Talkative :: Verbosity
  Chatty :: Verbosity
  Debug :: Verbosity
  Vomit :: Verbosity
  deriving stock (Show, Eq, Ord)

data ActivityType where
  UnknownType :: ActivityType
  CopyPathType :: ActivityType
  FileTransferType :: ActivityType
  RealiseType :: ActivityType
  CopyPathsType :: ActivityType
  BuildsType :: ActivityType
  BuildType :: ActivityType
  OptimiseStoreType :: ActivityType
  VerifyPathsType :: ActivityType
  SubstituteType :: ActivityType
  QueryPathInfoType :: ActivityType
  PostBuildHookType :: ActivityType
  BuildWaitingType :: ActivityType
  FetchTreeType :: ActivityType
  deriving stock (Show, Eq)

-- | nix src/libutil/include/nix/util/logging.hh ActivityType
data Activity where
  Unknown :: Activity
  CopyPath :: StorePath -> Host WithContext -> Host WithContext -> Activity
  FileTransfer :: Text -> Activity
  Realise :: Activity
  CopyPaths :: Activity
  Builds :: Activity
  Build :: Derivation -> Host WithContext -> Activity
  OptimiseStore :: Activity
  VerifyPaths :: Activity
  Substitute :: StorePath -> Host WithContext -> Activity
  QueryPathInfo :: StorePath -> Host WithContext -> Activity
  PostBuildHook :: Derivation -> Activity
  BuildWaiting :: Activity
  FetchTree :: Activity
  deriving stock (Show, Eq, Ord)

-- | nix src/libutil/include/nix/util/logging.hh ResultType
data ActivityResult where
  FileLinked :: Int -> Int -> ActivityResult
  BuildLogLine :: Text -> ActivityResult
  UntrustedPath :: StorePath -> ActivityResult
  CorruptedPath :: StorePath -> ActivityResult
  SetPhase :: Text -> ActivityResult
  Progress :: ActivityProgress -> ActivityResult
  SetExpected :: ActivityType -> Int -> ActivityResult
  PostBuildLogLine :: Text -> ActivityResult
  FetchStatus :: Text -> ActivityResult
  deriving stock (Show, Eq)

data ActivityProgress = MkActivityProgress
  { done :: Int
  , expected :: Int
  , running :: Int
  , failed :: Int
  }
  deriving stock (Show, Eq, Ord)

makeFieldLabelsNoPrefix ''ActivityProgress

data StartAction = MkStartAction
  { id :: ActivityId
  , level :: Verbosity
  , text :: Text
  , activity :: Activity
  }
  deriving stock (Show, Eq)

makeFieldLabelsNoPrefix ''StartAction

data ResultAction = MkResultAction
  { id :: ActivityId
  , result :: ActivityResult
  }
  deriving stock (Show, Eq)

makeFieldLabelsNoPrefix ''ResultAction

data MessageAction = MkMessageAction
  { level :: Verbosity
  , message :: Text
  -- currently unused, but theoretically present in the protocol
  -- , line :: Maybe Int
  -- , column :: Maybe Int
  -- , file :: Maybe Text
  }
  deriving stock (Show, Eq)

makeFieldLabelsNoPrefix ''MessageAction

data NixJSONMessage where
  Stop :: StopAction -> NixJSONMessage
  Start :: StartAction -> NixJSONMessage
  Result :: ResultAction -> NixJSONMessage
  Message :: MessageAction -> NixJSONMessage
  Plain :: ByteString -> NixJSONMessage
  ParseError :: NOMError -> NixJSONMessage
  deriving stock (Show, Eq)
