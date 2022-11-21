module NOM.NixMessage.JSON (NixJSONMessage (..), StartAction (..), StopAction (..), MessageAction (..), ResultAction (..), ActivityResult (..), Activity (..), ActivityId (..), Verbosity (..), ActivityProgress (..), ActivityType (..)) where

import NOM.Builds (Derivation (..), Host (..), StorePath (..))
import NOM.Error (NOMError)
import Relude

newtype ActivityId = MkId {value :: Int}
  deriving newtype (Show, Eq, Ord)
  deriving stock (Generic)

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
  deriving stock (Show, Eq)

data Activity where
  Unknown :: Activity
  CopyPath :: StorePath -> Host -> Host -> Activity
  FileTransfer :: Text -> Activity
  Realise :: Activity
  CopyPaths :: Activity
  Builds :: Activity
  Build :: Derivation -> Host -> Activity
  OptimiseStore :: Activity
  VerifyPaths :: Activity
  Substitute :: StorePath -> Host -> Activity
  QueryPathInfo :: StorePath -> Host -> Activity
  PostBuildHook :: Derivation -> Activity
  BuildWaiting :: Activity
  deriving stock (Show, Eq, Ord, Generic)

data ActivityResult where
  FileLinked :: Int -> Int -> ActivityResult
  BuildLogLine :: Text -> ActivityResult
  UntrustedPath :: StorePath -> ActivityResult
  CorruptedPath :: StorePath -> ActivityResult
  SetPhase :: Text -> ActivityResult
  Progress :: ActivityProgress -> ActivityResult
  SetExpected :: ActivityType -> Int -> ActivityResult
  PostBuildLogLine :: Text -> ActivityResult
  deriving stock (Show, Eq)

data ActivityProgress = MkActivityProgress
  { done :: Int
  , expected :: Int
  , running :: Int
  , failed :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

data StartAction = MkStartAction
  { id :: ActivityId
  , level :: Verbosity
  , text :: Text
  , activity :: Activity
  }
  deriving stock (Show, Eq)

data ResultAction = MkResultAction
  { id :: ActivityId
  , result :: ActivityResult
  }
  deriving stock (Show, Eq)

data MessageAction = MkMessageAction
  { level :: Verbosity
  , message :: Text
  -- currently unused, but theoretically present in the protocol
  -- , line :: Maybe Int
  -- , column :: Maybe Int
  -- , file :: Maybe Text
  }
  deriving stock (Show, Eq)

data NixJSONMessage where
  Stop :: StopAction -> NixJSONMessage
  Start :: StartAction -> NixJSONMessage
  Result :: ResultAction -> NixJSONMessage
  Message :: MessageAction -> NixJSONMessage
  Plain :: ByteString -> NixJSONMessage
  ParseError :: NOMError -> NixJSONMessage
  deriving stock (Show, Eq)
