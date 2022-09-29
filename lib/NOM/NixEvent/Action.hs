module NOM.NixEvent.Action (NixAction (..), StartAction (..), StopAction (..), MessageAction (..), ResultAction (..), ActivityResult (..), Activity (..), ActivityId (..), Verbosity (..), ActivityProgress (..), ActivityType(..)) where

import Relude
import NOM.Builds (StorePath(..), Host (..), Derivation (..))

newtype ActivityId = MkId {value :: Int}
  deriving newtype (Show, Eq, Ord)

newtype StopAction = MkStopAction {id :: ActivityId}
  deriving newtype (Eq)
  deriving stock (Show)

data Verbosity = Error | Warn | Notice | Info | Talkative | Chatty | Debug | Vomit
  deriving stock (Show, Eq, Ord)

data ActivityType
  = UnknownType
  | CopyPathType
  | FileTransferType
  | RealiseType
  | CopyPathsType
  | BuildsType
  | BuildType
  | OptimiseStoreType
  | VerifyPathsType
  | SubstituteType
  | QueryPathInfoType
  | PostBuildHookType
  | BuildWaitingType
  deriving stock (Show, Eq)

data Activity
  = Unknown
  | CopyPath StorePath Host Host
  | FileTransfer Text
  | Realise
  | CopyPaths
  | Builds
  | Build Derivation Host Int Int
  | OptimiseStore
  | VerifyPaths
  | Substitute StorePath Host
  | QueryPathInfo StorePath Host
  | PostBuildHook Derivation
  | BuildWaiting
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

data ActivityResult
  = FileLinked Int Int
  | BuildLogLine Text
  | UntrustedPath StorePath
  | CorruptedPath StorePath
  | SetPhase Text
  | Progress ActivityProgress
  | SetExpected ActivityType Int
  | PostBuildLogLine Text
  deriving stock (Show, Eq)

data ActivityProgress = MkActivityProgress
  { done :: Int
  , expected :: Int
  , running :: Int
  , failed :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

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
  , line :: Maybe Int
  , column :: Maybe Int
  , file :: Maybe Text
  }
  deriving stock (Show, Eq)

data NixAction = Stop StopAction | Start StartAction | Result ResultAction | Message MessageAction
  deriving stock (Show, Eq)