module NOM.Parser.JSON (InternalJson (..), StartAction (..), StopAction (..), MessageAction (..), ResultAction (..), ActivityResult (..), Activity (..), ActivityId (..), Verbosity (..), ActivityProgress (..)) where

import Relude

import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON

import NOM.Builds (Derivation, Host, StorePath)
import NOM.Util ((<|>>), (|>))

newtype ActivityId = MkId {value :: Int}
  deriving newtype (Show, Eq, Ord, NFData, JSON.FromJSON)

newtype StopAction = MkStopAction {id :: ActivityId}
  deriving newtype (Eq)
  deriving stock (Show)

data Verbosity = Error | Warn | Notice | Info | Talkative | Chatty | Debug | Vomit
  deriving stock (Show, Eq, Ord)

instance JSON.FromJSON Verbosity where
  parseJSON = JSON.withScientific "nix verbosity level" $ \case
    0 -> pure Error
    1 -> pure Warn
    2 -> pure Notice
    3 -> pure Info
    4 -> pure Talkative
    5 -> pure Chatty
    6 -> pure Debug
    7 -> pure Vomit
    other -> JSON.parseFail ("invalid verbosity level:" <> show other)

instance JSON.FromJSON ActivityType where
  parseJSON = JSON.withScientific "activity type" \case
    0 -> pure UnknownType
    100 -> pure CopyPathType
    101 -> pure FileTransferType
    102 -> pure RealiseType
    103 -> pure CopyPathsType
    104 -> pure BuildsType
    105 -> pure BuildType
    106 -> pure OptimiseStoreType
    107 -> pure VerifyPathsType
    108 -> pure SubstituteType
    109 -> pure QueryPathInfoType
    110 -> pure PostBuildHookType
    111 -> pure BuildWaitingType
    other -> JSON.parseFail ("invalid activity result type: " <> show other)

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

data InternalJson = Stop StopAction | Start StartAction | Result ResultAction | Message MessageAction
  deriving stock (Show, Eq)

instance JSON.FromJSON InternalJson where
  parseJSON = JSON.withObject "nix internal-json object" $ \object -> do
    action <- JSON.parseField object "action"
    action |> JSON.withText "nix internal-json action" \actionType ->
      JSON.prependFailure ("While parsing an action of type " <> toString actionType <> ": ") $ case actionType of
        "start" -> parseStartAction object <|>> Start
        "stop" -> parseStopAction object <|>> Stop
        "result" -> parseResultAction object <|>> Result
        "msg" -> parseMessageAction object <|>> Message
        other -> JSON.parseFail ("unknown action type: " <> toString other)

parseMessageAction :: JSON.Object -> JSON.Parser MessageAction
parseMessageAction object = do
  level <- JSON.parseField object "level"
  message <- JSON.parseField object "msg"
  line <- JSON.parseFieldMaybe object "line"
  column <- JSON.parseFieldMaybe object "column"
  file <- JSON.parseFieldMaybe object "file"
  pure MkMessageAction{..}

oneField :: (JSON.FromJSON b, Show b) => JSON.Object -> JSON.Parser b
oneField object = do
  fields <- JSON.parseField object "fields"
  case fields of
    [field] -> pure field
    other -> JSON.parseFail $ "was expecting one field but got: " <> show other

twoFields :: (JSON.FromJSON a, JSON.FromJSON b) => JSON.Object -> JSON.Parser (a, b)
twoFields object = do
  fields <- JSON.parseField object "fields"
  case fields of
    [field1, field2] -> do
      a <- JSON.parseJSON field1
      b <- JSON.parseJSON field2
      pure (a, b)
    other -> JSON.parseFail $ "was expecting two fields but got: " <> show other

threeFields :: (JSON.FromJSON a, JSON.FromJSON b, JSON.FromJSON c) => JSON.Object -> JSON.Parser (a, b, c)
threeFields object = do
  fields <- JSON.parseField object "fields"
  case fields of
    [field1, field2, field3] -> do
      a <- JSON.parseJSON field1
      b <- JSON.parseJSON field2
      c <- JSON.parseJSON field3
      pure (a, b, c)
    other -> JSON.parseFail $ "was expecting two fields but got: " <> show other

fourFields :: (JSON.FromJSON a, JSON.FromJSON b, JSON.FromJSON c, JSON.FromJSON d) => JSON.Object -> JSON.Parser (a, b, c, d)
fourFields object = do
  fields <- JSON.parseField object "fields"
  case fields of
    [field1, field2, field3, field4] -> do
      a <- JSON.parseJSON field1
      b <- JSON.parseJSON field2
      c <- JSON.parseJSON field3
      d <- JSON.parseJSON field4
      pure (a, b, c, d)
    other -> JSON.parseFail $ "was expecting four fields but got: " <> show other

parseResultAction :: JSON.Object -> JSON.Parser ResultAction
parseResultAction object = do
  idField <- JSON.parseField object "id"
  type' :: Int <- JSON.parseField object "type"
  result <- case type' of
    100 -> twoFields object <|>> uncurry FileLinked
    101 -> oneField object <|>> BuildLogLine
    102 -> oneField object <|>> UntrustedPath
    103 -> oneField object <|>> CorruptedPath
    104 -> oneField object <|>> SetPhase
    105 -> fourFields object <|>> \(done, expected, running, failed) -> Progress (MkActivityProgress{..})
    106 -> twoFields object <|>> uncurry SetExpected
    107 -> oneField object <|>> PostBuildLogLine
    other -> JSON.parseFail ("invalid activity result type: " <> show other)
  pure MkResultAction{id = idField, result}

parseStopAction :: JSON.Object -> JSON.Parser StopAction
parseStopAction object = JSON.parseField object "id" <|>> MkStopAction

parseStartAction :: JSON.Object -> JSON.Parser StartAction
parseStartAction object = do
  idField <- JSON.parseField object "id"
  text <- JSON.parseField object "text"
  level <- JSON.parseField object "level"
  activityType <- JSON.parseField object "type"
  activity <- JSON.prependFailure ("While parsing a start activity of " <> show activityType <> ": ") case activityType of
    UnknownType -> pure Unknown
    CopyPathType -> threeFields object <|>> \(path, from, to) -> CopyPath path from to
    FileTransferType -> oneField object <|>> FileTransfer
    RealiseType -> pure Realise
    CopyPathsType -> pure CopyPaths
    BuildsType -> pure Builds
    BuildType -> fourFields object <|>> \(path, host, currentRound, noOfRounds) -> Build path host currentRound noOfRounds
    OptimiseStoreType -> pure OptimiseStore
    VerifyPathsType -> pure VerifyPaths
    SubstituteType -> twoFields object <|>> uncurry Substitute
    QueryPathInfoType -> twoFields object <|>> uncurry QueryPathInfo
    PostBuildHookType -> oneField object <|>> PostBuildHook
    BuildWaitingType -> pure BuildWaiting
  pure MkStartAction{id = idField, text, activity, level}
