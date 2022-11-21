module NOM.Parser.JSON (parseJSONLine) where

import Control.Exception (try)
import Data.ByteString qualified as ByteString
import Data.Hermes qualified as JSON
import Data.Hermes.Decoder (listOfInt)
import NOM.Builds (parseDerivation, parseHost, parseStorePath)
import NOM.Error (NOMError (..))
import NOM.NixMessage.JSON (Activity (..), ActivityId (..), ActivityProgress (..), ActivityResult (..), ActivityType (..), MessageAction (..), NixJSONMessage (..), ResultAction (..), StartAction (..), StopAction (..), Verbosity (..))
import Relude hiding (one)
import System.IO.Unsafe qualified as Unsafe

parseJSONLine :: JSON.HermesEnv -> ByteString -> NixJSONMessage
parseJSONLine env input = maybe (Plain input) on_json (ByteString.stripPrefix "@nix " input)
 where
  on_json raw_json = either translate_hermes_error_to_nom_error id $ parseWithEnv env parseAction raw_json
   where
    translate_hermes_error_to_nom_error :: JSON.HermesException -> NixJSONMessage
    translate_hermes_error_to_nom_error json_error =
      ParseError $ ParseNixJSONMessageError (show json_error) raw_json

parseWithEnv :: Exception e => JSON.HermesEnv -> (JSON.Value -> JSON.Decoder a) -> ByteString -> Either e a
parseWithEnv env parser raw_json = Unsafe.unsafePerformIO . try $ JSON.withInputBuffer raw_json $ \input ->
  flip runReaderT env . JSON.runDecoder $ JSON.withDocumentValue parser input

parseVerbosity :: JSON.Value -> JSON.Decoder Verbosity
parseVerbosity = JSON.withInt \case
  0 -> pure Error
  1 -> pure Warn
  2 -> pure Notice
  3 -> pure Info
  4 -> pure Talkative
  5 -> pure Chatty
  6 -> pure Debug
  7 -> pure Vomit
  other -> fail ("invalid verbosity level:" <> show other)

parseActivityType :: MonadFail m => Int -> m ActivityType
parseActivityType = \case
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
  other -> fail ("invalid activity result type: " <> show other)

parseAction :: JSON.Value -> JSON.Decoder NixJSONMessage
parseAction = JSON.withObject $ \object -> do
  action <- JSON.atKey "action" JSON.text object
  ( \case
      "start" -> Start <$> parseStartAction object
      "stop" -> Stop <$> parseStopAction object
      "result" -> Result <$> parseResultAction object
      "msg" -> Message <$> parseMessageAction object
      other -> fail ("unknown action type: " <> toString other)
    )
    action

parseMessageAction :: JSON.Object -> JSON.Decoder MessageAction
parseMessageAction object = do
  level <- JSON.atKey "level" parseVerbosity object
  message <- JSON.atKey "msg" JSON.text object
  pure MkMessageAction{..}

textFields :: JSON.Object -> JSON.Decoder [Text]
textFields = JSON.atKey "fields" (JSON.list JSON.text)

textOrNumFields :: JSON.Object -> JSON.Decoder [Either Text Int]
textOrNumFields = JSON.atKey "fields" (JSON.list \val -> (Left <$> JSON.text val) <|> (Right <$> JSON.int val))

intFields :: JSON.Object -> JSON.Decoder [Int]
intFields = JSON.atKey "fields" listOfInt

one :: MonadFail m => m [b] -> m b
one listdec = do
  fields <- listdec
  case fields of
    [field] -> pure field
    _ -> fail "expected one field"

two :: MonadFail m => m [b] -> m (b, b)
two listdec = do
  fields <- listdec
  case fields of
    [field1, field2] -> pure (field1, field2)
    _ -> fail "expected one field"

three :: MonadFail m => m [b] -> m (b, b, b)
three listdec = do
  fields <- listdec
  case fields of
    [field1, field2, field3] -> pure (field1, field2, field3)
    _ -> fail "expected one field"

four :: MonadFail m => m [b] -> m (b, b, b, b)
four listdec = do
  fields <- listdec
  case fields of
    [field1, field2, field3, field4] -> pure (field1, field2, field3, field4)
    _ -> fail "expected one field"

unwrap :: MonadFail m => Maybe a -> m a
unwrap = maybe (fail "empty value") pure

parseResultAction :: JSON.Object -> JSON.Decoder ResultAction
parseResultAction object = do
  idField <- MkId <$> JSON.atKey "id" JSON.int object
  type' <- JSON.atKey "type" JSON.int object
  let txt = textFields object
  let num = intFields object
  result <- case type' of
    100 -> uncurry FileLinked <$> two num
    101 -> BuildLogLine <$> one txt
    102 -> UntrustedPath <$> (one txt >>= unwrap . parseStorePath)
    103 -> CorruptedPath <$> (one txt >>= unwrap . parseStorePath)
    104 -> SetPhase <$> one txt
    105 -> (\(done, expected, running, failed) -> Progress (MkActivityProgress{..})) <$> four num
    106 -> do
      (typeNum, number) <- two num
      activityType <- parseActivityType typeNum
      pure $ SetExpected activityType number
    107 -> PostBuildLogLine <$> one txt
    other -> fail ("invalid activity result type: " <> show other)
  pure MkResultAction{id = idField, result}

parseStopAction :: JSON.Object -> JSON.Decoder StopAction
parseStopAction object = MkStopAction . MkId <$> JSON.atKey "id" JSON.int object

parseStartAction :: JSON.Object -> JSON.Decoder StartAction
parseStartAction object = do
  idField <- JSON.atKey "id" JSON.int object
  text <- JSON.atKey "text" JSON.text object
  level <- JSON.atKey "level" parseVerbosity object
  activityType <- JSON.atKey "type" (JSON.withInt parseActivityType) object
  let txt = textFields object
  activity <- case activityType of
    UnknownType -> pure Unknown
    CopyPathType ->
      three txt >>= \(path, from, to) -> do
        path' <- unwrap (parseStorePath path)
        pure $ CopyPath path' (parseHost from) (parseHost to)
    FileTransferType -> FileTransfer <$> one txt
    RealiseType -> pure Realise
    CopyPathsType -> pure CopyPaths
    BuildsType -> pure Builds
    BuildType ->
      four (textOrNumFields object) >>= \(path, host, _, _) -> do
        path' <- unwrap (either Just (const Nothing) path)
        path'' <- unwrap (parseDerivation path')
        host' <- unwrap (either Just (const Nothing) host)
        pure $ Build path'' (parseHost host')
    OptimiseStoreType -> pure OptimiseStore
    VerifyPathsType -> pure VerifyPaths
    SubstituteType ->
      two txt >>= \(path, host) -> do
        path' <- unwrap (parseStorePath path)
        pure $ Substitute path' (parseHost host)
    QueryPathInfoType ->
      two txt >>= \(path, host) -> do
        path' <- unwrap (parseStorePath path)
        pure $ QueryPathInfo path' (parseHost host)
    PostBuildHookType -> PostBuildHook <$> (one txt >>= unwrap . parseDerivation)
    BuildWaitingType -> pure BuildWaiting
  pure MkStartAction{id = MkId idField, text, activity, level}
