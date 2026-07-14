module NOM.Parser.JSON (parseJSONLine) where

import Data.ByteString qualified as ByteString
import Data.Hermes qualified as JSON
import Data.Hermes.Decoder (listOfInt)
import NOM.Builds (parseDerivation, parseHost, parseStorePath)
import NOM.Error (NOMError (..))
import NOM.NixMessage.JSON (Activity (..), ActivityId (..), ActivityProgress (..), ActivityResult (..), ActivityType (..), MessageAction (..), NixJSONMessage (..), ResultAction (..), StartAction (..), StopAction (..), Verbosity (..))
import Relude hiding (one)

parseJSONLine :: JSON.HermesEnv -> ByteString -> NixJSONMessage
parseJSONLine env input = maybe (Plain input) on_json (ByteString.stripPrefix "@nix " input)
 where
  on_json raw_json = either translate_hermes_error_to_nom_error id $ JSON.parseByteString env parseAction raw_json
   where
    translate_hermes_error_to_nom_error :: JSON.HermesException -> NixJSONMessage
    translate_hermes_error_to_nom_error json_error =
      ParseError $ ParseNixJSONMessageError (show json_error) raw_json

parseVerbosity :: JSON.Decoder Verbosity
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

parseActivityType :: (MonadFail m) => Int -> m ActivityType
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
  112 -> pure FetchTreeType
  other -> fail ("invalid activity type: " <> show other)

parseAction :: JSON.Decoder NixJSONMessage
parseAction = JSON.object $ do
  action <- JSON.atKey "action" JSON.text
  ( \case
      "start" -> Start <$> parseStartAction
      "stop" -> Stop <$> parseStopAction
      "result" -> Result <$> parseResultAction
      "msg" -> Message <$> parseMessageAction
      other -> fail ("unknown action type: " <> toString other)
    )
    action

parseMessageAction :: JSON.FieldsDecoder MessageAction
parseMessageAction = do
  level <- JSON.atKey "level" parseVerbosity
  message <- JSON.atKey "msg" JSON.text
  pure MkMessageAction{..}

textFields :: JSON.FieldsDecoder [Text]
textFields = JSON.atKey "fields" (JSON.list JSON.text)

textOrNumFields :: JSON.FieldsDecoder [Either Text Int]
textOrNumFields = JSON.atKey "fields" $ JSON.list (Left <$> JSON.text <|> Right <$> JSON.int)

intFields :: JSON.FieldsDecoder [Int]
intFields = JSON.atKey "fields" listOfInt

one :: (MonadFail m) => m [b] -> m b
one listdec = do
  fields <- listdec
  case fields of
    [field] -> pure field
    _ -> fail "expected one field"

two :: (MonadFail m) => m [b] -> m (b, b)
two listdec = do
  fields <- listdec
  case fields of
    [field1, field2] -> pure (field1, field2)
    _ -> fail "expected one field"

three :: (MonadFail m) => m [b] -> m (b, b, b)
three listdec = do
  fields <- listdec
  case fields of
    [field1, field2, field3] -> pure (field1, field2, field3)
    _ -> fail "expected one field"

four :: (MonadFail m) => m [b] -> m (b, b, b, b)
four listdec = do
  fields <- listdec
  case fields of
    [field1, field2, field3, field4] -> pure (field1, field2, field3, field4)
    _ -> fail "expected one field"

parseResultAction :: JSON.FieldsDecoder ResultAction
parseResultAction = do
  idField <- MkId <$> JSON.atKey "id" JSON.uint
  type' <- JSON.atKey "type" JSON.int
  let txt = textFields
  let num = intFields
  result <- case type' of
    100 -> uncurry FileLinked <$> two num
    101 -> BuildLogLine <$> one txt
    102 -> UntrustedPath <$> (one txt >>= parseStorePath)
    103 -> CorruptedPath <$> (one txt >>= parseStorePath)
    104 -> SetPhase <$> one txt
    105 -> (\(done, expected, running, failed) -> Progress (MkActivityProgress{..})) <$> four num
    106 -> do
      (typeNum, number) <- two num
      activityType <- parseActivityType typeNum
      pure $ SetExpected activityType number
    107 -> PostBuildLogLine <$> one txt
    108 -> FetchStatus <$> one txt
    other -> fail ("invalid activity result type: " <> show other)
  pure MkResultAction{id = idField, result}

parseStopAction :: JSON.FieldsDecoder StopAction
parseStopAction = MkStopAction . MkId <$> JSON.atKey "id" JSON.uint

parseStartAction :: JSON.FieldsDecoder StartAction
parseStartAction = do
  idField <- JSON.atKey "id" JSON.uint
  text <- JSON.atKey "text" JSON.text
  level <- JSON.atKey "level" parseVerbosity
  activityType <- JSON.atKey "type" (JSON.withInt parseActivityType)
  let txt = textFields
  activity <- case activityType of
    UnknownType -> pure Unknown
    CopyPathType ->
      three txt >>= \(path, from, to) -> do
        path' <- parseStorePath path
        pure $ CopyPath path' (parseHost from) (parseHost to)
    FileTransferType -> FileTransfer <$> one txt
    RealiseType -> pure Realise
    CopyPathsType -> pure CopyPaths
    BuildsType -> pure Builds
    BuildType ->
      four textOrNumFields >>= \(path, host, _, _) -> do
        path' <- either pure (const $ fail "Got Int expected Text") path
        path'' <- parseDerivation path'
        host' <- either pure (const $ fail "Got Int expected Text") host
        pure $ Build path'' (parseHost host')
    OptimiseStoreType -> pure OptimiseStore
    VerifyPathsType -> pure VerifyPaths
    SubstituteType ->
      two txt >>= \(path, host) -> do
        path' <- parseStorePath path
        pure $ Substitute path' (parseHost host)
    QueryPathInfoType ->
      two txt >>= \(path, host) -> do
        path' <- parseStorePath path
        pure $ QueryPathInfo path' (parseHost host)
    PostBuildHookType -> PostBuildHook <$> (one txt >>= parseDerivation)
    BuildWaitingType -> pure BuildWaiting
    FetchTreeType -> pure FetchTree
  pure MkStartAction{id = MkId idField, text, activity, level}
