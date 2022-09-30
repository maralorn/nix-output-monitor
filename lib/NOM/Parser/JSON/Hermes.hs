module NOM.Parser.JSON.Hermes (parseJSON) where

import Relude hiding (one)

import Data.Hermes qualified as JSON
import Data.Hermes.Decoder (listOfInt)
import NOM.Builds (parseDerivation, parseHost, parseStorePath)
import NOM.Error (NOMError (..))
import NOM.NixEvent (NixEvent (JsonMessage))
import NOM.NixEvent.Action (Activity (..), ActivityId (..), ActivityProgress (..), ActivityResult (..), ActivityType (..), MessageAction (..), NixAction (..), ResultAction (..), StartAction (..), StopAction (..), Verbosity (..))
import NOM.Util ((<|>>), (|>))

parseJSON :: ByteString -> NixEvent
parseJSON raw_json = JsonMessage (first translate_aeson_error_to_nom_error json_parse_result)
 where
  json_parse_result = JSON.decodeEither parseAction raw_json
  translate_aeson_error_to_nom_error :: JSON.HermesException -> NOMError
  translate_aeson_error_to_nom_error aeson_error =
    ParseNixActionError (show aeson_error) raw_json

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

parseAction :: JSON.Value -> JSON.Decoder NixAction
parseAction = JSON.withObject $ \object -> do
  action <- JSON.atKey "action" JSON.text object
  action |> \case
    "start" -> parseStartAction object <|>> Start
    "stop" -> parseStopAction object <|>> Stop
    "result" -> parseResultAction object <|>> Result
    "msg" -> parseMessageAction object <|>> Message
    other -> fail ("unknown action type: " <> toString other)

parseMessageAction :: JSON.Object -> JSON.Decoder MessageAction
parseMessageAction object = do
  level <- JSON.atKey "level" parseVerbosity object
  message <- JSON.atKey "msg" JSON.text object
  line <- JSON.atKeyOptional "line" JSON.int object
  column <- JSON.atKeyOptional "column" JSON.int object
  file <- JSON.atKeyOptional "file" JSON.text object
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
    100 -> two num <|>> uncurry FileLinked
    101 -> one txt <|>> BuildLogLine
    102 -> (one txt >>= unwrap . parseStorePath) <|>> UntrustedPath
    103 -> (one txt >>= unwrap . parseStorePath) <|>> CorruptedPath
    104 -> one txt <|>> SetPhase
    105 -> four num <|>> \(done, expected, running, failed) -> Progress (MkActivityProgress{..})
    106 -> do
      (typeNum, number) <- two num
      activityType <- parseActivityType typeNum
      pure $ SetExpected activityType number
    107 -> one txt <|>> PostBuildLogLine
    other -> fail ("invalid activity result type: " <> show other)
  pure MkResultAction{id = idField, result}

parseStopAction :: JSON.Object -> JSON.Decoder StopAction
parseStopAction object = JSON.atKey "id" JSON.int object <|>> MkStopAction . MkId

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
    FileTransferType -> one txt <|>> FileTransfer
    RealiseType -> pure Realise
    CopyPathsType -> pure CopyPaths
    BuildsType -> pure Builds
    BuildType ->
      four (textOrNumFields object) >>= \(path, host, _, _) -> do
        path' <- unwrap (either Just (const Nothing) path)
        path'' <- unwrap (parseDerivation path')
        host' <- unwrap (either Just (const Nothing) host)
        pure $ Build path'' (parseHost host') 0 0
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
    PostBuildHookType -> (one txt >>= unwrap . parseDerivation) <|>> PostBuildHook
    BuildWaitingType -> pure BuildWaiting
  pure MkStartAction{id = MkId idField, text, activity, level}