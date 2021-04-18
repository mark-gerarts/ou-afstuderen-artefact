module App.Task where

import Prelude
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , encodeJson
  , isBoolean
  , isNumber
  , jsonEmptyObject
  , jsonNull
  , (.!=)
  , (.:)
  , (.:?)
  , (:=)
  , (~>)
  )
import Data.Argonaut.Decode.Error as JsonDecodeError
import Data.Either (Either(..))

data Task
  = Edit Name Editor
  | Pair Task Task
  | Step Task

instance decodeJsonTask :: DecodeJson Task where
  decodeJson json = do
    obj <- decodeJson json
    taskType <- obj .: "type"
    case taskType of
      "edit" -> do
        editor <- obj .: "editor" >>= decodeJson
        name <- obj .:? "name" .!= Unnamed
        pure $ Edit name editor
      "pair" -> do
        t1 <- obj .: "t1" >>= decodeJson
        t2 <- obj .: "t2" >>= decodeJson
        pure $ Pair t1 t2
      "step" -> do
        task <- obj .: "task" >>= decodeJson
        pure $ Step task
      _ -> Left (JsonDecodeError.UnexpectedValue (encodeJson taskType))

instance showTask :: Show Task where
  show (Edit name editor) = "Edit [" <> show name <> "] [" <> show editor <> "]"
  show (Pair t1 t2) = "Pair [" <> show t1 <> "] [" <> show t2 <> "]"
  show (Step t) = "Step [" <> show t <> "]"

data Editor
  = Update Value

instance showEditor :: Show Editor where
  show (Update x) = show x

instance decodeJsonEditor :: DecodeJson Editor where
  decodeJson json = do
    obj <- decodeJson json
    editorType <- obj .: "type"
    case editorType of
      "update" -> do
        value <- obj .: "value"
        pure $ Update value
      _ -> Left (JsonDecodeError.UnexpectedValue json)

data Name
  = Named Int
  | Unnamed

derive instance eqName :: Eq Name

instance showName :: Show Name where
  show (Named id) = "Named " <> show id
  show Unnamed = "Unnamed"

instance decodeJsonName :: DecodeJson Name where
  decodeJson json = do
    int <- decodeJson json
    pure $ Named int

instance encodeJsonName :: EncodeJson Name where
  encodeJson name = case name of
    Named id -> encodeJson id
    Unnamed -> jsonNull

data Value
  = Int Int
  | String String
  | Boolean Boolean

instance showValue :: Show Value where
  show (Int int) = show int
  show (String string) = string
  show (Boolean boolean) = show boolean

instance decodeJsonValue :: DecodeJson Value where
  decodeJson json = do
    value <- decodeJson json
    fromValue value
    where
    fromValue v
      | isBoolean v = Boolean <$> decodeJson v
      | isNumber v = Int <$> decodeJson v
      | otherwise = String <$> decodeJson v

instance encodeJsonValue :: EncodeJson Value where
  encodeJson (String string) = encodeJson string
  encodeJson (Int int) = encodeJson int
  encodeJson (Boolean bool) = encodeJson bool

data Input
  = Input Name Value

instance showInput :: Show Input where
  show (Input id x) = "Input [" <> show x <> "] [" <> show id <> "]"

instance encodeInput :: EncodeJson Input where
  encodeJson (Input name value) =
    "value" := value
      ~> "name"
      := name
      ~> jsonEmptyObject

-- @todo: rework the frontend so this updating is no longer necessary. Input
-- should just be sent to the backend straight away.
updateTask :: Name -> Value -> Task -> Task
updateTask name newValue task@(Edit name' editor)
  | name == name' = Edit name (setValue editor newValue)
  | otherwise = task

updateTask id newValue (Pair t1 t2) =
  Pair
    (updateTask id newValue t1)
    (updateTask id newValue t2)

updateTask id newValue (Step t) = Step (updateTask id newValue t)

setValue :: Editor -> Value -> Editor
setValue (Update _) = Update
