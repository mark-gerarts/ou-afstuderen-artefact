module App.Task where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)

type Id
  = Int

data Value
  = Value String ValueType

instance showValue :: Show Value where
  show (Value value valueType) = value <> " (type: " <> show valueType <> ")"

instance decodeJsonValue :: DecodeJson Value where
  decodeJson json = do
    obj <- decodeJson json
    value <- (obj .: "value" <|> fromInt obj "value")
    valueType <- obj .: "type"
    pure $ Value value valueType
    where
    fromInt obj x = do
      int :: Int <- obj .: x
      pure $ show int

decodeValue :: ValueType -> Object Json -> Either JsonDecodeError String
decodeValue Int obj = do
  value :: Int <- obj .: "value"
  pure $ show value

decodeValue String obj = do
  value <- obj .: "value"
  pure value

data ValueType
  = Int
  | String

instance showValueType :: Show ValueType where
  show Int = "Int"
  show String = "String"

instance decodeJsonValueType :: DecodeJson ValueType where
  decodeJson json = do
    string <- decodeJson json
    let
      valueType = case string of
        "Int" -> Just Int
        "Text" -> Just String
        _ -> Nothing
    note (TypeMismatch "ValueType") valueType

data Task
  = Update Id Value

instance decodeJsonTask :: DecodeJson Task where
  decodeJson json = do
    obj <- decodeJson json
    value <- obj .: "value"
    id <- obj .: "id"
    pure $ Update id value

instance showTask :: Show Task where
  show (Update id x) = "Update [" <> show x <> "] [" <> show id <> "]"

data Input
  = Input Id String

instance showInput :: Show Input where
  show (Input id x) = "Input [" <> x <> "] [" <> show id <> "]"

instance encodeInput :: EncodeJson Input where
  encodeJson (Input id value) =
    "value" := value
      ~> "id"
      := id
      ~> jsonEmptyObject

updateTask :: String -> Task -> Task
updateTask newValue (Update id (Value oldValue t)) = Update id (Value newValue t)
