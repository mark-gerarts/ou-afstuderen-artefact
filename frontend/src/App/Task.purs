module App.Task where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Either (Either(..), note)
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
    value <- (obj .: "value" <|> fromInt obj "value" <|> fromBool obj "value")
    valueType <- obj .: "type"
    pure $ Value value valueType
    where
    fromInt obj x = do
      int :: Int <- obj .: x
      pure $ show int

    fromBool obj x = do
      bool <- obj .: x
      pure $ if bool then "True" else "False"

data ValueType
  = Int
  | String
  | Boolean

instance showValueType :: Show ValueType where
  show Int = "Int"
  show String = "String"
  show Boolean = "Bool"

instance decodeJsonValueType :: DecodeJson ValueType where
  decodeJson json = do
    string <- decodeJson json
    let
      valueType = case string of
        "Int" -> Just Int
        "Text" -> Just String
        "Bool" -> Just Boolean
        _ -> Nothing
    note (TypeMismatch "ValueType") valueType

data Task
  = Update Id Value
  | Pair Task Task

instance decodeJsonTask :: DecodeJson Task where
  decodeJson json = do
    obj <- decodeJson json
    taskType <- obj .: "type"
    case taskType of
      "update" -> do
        value <- obj .: "value"
        id <- obj .: "id"
        pure $ Update id value
      "pair" -> do
        --aEnc <- obj .: "a"
        a <- obj .: "a" >>= decodeJson
        b <- obj .: "b" >>= decodeJson
        pure $ Pair a b
      _ -> Left (UnexpectedValue json)

instance showTask :: Show Task where
  show (Update id x) = "Update [" <> show x <> "] [" <> show id <> "]"
  show (Pair t1 t2) = "Pair [" <> show t1 <> "] [" <> show t2 <> "]"

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

updateTask :: Id -> String -> Task -> Task
updateTask id newValue task@(Update taskId (Value oldValue t))
  | id == taskId = Update id (Value newValue t)
  | otherwise = task

updateTask id newValue (Pair t1 t2) = Pair (updateTask id newValue t1) (updateTask id newValue t2)
