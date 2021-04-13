module App.Task where

import Prelude
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , decodeJson
  , encodeJson
  , isBoolean
  , isNumber
  , jsonEmptyObject
  , (.:)
  , (:=)
  , (~>)
  )
import Data.Either (Either(..))

type Id
  = Int

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
        a <- obj .: "a" >>= decodeJson
        b <- obj .: "b" >>= decodeJson
        pure $ Pair a b
      _ -> Left (UnexpectedValue json)

instance showTask :: Show Task where
  show (Update id x) = "Update [" <> show x <> "] [" <> show id <> "]"
  show (Pair t1 t2) = "Pair [" <> show t1 <> "] [" <> show t2 <> "]"

data Input
  = Input Id Value

instance showInput :: Show Input where
  show (Input id x) = "Input [" <> show x <> "] [" <> show id <> "]"

instance encodeInput :: EncodeJson Input where
  encodeJson (Input id value) =
    "value" := value
      ~> "id"
      := id
      ~> jsonEmptyObject

updateTask :: Id -> Value -> Task -> Task
updateTask id newValue task@(Update taskId value)
  | id == taskId = Update id newValue
  | otherwise = task

updateTask id newValue (Pair t1 t2) =
  Pair
    (updateTask id newValue t1)
    (updateTask id newValue t2)
