module App.Task where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, (.:), (:=), (~>))

type Id
  = Int

data Task
  = Update Id String

instance decodeJsonTask :: DecodeJson Task where
  decodeJson json = do
    obj <- decodeJson json
    value <- obj .: "value"
    id <- obj .: "id"
    pure $ Update id value

instance showTask :: Show Task where
  show (Update id x) = "Update [" <> x <> "] [" <> show id <> "]"

data Input
  = Input Id String

instance showInput :: Show Input where
  show (Input id x) = "Input [" <> x <> "] [" <> show id <> "]"

instance encodeInput :: EncodeJson Input where
  encodeJson (Input id value) =
    "value" := value
      ~> "id"
      := id

updateTask :: String -> Task -> Task
updateTask newValue (Update id value) = Update id newValue
