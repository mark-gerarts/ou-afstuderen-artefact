module App.Client (ApiError, TaskResponse(..), getInitialTask, interact, reset) where

import Prelude
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import App.Task (Input, Task)
import Data.Argonaut.Core (Json)
import Data.Argonaut (class DecodeJson, JsonDecodeError, decodeJson, encodeJson, (.:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

data ApiError
  = RequestError AX.Error
  | JsonError JsonDecodeError

data TaskResponse
  = TaskResponse Task (Array Input)

instance decodeJsonTaskResponse :: DecodeJson TaskResponse where
  decodeJson json = do
    obj <- decodeJson json
    task <- obj .: "task"
    inputs <- obj .: "inputs"
    pure $ TaskResponse task inputs

instance showApiError :: Show ApiError where
  show (RequestError err) = AX.printError err
  show (JsonError err) = show err

baseUri :: String
baseUri = "/"

endpoint :: String -> String
endpoint s = baseUri <> s

getInitialTask :: Aff (Either ApiError TaskResponse)
getInitialTask = handleTaskResponse <$> (AX.get AXRF.json $ endpoint "initial-task")

interact :: Input -> Aff (Either ApiError TaskResponse)
interact input =
  let
    jsonBody = Just $ AXRB.json (encodeJson input)
  in
    handleTaskResponse <$> (AX.post AXRF.json (endpoint "interact") jsonBody)

reset :: Aff (Either ApiError TaskResponse)
reset = handleTaskResponse <$> (AX.get AXRF.json $ endpoint "reset")

handleTaskResponse :: Either AX.Error (AX.Response Json) -> Either ApiError TaskResponse
handleTaskResponse r = case r of
  Left err -> Left (RequestError err)
  Right response -> case decodeJson response.body of
    Left err -> Left (JsonError err)
    Right task -> Right task
