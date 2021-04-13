module App.Client (ApiError, getCurrentTask, interact) where

import Prelude
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import App.Task (Input, Task)
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

data ApiError
  = RequestError AX.Error
  | JsonError JsonDecodeError

instance showApiError :: Show ApiError where
  show (RequestError err) = AX.printError err
  show (JsonError err) = show err

baseUri :: String
baseUri = "http://localhost:3000/"

endpoint :: String -> String
endpoint s = baseUri <> s

getCurrentTask :: Aff (Either ApiError Task)
getCurrentTask = do
  r <- AX.get AXRF.json $ endpoint "current-task"
  case r of
    Left err -> pure $ Left (RequestError err)
    Right response -> case decodeJson response.body of
      Left err -> pure $ Left (JsonError err)
      Right task -> pure $ Right task

interact :: Input -> Aff (Either ApiError Task)
interact input = do
  r <- AX.post AXRF.json (endpoint "interact") $ Just (AXRB.json (encodeJson input))
  case r of
    Left err -> pure $ Left (RequestError err)
    Right response -> case decodeJson response.body of
      Left err -> pure $ Left (JsonError err)
      Right task -> pure $ Right task
