{-|
Module      : App.Client
Description : Module for communication with backend
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : ...
Maintainer  : sample@email.com
Stability   : experimental

Module to send requests (initial tasks, interact, reset) to the backend and handle responses.
-}

module App.Client (ApiError, TaskResponse(..), getInitialTask, interact, reset) where

import Prelude
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import App.Task (Input, InputDescription, Task)
import Data.Argonaut (class DecodeJson, JsonDecodeError, decodeJson, encodeJson, (.:))
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Web.DOM.Element (hasAttribute)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (toElement)

data TaskResponse
  = TaskResponse Task (Array InputDescription)

instance decodeJsonTaskResponse :: DecodeJson TaskResponse where
  decodeJson json = do
    obj <- decodeJson json
    task <- obj .: "task"
    inputs <- obj .: "inputs"
    pure $ TaskResponse task inputs

data ApiError
  = RequestError AX.Error
  | JsonError JsonDecodeError

instance showApiError :: Show ApiError where
  show (RequestError err) = AX.printError err
  show (JsonError err) = show err

-- If the frontend code is served directly from the backend, we can simply use
-- relative paths to access the API. However, when developing we use Parcel to
-- spin up a development server on port 3001, next to the backend server on port
-- 3000.
--
-- To differentiate between the two cases, the body element will contain a
-- "dev-mode" property only when served via Parcel.
getBaseUri :: Aff String
getBaseUri = do
  body <- HA.selectElement $ QuerySelector "body"
  case body of
    Nothing -> pure defaultUri
    Just body' -> do
      devMode <- liftEffect $ hasAttribute "dev-mode" (toElement body')
      pure $ if devMode then devUri else defaultUri
  where
  defaultUri = "/"

  devUri = "http://localhost:3000/"

-- function that returns the url of and endpoint.
endpoint :: String -> Aff String
endpoint s = do
  baseUri <- getBaseUri
  pure $ baseUri <> s

-- function that converts Json into TaskResponse (if possible).
handleTaskResponse :: Either AX.Error (AX.Response Json) -> Either ApiError TaskResponse
handleTaskResponse r = case r of
  Left err -> Left (RequestError err)
  Right response -> case decodeJson response.body of
    Left err -> Left (JsonError err)
    Right task -> Right task

-- function that returns the initial task.
getInitialTask :: Aff (Either ApiError TaskResponse)
getInitialTask = do
  url <- endpoint "initial-task"
  handleTaskResponse <$> AX.get AXRF.json url

-- function takes an Input as argument and returns the new task.
interact :: Input -> Aff (Either ApiError TaskResponse)
interact input = do
  url <- endpoint "interact"
  let
    jsonBody = Just $ AXRB.json (encodeJson input)
  handleTaskResponse <$> AX.post AXRF.json url jsonBody

-- function that returns the original task.
reset :: Aff (Either ApiError TaskResponse)
reset = do
  url <- endpoint "reset"
  handleTaskResponse <$> AX.get AXRF.json url