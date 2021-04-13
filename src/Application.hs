{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Application (application) where

import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromJust)
import Network.Wai (Middleware)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Network.Wai.Middleware.Cors
import Servant
import Task (Input (..), Task (..), TaskValue)
import WaiAppStatic.Types (unsafeToPiece)

type TaskAPI a =
  "initial-task" :> Get '[JSON] (Task a)
    :<|> "interact" :> ReqBody '[JSON] Input :> Post '[JSON] (Task a)

type StaticAPI = Raw

type API a = TaskAPI a :<|> StaticAPI

interact :: Input -> Task a -> Task a
interact (Input id value) task@(Update taskId _)
  | id == taskId = Update (taskId + 3) (fromJSONValue' value)
  | otherwise = task
interact input (Pair a b) = Pair (interact input a) (interact input b)

-- No error handling for now, a wrong input type results in a runtime error.
fromJSONValue' :: TaskValue a => Value -> a
fromJSONValue' = fromJust << parseMaybe parseJSON

server :: Task a -> Server (API a)
server task = taskServer task :<|> staticServer
  where
    taskServer :: Task a -> Server (TaskAPI a)
    taskServer task' =
      return task'
        :<|> (\input -> return <| interact input task')

    staticServer :: Server StaticAPI
    staticServer =
      -- serveDirectoryWebAbb does not automatically use index.html when
      -- visiting /, so we use the default webApp settings, but override the
      -- fallback.
      let defaultSettings = defaultWebAppSettings "frontend/prod"
          indexFallback = map unsafeToPiece ["index.html"]
       in serveDirectoryWith <| defaultSettings {ssIndices = indexFallback}

apiProxy :: Task a -> Proxy (API a)
apiProxy _ = Proxy

application :: Application
application = corsPolicy <| serve (apiProxy task) (server task)
  where
    -- Task is now hardcoded here, but can serve as the input to Application in
    -- a later stage.
    task = initialTask

corsPolicy :: Middleware
corsPolicy = cors (const <| Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "OPTIONS"],
          corsOrigins = Just (["http://localhost:3001"], True),
          corsRequestHeaders = ["authorization", "content-type"]
        }

initialTask :: Task (Text, (Int, Bool))
initialTask = Pair textUpdate rightPair
  where
    textUpdate :: Task Text
    textUpdate = Update 1 "Edit me!!"

    intUpdate :: Task Int
    intUpdate = Update 2 123

    boolUpdate :: Task Bool
    boolUpdate = Update 3 True

    rightPair :: Task (Int, Bool)
    rightPair = Pair intUpdate boolUpdate
