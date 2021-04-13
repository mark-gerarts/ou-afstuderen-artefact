{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Application (application, initialTask, State (..)) where

import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromJust)
import Network.Wai (Middleware)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Network.Wai.Middleware.Cors
import Servant
import Task (Input (..), Task (..), TaskValue)
import WaiAppStatic.Types (unsafeToPiece)

data State a = State
  { currentTask :: TVar (Task a)
  }

type TaskAPI a =
  "initial-task" :> Get '[JSON] (Task a)
    :<|> "interact" :> ReqBody '[JSON] Input :> Post '[JSON] (Task a)

type StaticAPI = Raw

type API a = TaskAPI a :<|> StaticAPI

type AppM a = ReaderT (State a) Handler

interact :: Input -> Task a -> Task a
interact (Input id value) task@(Update taskId _)
  | id == taskId = Update (taskId + 3) (fromJSONValue' value)
  | otherwise = task
interact input (Pair a b) = Pair (interact input a) (interact input b)

-- No error handling for now, a wrong input type results in a runtime error.
fromJSONValue' :: TaskValue a => Value -> a
fromJSONValue' = fromJust << parseMaybe parseJSON

server :: State a -> ServerT (API a) (AppM a)
server _ = taskServer :<|> staticServer
  where
    taskServer :: ServerT (TaskAPI a) (AppM a)
    taskServer =
      initialTaskHandler
        :<|> interactHandler

    initialTaskHandler :: AppM a (Task a)
    initialTaskHandler = do
      State {currentTask = t} <- ask
      liftIO <| atomically <| readTVar t

    interactHandler :: Input -> AppM a (Task a)
    interactHandler input = do
      State {currentTask = t} <- ask
      liftIO <| atomically <| do
        t' <- readTVar t
        let newTask = interact input t'
        writeTVar t newTask
        return newTask

    staticServer :: ServerT StaticAPI (AppM a)
    staticServer =
      -- serveDirectoryWebAbb does not automatically use index.html when
      -- visiting /, so we use the default webApp settings, but override the
      -- fallback.
      let defaultSettings = defaultWebAppSettings "frontend/prod"
          indexFallback = map unsafeToPiece ["index.html"]
       in serveDirectoryWith <| defaultSettings {ssIndices = indexFallback}

apiProxy :: State a -> Proxy (API a)
apiProxy _ = Proxy

application :: State a -> Application
application s =
  corsPolicy
    <| serve (apiProxy s)
    <| hoistServer (apiProxy s) (nt s) (server s)
  where
    nt s' x = runReaderT x s'

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
    textUpdate = Update 1 "Edit me!"

    intUpdate :: Task Int
    intUpdate = Update 2 123

    boolUpdate :: Task Bool
    boolUpdate = Update 3 True

    rightPair :: Task (Int, Bool)
    rightPair = Pair intUpdate boolUpdate
