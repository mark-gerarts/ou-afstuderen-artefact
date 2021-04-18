{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Application (application, initialTask, State (..)) where

import Communication (Envelope (..), Input (..))
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value)
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromJust)
import Network.Wai (Middleware)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Network.Wai.Middleware.Cors
import Servant
import Task (Task (Pair), update)
import Task.Run (interact)
import WaiAppStatic.Types (unsafeToPiece)

data State h t = State
  { currentTask :: TVar (Task h t)
  }

type TaskAPI =
  "initial-task" :> Get '[JSON] Envelope
    :<|> "interact" :> ReqBody '[JSON] Input :> Post '[JSON] Envelope

type StaticAPI = Raw

type API = TaskAPI :<|> StaticAPI

type AppM h t = ReaderT (State h t) Handler

-- No error handling for now, a wrong input type results in a runtime error.
--fromJSONValue' :: TaskValue a => Value -> a
--fromJSONValue' = fromJust << parseMaybe parseJSON

-- We define this method for now until everything else compiles - then we can
-- see how we are actually going to use TopHat's interact.
interact' :: Input -> Task h a -> Task h a
interact' = undefined

server :: ToJSON t => State h t -> ServerT API (AppM h t)
server _ = taskServer :<|> staticServer
  where
    taskServer :: ToJSON t => ServerT TaskAPI (AppM h t)
    taskServer = do
      initialTaskHandler
        :<|> interactHandler

    initialTaskHandler :: ToJSON t => AppM h t Envelope
    initialTaskHandler = do
      State {currentTask = t} <- ask
      liftIO <| do
        t' <- atomically <| readTVar t
        return (Envelope t')

    interactHandler :: ToJSON t => Input -> AppM h t Envelope
    interactHandler input = do
      State {currentTask = t} <- ask
      liftIO <| atomically <| do
        t' <- readTVar t
        let newTask = interact' input t'
        writeTVar t newTask
        return (Envelope newTask)

    staticServer :: ServerT StaticAPI (AppM h t)
    staticServer =
      -- serveDirectoryWebAbb does not automatically use index.html when
      -- visiting /, so we use the default webApp settings, but override the
      -- fallback.
      let defaultSettings = defaultWebAppSettings "frontend/prod"
          indexFallback = map unsafeToPiece ["index.html"]
       in serveDirectoryWith <| defaultSettings {ssIndices = indexFallback}

apiProxy :: State h t -> Proxy API
apiProxy _ = Proxy

application :: ToJSON t => State h t -> Application
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

initialTask :: Task h Int
initialTask = do
  x <- update 42
  update (x + 1)
