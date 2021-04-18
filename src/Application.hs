{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Application (application, initialTask, State (..)) where

import Communication (JsonInput (..), JsonTask (..))
import Data.Aeson (ToJSON)
import Network.Wai (Middleware)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Network.Wai.Middleware.Cors
import Polysemy
import Polysemy.Log
import Polysemy.Mutate
import Polysemy.Supply
import Servant
import Task (RealWorld, Task (Pair), update, view, (<?>), (>>?))
import Task.Input (Concrete (..), Input (..))
import Task.Run (NotApplicable, Steps, initialise, interact)
import WaiAppStatic.Types (unsafeToPiece)

data State h t = State
  { currentTask :: TVar (Task RealWorld t),
    initialised :: Bool
  }

type TaskAPI =
  "initial-task" :> Get '[JSON] JsonTask
    :<|> "interact" :> ReqBody '[JSON] JsonInput :> Post '[JSON] JsonTask

type StaticAPI = Raw

type API = TaskAPI :<|> StaticAPI

type AppM h t = ReaderT (State h t) Handler

server :: ToJSON t => State h t -> ServerT API (AppM h t)
server _ = taskServer :<|> staticServer
  where
    taskServer :: ToJSON t => ServerT TaskAPI (AppM h t)
    taskServer = do
      initialTaskHandler
        :<|> interactHandler

    initialTaskHandler :: ToJSON t => AppM h t JsonTask
    initialTaskHandler = do
      State {currentTask = t, initialised = i} <- ask
      t' <- readTVarIO t
      if i
        then return (JsonTask t')
        else
          liftIO <| do
            initialisedTask <- initialiseIO t'
            atomically <| writeTVar t initialisedTask
            return (JsonTask initialisedTask)

    interactHandler :: ToJSON t => JsonInput -> AppM h t JsonTask
    interactHandler (JsonInput input) = do
      State {currentTask = t} <- ask
      liftIO <| do
        t' <- readTVarIO t
        newTask <- interactIO input t'
        atomically <| writeTVar t newTask
        return (JsonTask newTask)

    staticServer :: ServerT StaticAPI (AppM h t)
    staticServer =
      -- serveDirectoryWebAbb does not automatically use index.html when
      -- visiting /, so we use the default webApp settings, but override the
      -- fallback.
      let defaultSettings = defaultWebAppSettings "frontend/prod"
          indexFallback = map unsafeToPiece ["index.html"]
       in serveDirectoryWith <| defaultSettings {ssIndices = indexFallback}

interactIO :: Input Concrete -> Task RealWorld a -> IO (Task RealWorld a)
interactIO i t =
  withIO <| do
    interact i t

initialiseIO :: Task RealWorld a -> IO (Task RealWorld a)
initialiseIO t =
  withIO <| do
    initialise t

withIO ::
  Sem '[Write RealWorld, Supply Nat, Read RealWorld, Log NotApplicable, Log Steps, Alloc RealWorld, Embed IO] a ->
  IO a
withIO =
  writeToIO
    >> supplyToIO
    >> readToIO
    >> logToIO @NotApplicable
    >> logToIO @Steps
    >> allocToIO
    >> runM

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
  update (x + 1) <?> fail
