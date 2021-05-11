{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Application (application, State (..)) where

import Communication (JsonInput (..), JsonTask (..))
import Data.Aeson (ToJSON)
import Network.Wai (Middleware)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Network.Wai.Middleware.Cors
import Polysemy (Embed, Sem, runM)
import Polysemy.Log (Log, logToIO)
import Polysemy.Mutate
  ( Alloc,
    Read,
    Write,
    allocToIO,
    readToIO,
    writeToIO,
  )
import Polysemy.Supply (Supply, supplyToIO)
import Servant
import Task (RealWorld, Task)
import Task.Input (Concrete (..), Dummy, Input (..))
import Task.Observe (inputs)
import Task.Run (NotApplicable, Steps, initialise, interact)
import WaiAppStatic.Types (unsafeToPiece)

data State h t = State
  { currentTask :: TVar (Task RealWorld t),
    initialised :: Bool,
    originalTask :: Task RealWorld t
  }

type TaskAPI =
  "initial-task" :> Get '[JSON] JsonTask
    :<|> "interact" :> ReqBody '[JSON] JsonInput :> Post '[JSON] JsonTask
    :<|> "reset" :> Get '[JSON] JsonTask

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
        :<|> resetHandler

    initialTaskHandler :: ToJSON t => AppM h t JsonTask
    initialTaskHandler = do
      initialisedTask <- getInitialisedTask
      possibleInputs <- liftIO <| inputsIO initialisedTask
      return (JsonTask initialisedTask possibleInputs)
      where
        getInitialisedTask = do
          State {currentTask = t, initialised = i} <- ask
          t' <- readTVarIO t
          if i
            then return t'
            else
              liftIO <| do
                initialisedTask <- initialiseIO t'
                atomically <| writeTVar t initialisedTask
                return initialisedTask

    interactHandler :: ToJSON t => JsonInput -> AppM h t JsonTask
    interactHandler (JsonInput input) = do
      State {currentTask = t} <- ask
      liftIO <| do
        t' <- readTVarIO t
        newTask <- interactIO input t'
        atomically <| writeTVar t newTask
        possibleInputs <- inputsIO newTask
        return (JsonTask newTask possibleInputs)

    resetHandler :: ToJSON t => AppM h t JsonTask
    resetHandler = do
      State {currentTask = t, originalTask = t_o} <- ask
      liftIO <| do
        initialisedTask <- initialiseIO t_o
        atomically <| writeTVar t initialisedTask
        possibleInputs <- inputsIO initialisedTask
        return (JsonTask initialisedTask possibleInputs)

    staticServer :: ServerT StaticAPI (AppM h t)
    staticServer =
      -- serveDirectoryWebAbb does not automatically use index.html when
      -- visiting /, so we use the default webApp settings, but override the
      -- fallback.
      let defaultSettings = defaultWebAppSettings "frontend/prod"
          indexFallback = map unsafeToPiece ["index.html"]
       in serveDirectoryWith <| defaultSettings {ssIndices = indexFallback}

interactIO :: Input Concrete -> Task RealWorld a -> IO (Task RealWorld a)
interactIO i t = withIO <| interact i t

initialiseIO :: Task RealWorld a -> IO (Task RealWorld a)
initialiseIO t = withIO <| initialise t

inputsIO :: Task RealWorld a -> IO (List (Input Dummy))
inputsIO t = withIO <| inputs t

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
          corsOrigins = Just (["http://localhost:3001", "http://localhost:3000"], True),
          corsRequestHeaders = ["authorization", "content-type"]
        }
