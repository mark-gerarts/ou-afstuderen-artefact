{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Application
Description : Module to load the application
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : ...
Maintainer  : sample@email.com
Stability   : experimental

Module to load the application: set the server and define the handlers (initial tasks, interact, reset and static files).
-}

module Application (application, State (..)) where

import Communication (JsonInput (..), JsonTask (..))
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as BS
import Data.Text (replace)
import Network.HTTP.Media ((//), (/:))
import Network.Wai (Middleware)
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

{-|
  State is used to keep the current task and the original task. 
  currentTask: After sending an Input to Tophat, a new Task is returned. This task is kept in currentTask.
  originalTask: The original task contains the initial task. This task will be reloaded after a reset.
  initialised: used to determine if State is initialised.
-} 
data State h t = State
  { currentTask :: TVar (Task RealWorld t),
    initialised :: Bool,
    originalTask :: Task RealWorld t
  }

type TaskAPI =
  "initial-task" :> Get '[JSON] JsonTask
    :<|> "interact" :> ReqBody '[JSON] JsonInput :> Post '[JSON] JsonTask
    :<|> "reset" :> Get '[JSON] JsonTask

type StaticAPI = Get '[HTML] RawHtml :<|> Raw

type API = TaskAPI :<|> StaticAPI

type AppM h t = ReaderT (State h t) Handler

-- We define a custom HTML type so we can render the index.html file.
data HTML = HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

newtype RawHtml = RawHtml {unRaw :: BS.ByteString}

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

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
    staticServer = indexHandler :<|> assetsHandler

    -- Serves the main entrypoint of the application. We remove the "dev-mode"
    -- property to indicate to the frontend that the file is served from the
    -- backend directly (when developing, the frontend is served using Parcel).
    indexHandler :: AppM h t RawHtml
    indexHandler = do
      fmap RawHtml <| liftIO <| do
        htmlFile <- BS.readFile "frontend/prod/index.html"
        let htmlFileString = decodeUtf8 @Text @BS.ByteString htmlFile
            replacedFile = replace "dev-mode" "" htmlFileString
        return <| encodeUtf8 replacedFile

    -- Serves all static assets (CSS, JS, ...)
    assetsHandler :: Tagged (AppM h t) Application
    assetsHandler = serveDirectoryWebApp "frontend/prod"

apiProxy :: State h t -> Proxy API
apiProxy _ = Proxy

corsPolicy :: Middleware
corsPolicy = cors (const <| Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "OPTIONS"],
          corsOrigins = Nothing,
          corsRequestHeaders = ["authorization", "content-type"]
        }

-- | Create Application. The function takes a State as argument and returns an Application.
application :: ToJSON t => State h t -> Application
application s =
  corsPolicy
    <| serve (apiProxy s)
    <| hoistServer (apiProxy s) (nt s) (server s)
  where
    nt s' x = runReaderT x s'