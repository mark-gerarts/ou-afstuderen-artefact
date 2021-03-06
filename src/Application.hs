{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Application
-- Description : Module to load the application
-- Copyright   : (c) Some Guy, 2013
--                   Someone Else, 2014
-- License     : ...
-- Maintainer  : sample@email.com
-- Stability   : experimental
--
-- Module to load the application: set the server and define the handlers (initial tasks, interact, reset and static files).
module Application (application, State (..)) where

import Communication (JsonInput (..), TaskDescription (..), describe)
import qualified Data.ByteString.Lazy as BS
import Data.Store (RealWorld)
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
import Task (Task)
import Task.Input (Concrete (..), Input (..))
import Task.Run (NotApplicable, Steps, initialise, interact)

-- |
--  State is used to keep the current task and the original task.
--  currentTask: After sending an Input to Tophat, a new Task is returned. This task is kept in currentTask.
--  originalTask: The original task contains the initial task. This task will be reloaded after a reset.
--  initialised: used to determine if State is initialised.
data State h t = State
  { currentTask :: TVar (Task RealWorld t),
    initialised :: Bool,
    originalTask :: Task RealWorld t
  }

-- defining endpoints, request and response formats related to Tasks
type TaskAPI =
  "initial-task" :> Get '[JSON] TaskDescription
    :<|> "interact" :> ReqBody '[JSON] JsonInput :> Post '[JSON] TaskDescription
    :<|> "reset" :> Get '[JSON] TaskDescription

-- defining endpoints, request and response formats related to static files
type StaticAPI = Get '[HTML] RawHtml :<|> Raw

-- combined API
type API = TaskAPI :<|> StaticAPI

-- Monad to run handlers
type AppM h t = ReaderT (State h t) Handler

-- We define a custom HTML type so we can render the index.html file.
data HTML = HTML

-- needed to render index.html
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- needed to render index.html
newtype RawHtml = RawHtml {unRaw :: BS.ByteString}

-- needed to render index.html
instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

-- Function that takes an Input and a Task as argument and returns a new Task (Communication with TopHat).
interactIO :: Input Concrete -> Task RealWorld a -> IO (Task RealWorld a)
interactIO i t = withIO <| interact i t

-- Function to initialise Task (Communication with Tophat).
initialiseIO :: Task RealWorld a -> IO (Task RealWorld a)
initialiseIO t = withIO <| initialise t

-- Transforms a task into a TaskDescription, which can be sent to the frontend.
describeIO :: Task RealWorld a -> IO TaskDescription
describeIO t = liftIO <| withIO <| describe t

-- Polysemy
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

-- Defining webservice to handle requests: defining handlers
server :: State h t -> ServerT API (AppM h t)
server _ = taskServer :<|> staticServer
  where
    -- defining web server for tasks
    taskServer :: ServerT TaskAPI (AppM h t)
    taskServer = do
      initialTaskHandler
        :<|> interactHandler
        :<|> resetHandler

    -- handler that returns the initialised task
    initialTaskHandler :: AppM h t TaskDescription
    initialTaskHandler = do
      initialisedTask <- getInitialisedTask
      liftIO <| describeIO initialisedTask
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

    -- handler that takes a JsonInput and returns a new task.
    interactHandler :: JsonInput -> AppM h t TaskDescription
    interactHandler (JsonInput input) = do
      State {currentTask = t} <- ask
      liftIO <| do
        t' <- readTVarIO t
        newTask <- interactIO input t'
        atomically <| writeTVar t newTask
        liftIO <| describeIO newTask

    -- handler that returns the original task.
    resetHandler :: AppM h t TaskDescription
    resetHandler = do
      State {currentTask = t, originalTask = t_o} <- ask
      liftIO <| do
        initialisedTask <- initialiseIO t_o
        atomically <| writeTVar t initialisedTask
        liftIO <| describeIO initialisedTask

    -- defining webservice for static files
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

-- boilerplate code for type inference
apiProxy :: State h t -> Proxy API
apiProxy _ = Proxy

-- Frontend is running at another port. corsPolicy is needed to allow requests from frontend.
corsPolicy :: Middleware
corsPolicy = cors (const <| Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "OPTIONS"],
          corsOrigins = Nothing,
          corsRequestHeaders = ["authorization", "content-type"]
        }

-- | Create abstract web Application. The function takes a State as argument and returns an Application.
application :: State h t -> Application
application s =
  corsPolicy
    <| serve (apiProxy s)
    -- hoistServer transforms ServerT API AppM into ServerT API Handler
    <| hoistServer (apiProxy s) (nt s) (server s)
  where
    -- Function to transform AppM into Handler
    nt s' x = runReaderT x s'
