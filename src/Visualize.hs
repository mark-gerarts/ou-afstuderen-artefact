{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Visualize
-- Description : Module to load web server with initial task.
-- Copyright   : (c) Some Guy, 2013
--                   Someone Else, 2014
-- License     : ...
-- Maintainer  : sample@email.com
-- Stability   : experimental
--
-- Module to run a web server with a given task. It is possible to load a development or a production environment.
module Visualize (visualizeTask, visualizeTaskDevel) where

import Application (State (..), application)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Data.Maybe (fromJust)
import Data.Store (RealWorld)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory (doesFileExist)
import System.Environment (getEnv, lookupEnv)
import Task (Task)

defaultPort :: [Char]
defaultPort = "3000"

toInt :: ToText a => a -> Int
toInt = toText >> scan >> fromJust

-- function that takes a task and returns an IO Application
initApp :: Task RealWorld t -> IO Application
initApp task = do
  currentTaskTVar <- atomically <| newTVar task
  let initialState =
        State
          { currentTask = currentTaskTVar,
            initialised = False,
            originalTask = task
          }
      app = application initialState
  return app

-- function needed for development purposes.
watchTermFile :: IO ()
watchTermFile =
  loop
  where
    loop = do
      exists <- doesFileExist "yesod-devel/devel-terminate"
      if exists
        then return ()
        else do
          threadDelay 100000
          loop

-- |
--  Run given task in development environment.
visualizeTaskDevel :: Task RealWorld t -> IO ()
visualizeTaskDevel task =
  race_ watchTermFile <| do
    innerPort <- getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putTextLn <| "Running in development mode on port " <> toText innerPort
    putTextLn <| "But you should connect to port " <> toText displayPort
    app <- initApp task
    run (toInt innerPort) (logStdoutDev app)

-- |
--  Run given task in production environment.
visualizeTask :: Task RealWorld t -> IO ()
visualizeTask task = do
  port <- lookupEnv "PORT"
  let realPort = toInt <| port ?: defaultPort
  putTextLn <| "Starting webserver at http://localhost:" <> display realPort
  app <- initApp task
  run realPort app
