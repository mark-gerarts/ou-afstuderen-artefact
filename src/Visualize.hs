module Visualize (visualizeTask, visualizeTaskDevel) where

import Application (State (..), application)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Data.Aeson (ToJSON)
import Data.Maybe (fromJust)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import Task (RealWorld, Task)

port :: Int
port = 3000

visualizeTask :: ToJSON t => Task RealWorld t -> IO ()
visualizeTask task = do
  putStrLn <| "Starting webserver at http://localhost:3000"
  app <- initApp task
  run port app

visualizeTaskDevel :: ToJSON t => Task RealWorld t -> IO ()
visualizeTaskDevel task =
  race_ watchTermFile <| do
    innerPort <- getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putTextLn <| "Running in development mode on port " <> toText innerPort
    putTextLn <| "But you should connect to port " <> toText displayPort
    app <- initApp task
    run ((toText >> scan >> fromJust) innerPort) (logStdoutDev app)

initApp :: ToJSON t => Task RealWorld t -> IO Application
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
