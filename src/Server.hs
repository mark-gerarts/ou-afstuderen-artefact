module Server (prodMain, develMain) where

import Application (State (..), application, initialTask)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Data.Maybe (fromJust)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory (doesFileExist)
import System.Environment (getEnv)

port :: Int
port = 3000

prodMain :: IO ()
prodMain = do
  putStrLn <| "Starting webserver at http://localhost:3000"
  app <- initApp
  run port app

develMain :: IO ()
develMain =
  race_ watchTermFile <| do
    innerPort <- getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putTextLn <| "Running in development mode on port " <> toText innerPort
    putTextLn <| "But you should connect to port " <> toText displayPort
    app <- initApp
    run ((toText >> scan >> fromJust) innerPort) (logStdoutDev app)

initApp :: IO Application
initApp = do
  -- Task is now hardcoded here, but can serve as the input to Application in
  -- a later stage.
  let task = initialTask
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
