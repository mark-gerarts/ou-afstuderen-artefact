module Server (prodMain, develMain) where

import Application (application)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Data.Maybe (fromJust)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory (doesFileExist)
import System.Environment (getEnv)

port :: Int
port = 3000

prodMain :: IO ()
prodMain = do
  putStrLn <| "Starting webserver at http://localhost:3000"
  run port application

develMain :: IO ()
develMain =
  race_ watchTermFile <| do
    innerPort <- getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putTextLn <| "Running in development mode on port " <> toText innerPort
    putTextLn <| "But you should connect to port " <> toText displayPort
    run ((toText >> scan >> fromJust) innerPort) (logStdoutDev application)

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
