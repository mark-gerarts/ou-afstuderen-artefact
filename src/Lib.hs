{-# LANGUAGE OverloadedStrings #-}

module Lib (prodMain, develMain) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Network.HTTP.Types (status200)
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory (doesFileExist)
import System.Environment (getEnv)

port :: Int
port = 3000

application :: p -> (Response -> t) -> t
application _ respond =
  respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

prodMain :: IO ()
prodMain = do
  putStrLn $ "Starting webserver on port " ++ show port
  run port application

develMain :: IO ()
develMain = race_ watchTermFile $ do
  port <- read <$> getEnv "PORT"
  displayPort <- getEnv "DISPLAY_PORT"
  putStrLn $ "Running in development mode on port " ++ show port
  putStrLn $ "But you should connect to port " ++ displayPort
  run port $ logStdoutDev application

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
