module Lib (prodMain, develMain) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Network.HTTP.Types (status200)
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import Task (Task, select, update, view, (<?>))

port :: Int
port = 3000

application :: p -> (Response -> t) -> t
application _ respond =
  respond
    <| responseLBS status200 [("Content-Type", "text/plain")]
    <| encodeUtf8
    <| display (inc 2)
  where
    inc :: Int -> Task h Int
    inc x =
      view (x + 1)

    dec :: Int -> Task h Int
    dec x =
      view (x - 1)

    oneStep :: Task h Int
    oneStep = do
      x <- update 0
      select
        <| HM.fromList
          [ "Increase" ~> inc x,
            "Decrease" ~> dec x
          ]

prodMain :: IO ()
prodMain = do
  putStrLn <| "Starting webserver at http://localhost:3000"
  run port application

develMain :: IO ()
develMain =
  race_ watchTermFile <| do
    port <- getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putTextLn <| "Running in development mode on port " <> toText port
    putTextLn <| "But you should connect to port " <> toText displayPort
    run ((toText >> scan >> fromJust) port) (logStdoutDev application)

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
