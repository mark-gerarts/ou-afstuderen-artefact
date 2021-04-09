{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Application (application) where

import Data.Maybe (fromJust)
import Network.Wai (Middleware)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Network.Wai.Middleware.Cors
import Servant
import Task (Input (..), Task (..))
import WaiAppStatic.Types (unsafeToPiece)

type TaskAPI a =
  "current-task" :> Get '[JSON] (Task a)
    :<|> "interact" :> ReqBody '[JSON] Input :> Post '[JSON] (Task a)

type StaticAPI = Raw

type API a = TaskAPI a :<|> StaticAPI

interact :: Input -> Task a -> Task a
interact (Input id value) task@(Update taskId taskValue)
  | id == taskId = Update taskId (fromText (typeOf taskValue) value)
  | otherwise = task
interact input (Pair a b) = Pair (interact input a) (interact input b)

-- | This is a *very* hacky way to handle input for now. Everything is sent to
-- us as a string, we just attempt to cast it to what we need. It gets event
-- uglier because scanning a Text does not work out of the box...
fromText :: (Scan a) => TypeRep a -> Text -> a
fromText tr x =
  case testEquality tr (typeRep @Text) of
    Just Refl -> (scan >> fromJust) ("\"" <> x <> "\"")
    Nothing -> (scan >> fromJust) x

server :: Task a -> Server (API a)
server task = taskServer task :<|> staticServer
  where
    taskServer :: Task a -> Server (TaskAPI a)
    taskServer task' =
      return task'
        :<|> (\input -> return <| interact input task')

    staticServer :: Server StaticAPI
    staticServer =
      -- serveDirectoryWebAbb does not automatically use index.html when
      -- visiting /, so we use the default webApp settings, but override the
      -- fallback.
      let defaultSettings = defaultWebAppSettings "frontend/prod"
          indexFallback = map unsafeToPiece ["index.html"]
       in serveDirectoryWith <| defaultSettings {ssIndices = indexFallback}

apiProxy :: Task a -> Proxy (API a)
apiProxy _ = Proxy

application :: Application
application = corsPolicy <| serve (apiProxy task) (server task)
  where
    -- Task is now hardcoded here, but can serve as the input to Application in
    -- a later stage.
    task = currentTask

corsPolicy :: Middleware
corsPolicy = cors (const <| Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "OPTIONS"],
          corsOrigins = Just (["http://localhost:3001"], True),
          corsRequestHeaders = ["authorization", "content-type"]
        }

currentTask :: Task (Text, Int)
currentTask = Pair textUpdate intUpdate
  where
    textUpdate :: Task Text
    textUpdate = Update 1 "Edit me!!"

    intUpdate :: Task Int
    intUpdate = Update 2 123
