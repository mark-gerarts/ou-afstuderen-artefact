{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Application (application) where

import Network.Wai (Middleware)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Network.Wai.Middleware.Cors
import Servant
import Task (Input (..), Task (..))
import WaiAppStatic.Types (unsafeToPiece)

type TaskAPI a =
  "current-task" :> Get '[JSON] (Task a)
    :<|> "interact" :> ReqBody '[JSON] Input :> Post '[JSON] (Task Text)

type StaticAPI = Raw

type API a = TaskAPI a :<|> StaticAPI

interact :: Input -> Handler (Task Text)
interact (Input id value) =
  return
    <| Update id ("Server received: \"" <> value <> "\"")

server :: Task a -> Server (API a)
server task = taskServer task :<|> staticServer
  where
    taskServer :: Task a -> Server (TaskAPI a)
    taskServer task' =
      return task'
        :<|> interact

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
    task = intUpdate

    textUpdate :: Task Text
    textUpdate = Update 1 "Edit me!"

    intUpdate :: Task Int
    intUpdate = Update 1 123

corsPolicy :: Middleware
corsPolicy = cors (const <| Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "OPTIONS"],
          corsOrigins = Just (["http://localhost:3001"], True),
          corsRequestHeaders = ["authorization", "content-type"]
        }
