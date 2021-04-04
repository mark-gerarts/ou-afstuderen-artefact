{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Application (application) where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Network.Wai (Middleware)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Network.Wai.Middleware.Cors
import Servant
import WaiAppStatic.Types (unsafeToPiece)

type Id = Int

data Task a where
  Update :: Id -> Text -> Task Text

-- Pair is for a later stage.
-- Pair :: Task a -> Task a -> Task (a, a)

instance ToJSON (Task a) where
  toJSON (Update id x) =
    object
      [ "type" .= ("update" :: Text),
        "value" .= x,
        "id" .= id
      ]

type TaskAPI =
  "current-task" :> Get '[JSON] (Task Text)
    :<|> "interact" :> Post '[JSON] (Task Text)

type StaticAPI = Raw

type API = TaskAPI :<|> StaticAPI

currentTask :: Task Text
currentTask = Update 1 "Edit me!"

interact :: Task Text
interact = Update 1 "You've interacted!"

server :: Server API
server = taskServer :<|> staticServer
  where
    taskServer :: Server TaskAPI
    taskServer =
      return currentTask
        :<|> return interact

    staticServer :: Server StaticAPI
    staticServer =
      -- serveDirectoryWebAbb does not automatically use index.html when
      -- visiting /, so we use the default webApp settings, but override the
      -- fallback.
      let defaultSettings = defaultWebAppSettings "frontend/prod"
          indexFallback = map unsafeToPiece ["index.html"]
       in serveDirectoryWith <| defaultSettings {ssIndices = indexFallback}

apiProxy :: Proxy API
apiProxy = Proxy

application :: Application
application = corsPolicy <| serve apiProxy server

corsPolicy :: Middleware
corsPolicy = cors (const <| Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "OPTIONS"],
          corsOrigins = Just (["http://localhost:3001"], True),
          corsRequestHeaders = ["authorization", "content-type"]
        }
