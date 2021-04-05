{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Application (application) where

import Data.Aeson
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

data Input = Input Id Text

instance FromJSON Input where
  parseJSON =
    withObject "Input" <| \obj -> do
      id <- obj .: "id"
      value <- obj .: "value"
      return (Input id value)

type TaskAPI =
  "current-task" :> Get '[JSON] (Task Text)
    :<|> "interact" :> ReqBody '[JSON] Input :> Post '[JSON] (Task Text)

type StaticAPI = Raw

type API = TaskAPI :<|> StaticAPI

currentTask :: Handler (Task Text)
currentTask = return <| Update 1 "Edit me!"

interact :: Input -> Handler (Task Text)
interact (Input id value) =
  return
    <| Update id ("Server received: \"" <> value <> "\"")

server :: Server API
server = taskServer :<|> staticServer
  where
    taskServer :: Server TaskAPI
    taskServer =
      currentTask
        :<|> interact

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
