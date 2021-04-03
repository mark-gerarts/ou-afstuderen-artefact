{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Application (application) where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Servant
import WaiAppStatic.Types (unsafeToPiece)

data Task a where
  Update :: Text -> Task Text

-- Pair is for a later stage.
-- Pair :: Task a -> Task a -> Task (a, a)

instance ToJSON (Task a) where
  toJSON (Update x) = object ["type" .= ("update" :: Text), "value" .= x]

type TaskAPI = "current-task" :> Get '[JSON] (Task Text)

type StaticAPI = Raw

type API = TaskAPI :<|> StaticAPI

currentTask :: Task Text
currentTask = Update "Edit me!"

server :: Server API
server = taskServer :<|> staticServer
  where
    taskServer :: Server TaskAPI
    taskServer = return currentTask

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
application = serve apiProxy server
