{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Application (application) where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Servant (Application, Get, JSON, Server, serve, (:>))

data Task a where
  Update :: Text -> Task Text

-- Pair is for a later stage.
-- Pair :: Task a -> Task a -> Task (a, a)

instance ToJSON (Task a) where
  toJSON (Update x) = object ["type" .= ("update" :: Text), "value" .= x]

type TaskAPI = "current-task" :> Get '[JSON] (Task Text)

currentTask :: Task Text
currentTask = Update "Edit me!"

server :: Server TaskAPI
server = return currentTask

userAPI :: Proxy TaskAPI
userAPI = Proxy

application :: Application
application = serve userAPI server
