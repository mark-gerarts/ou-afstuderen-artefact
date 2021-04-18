{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Communication (JsonTask (..), JsonInput (..)) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Task
import Task.Input (Concrete (..), Input (..))

-- We wrap the Task in a new datatype and use regular functions to encode them
-- to JSON to prevent orphaned instances.
data JsonTask where -- @todo: find a better name for this.
  JsonTask :: ToJSON t => Task h t -> JsonTask

instance ToJSON JsonTask where
  toJSON (JsonTask task) = taskToJSON task

taskToJSON :: Task h t -> Value
taskToJSON (Edit name editor) =
  object
    [ "type" .= String "edit",
      "editor" .= editorToJSON editor,
      "name" .= nameToJSON name
    ]
taskToJSON (Pair t1 t2) =
  object
    [ "type" .= String "pair",
      "t1" .= taskToJSON t1,
      "t2" .= taskToJSON t2
    ]
taskToJSON (Step t _) =
  object
    [ "type" .= String "step",
      "task" .= taskToJSON t
    ]
taskToJSON _ = undefined

nameToJSON :: Name -> Value
nameToJSON (Named name) = toJSON name
nameToJSON Unnamed = Null

editorToJSON :: Editor h t -> Value
editorToJSON (Update t) =
  object
    [ "type" .= String "update",
      "value" .= t
    ]
editorToJSON _ = undefined

data JsonInput where
  JsonInput :: Input Concrete -> JsonInput

-- @todo: hardcoded Int for now
instance FromJSON JsonInput where
  parseJSON =
    withObject "Input" <| \obj -> do
      name <- obj .: "name"
      value <- obj .: "value"
      pure (JsonInput (Insert name (Concrete (value :: Int))))
