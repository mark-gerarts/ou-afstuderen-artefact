{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Communication (JsonTask (..), JsonInput (..)) where

import Data.Aeson
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import Task
import Task.Input (Concrete (..), Dummy, Input (..))

-- We wrap the Task in a new datatype and use regular functions to encode them
-- to JSON to prevent orphaned instances.
data JsonTask where -- @todo: find a better name for this.
  JsonTask :: ToJSON t => Task h t -> List (Input Dummy) -> JsonTask

instance ToJSON JsonTask where
  toJSON (JsonTask task inputs) =
    object
      [ "task" .= taskToJSON task,
        "inputs" .= map inputToJSON inputs
      ]

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

nameFromJSON :: Value -> Maybe Name
nameFromJSON (Number i) = Named <|| toBoundedInteger i
nameFromJSON Null = Just Unnamed
nameFromJSON _ = Nothing

editorToJSON :: Editor h t -> Value
editorToJSON (Update t) =
  object
    [ "type" .= String "update",
      "value" .= t
    ]
editorToJSON (View t) =
  object
    [ "type" .= String "view",
      "value" .= t
    ]
editorToJSON _ = undefined

data JsonInput where
  JsonInput :: Input Concrete -> JsonInput

-- @todo: hardcoded Int for now
instance FromJSON JsonInput where
  parseJSON =
    withObject "Input" <| \obj -> do
      inputType <- obj .: "type"
      case inputType of
        ("insert" :: Text) -> do
          id <- obj .: "id"
          value <- obj .: "value"
          pure (JsonInput (Insert id (Concrete (value :: Int))))
        ("option" :: Text) -> do
          name <- obj .: "name"
          label <- obj .: "label"
          pure (JsonInput (Option (fromJust <| nameFromJSON name) label))
        _ -> pure (JsonInput (Option Unnamed "todo: error handle me"))

inputToJSON :: Input Dummy -> Value
inputToJSON (Insert id value) =
  object
    [ "type" .= String "insert",
      "id" .= id,
      "value" .= display value
    ]
inputToJSON (Option name label) =
  object
    [ "type" .= String "option",
      "name" .= nameToJSON name,
      "label" .= label
    ]
