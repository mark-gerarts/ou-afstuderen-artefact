{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Communication (Input (..), Envelope (..)) where

import Data.Aeson
import Task

-- We wrap the Task in a new datatype and use regular functions to encode them
-- to JSON to prevent orphaned instances.
data Envelope where -- @todo: find a better name for this.
  Envelope :: ToJSON t => Task h t -> Envelope

instance ToJSON Envelope where
  toJSON (Envelope task) = taskToJSON task

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

type Id = Int

data Input where
  Input :: Id -> Value -> Input

instance FromJSON Input where
  parseJSON =
    withObject "Input" <| \obj -> do
      id <- obj .: "id"
      value <- obj .: "value"
      pure <| Input id value
