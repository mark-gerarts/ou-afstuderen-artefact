{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module Communication (Input (..), Envelope (..)) where

import Data.Aeson
import Task
import Prelude hiding (typeOf)

-- We wrap the Task in a new datatype and use regular functions to encode them
-- to JSON to prevent orphaned instances.
data Envelope where -- @todo: find a better name for this.
  Envelope :: ToJSON t => Task h t -> Envelope

instance ToJSON Envelope where
  toJSON (Envelope task) = taskToJSON task

taskToJSON :: ToJSON t => Task h t -> Value
taskToJSON (Edit _ (Update t)) =
  -- @todo: change JSON structure to reflect nested editor
  object
    [ "type" .= ("update" :: Text),
      "value" .= t,
      "id" .= (1 :: Int) -- @todo: rework frontend to use strings instead of ints
    ]
taskToJSON _ = undefined

type Id = Int

data Input where
  Input :: Id -> Value -> Input

instance FromJSON Input where
  parseJSON =
    withObject "Input" <| \obj -> do
      id <- obj .: "id"
      value <- obj .: "value"
      pure <| Input id value
