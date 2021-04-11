{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Task (Task (..), Input (..), TaskValue) where

import Data.Aeson

type Id = Int

type TaskValue a =
  ( ToJSON a, -- To transfer values over the wire.
    FromJSON a
  )

data Task a where
  Update :: TaskValue a => Id -> a -> Task a
  Pair :: (TaskValue a, TaskValue b) => Task a -> Task b -> Task (a, b)

instance ToJSON (Task a) where
  toJSON (Update id value) =
    object
      [ "type" .= ("update" :: Text),
        "value" .= value,
        "id" .= id
      ]
  toJSON (Pair a b) =
    object
      [ "type" .= ("pair" :: Text),
        "a" .= toJSON a,
        "b" .= toJSON b
      ]

data Input where
  Input :: Id -> Value -> Input

instance FromJSON Input where
  parseJSON =
    withObject "Input" <| \obj -> do
      id <- obj .: "id"
      value <- obj .: "value"
      pure <| Input id value
