{-# LANGUAGE GADTs #-}

module Task (Task (..), Input (..)) where

import Data.Aeson

type Id = Int

data Task a where
  Update :: Id -> a -> Task a

-- Pair is for a later stage.
-- Pair :: Task a -> Task a -> Task (a, a)

instance ToJSON a => ToJSON (Task a) where
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
