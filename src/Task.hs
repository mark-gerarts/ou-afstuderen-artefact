{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Task (Task (..), Input (..)) where

import Data.Aeson

type Id = Int

type TaskValue a =
  ( ToJSON a, -- To transfer the value over the wire
    Typeable a, -- To deduce the type of the value
    Scan a -- To read the value back
  )

data Task a where
  Update :: TaskValue a => Id -> a -> Task a
  Pair :: (TaskValue a, TaskValue b) => Task a -> Task b -> Task (a, b)

instance (ToJSON a, Typeable a) => ToJSON (Task a) where
  toJSON (Update id x) =
    object
      [ "type" .= ("update" :: Text),
        "value" .= value,
        "id" .= id
      ]
    where
      value =
        object
          [ "value" .= x,
            "type" .= showType x
          ]
  toJSON (Pair a b) =
    object
      [ "type" .= ("pair" :: Text),
        "a" .= toJSON a,
        "b" .= toJSON b
      ]

showType :: Typeable a => a -> Text
showType x = display (typeOf x)

data Input where
  Input :: Id -> Text -> Input

instance FromJSON Input where
  parseJSON =
    withObject "Input" <| \obj -> do
      id <- obj .: "id"
      value <- obj .: "value"
      return (Input id value)
