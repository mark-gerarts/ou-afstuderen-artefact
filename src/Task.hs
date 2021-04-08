{-# LANGUAGE GADTs #-}

module Task (Task (..), Input (..)) where

import Data.Aeson

type Id = Int

data Task a where
  Update :: Id -> a -> Task a

-- Pair is for a later stage.
-- Pair :: Task a -> Task a -> Task (a, a)

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

showType :: Typeable a => a -> Text
showType x = display (typeOf x)

data Input = Input Id Text

instance FromJSON Input where
  parseJSON =
    withObject "Input" <| \obj -> do
      id <- obj .: "id"
      value <- obj .: "value"
      return (Input id value)
