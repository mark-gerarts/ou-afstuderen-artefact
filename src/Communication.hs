{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Communication
Description : Module to convert data types into JSON and visa versa.
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : ...
Maintainer  : sample@email.com
Stability   : experimental

Module to convert JSONTask (combination of Task and JsonInput) into JSON. 
This module also provides functions to convert JSON version of inputs to JsonInput.
-}

module Communication (JsonTask (..), JsonInput (..)) where

import Control.Exception (throw)
import Data.Aeson
import Data.Aeson.Types (Parser, parseFail)
import Data.Scientific (toBoundedInteger)
import Task
import Task.Input (Concrete (..), Dummy, Input (..))

newtype NotImplementedException = NotImplementedException Text deriving (Debug)

instance Exception NotImplementedException

{-|
  JsonTask: We wrap the Task in a new datatype and use regular functions to encode them
  to JSON to prevent orphaned instances.
-} 
data JsonTask where -- @todo: find a better name for this.
  -- | JsonTask takes a Task and a List of Input as a argument and return a JsonTask
  JsonTask :: ToJSON t => Task h t -> List (Input Dummy) -> JsonTask

instance ToJSON JsonTask where
  -- | function to convert JsonTask to JSON
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
taskToJSON (Done _) =
  object
    [ "type" .= String "done"
    ]
taskToJSON Fail =
  object
    [ "type" .= String "fail"
    ]
taskToJSON _ = throw (NotImplementedException "Task of this type is not supported yet")

nameToJSON :: Name -> Value
nameToJSON (Named name) = toJSON name
nameToJSON Unnamed = Null

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
editorToJSON Enter =
  object
    [ "type" .= String "enter"
    ]
editorToJSON (Select _) =
  object
    [ "type" .= String "select"
    ]
editorToJSON (Change _) = throw (NotImplementedException "Editor type 'Change' is not supported yet")
editorToJSON (Watch _) = throw (NotImplementedException "Editor type 'Watch' is not supported yet")

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

{-|
  JsonInput: input from frontend (in JSON) is converted to JsonInput
-}    
data JsonInput where
  JsonInput :: Input Concrete -> JsonInput

instance FromJSON JsonInput where
  -- | function to convert JSON to JsonInput
  parseJSON =
    withObject "Input" <| \obj -> do
      inputType <- obj .: "type"
      case inputType of
        ("insert" :: Text) -> do
          id <- obj .: "id"
          value <- obj .: "value" >>= parseConcrete
          pure (JsonInput (Insert id value))
        ("option" :: Text) -> do
          name <- obj .: "name" >>= parseName
          label <- obj .: "label"
          pure (JsonInput (Option name label))
        _ -> parseFail "Invalid input type"

parseConcrete :: Value -> Parser Concrete
parseConcrete (String s) = return (Concrete s)
parseConcrete (Number x) =
  case toBoundedInteger x of
    Just (i :: Int) -> return (Concrete i)
    Nothing -> parseFail "Invalid integer value"
parseConcrete (Bool b) = return (Concrete b)
parseConcrete _ = parseFail "Unsupported type"

parseName :: Value -> Parser Name
parseName (Number i) =
  case toBoundedInteger i of
    Just i' -> return (Named i')
    Nothing -> parseFail "Unexpected name"
parseName Null = return Unnamed
parseName _ = parseFail "Unexpected name"