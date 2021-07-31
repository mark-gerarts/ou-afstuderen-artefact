{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Communication
-- Description : Module to convert data types into JSON and visa versa.
-- Copyright   : (c) Some Guy, 2013
--                   Someone Else, 2014
-- License     : ...
-- Maintainer  : sample@email.com
-- Stability   : experimental
--
-- Module to convert JSONTask (combination of Task and JsonInput) into JSON.
-- This module also provides functions to convert JSON version of inputs to JsonInput.
module Communication (JsonInput (..), TaskDescription (..), describe) where

import Control.Exception (throw)
import Data.Aeson
import Data.Aeson.Types (Parser, parseFail)
import Data.Scientific (toBoundedInteger)
import qualified Data.Store as Store
import Polysemy (Members, Sem)
import Polysemy.Mutate (Alloc, Read)
import Task
import Task.Input (Concrete (..), Dummy, Input (..))
import qualified Task.Observe as Task (inputs)

newtype NotImplementedException = NotImplementedException Text deriving (Debug)

instance Exception NotImplementedException

type JsonTask = Value

type InputDescriptions = List (Input Dummy)

-- | We wrap the Task and its inputs in a new datatype and use regular functions
--  to encode them to JSON to prevent orphaned instances.
data TaskDescription where
  -- | We take a JsonTask as input because converting a task to JSON requires
  -- effects (polysemy), which can't be used in Aeson's ToJSON.
  TaskDescription :: JsonTask -> InputDescriptions -> TaskDescription

instance ToJSON TaskDescription where
  toJSON (TaskDescription jsonTask inputs) =
    object
      [ "task" .= jsonTask,
        "inputs" .= map inputToJSON inputs
      ]

-- | Transforms a task into a JSON representation of the task and its possible
-- inputs, which gets passed to the frontend.
describe :: Members '[Alloc h, Read h] r => Task h t -> Sem r TaskDescription
describe t = do
  inputs' <- Task.inputs t
  t' <- taskToJSON t
  pure <| TaskDescription t' inputs'

taskToJSON :: Members '[Read h] r => Task h t -> Sem r JsonTask
taskToJSON (Edit name editor) = do
  editor' <- editorToJSON editor
  pure
    <| object
      [ "type" .= String "edit",
        "editor" .= editor',
        "name" .= nameToJSON name
      ]
taskToJSON (Pair t1 t2) = do
  t1' <- taskToJSON t1
  t2' <- taskToJSON t2
  pure
    <| object
      [ "type" .= String "pair",
        "t1" .= t1',
        "t2" .= t2'
      ]
taskToJSON (Step t _) = do
  t' <- taskToJSON t
  pure
    <| object
      [ "type" .= String "step",
        "task" .= t'
      ]
taskToJSON (Done _) =
  pure
    <| object
      [ "type" .= String "done"
      ]
taskToJSON Fail =
  pure
    <| object
      [ "type" .= String "fail"
      ]
taskToJSON _ = throw (NotImplementedException "Task of this type is not supported yet")

nameToJSON :: Name -> Value
nameToJSON (Named name) = toJSON name
nameToJSON Unnamed = Null

editorToJSON :: Members '[Read h] r => Editor h t -> Sem r Value
editorToJSON (Update t) =
  pure
    <| object
      [ "type" .= String "update",
        "value" .= t
      ]
editorToJSON (View t) =
  pure
    <| object
      [ "type" .= String "view",
        "value" .= t
      ]
editorToJSON Enter =
  pure
    <| object
      [ "type" .= String "enter"
      ]
editorToJSON (Select _) =
  pure
    <| object
      [ "type" .= String "select"
      ]
editorToJSON (Change l) = do
  value <- Store.read l
  pure
    <| object
      [ "type" .= String "change",
        "value" .= value
      ]
editorToJSON (Watch l) = do
  value <- Store.read l
  pure
    <| object
      [ "type" .= String "watch",
        "value" .= value
      ]

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

-- |
--  Input from frontend (in JSON) is converted to JsonInput
data JsonInput where
  JsonInput :: Input Concrete -> JsonInput

-- | Convert JSON objects into JsonInput
instance FromJSON JsonInput where
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
