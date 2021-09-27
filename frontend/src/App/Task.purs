{-|
Module      : App.Task
Description : Module for JSON conversion
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : ...
Maintainer  : sample@email.com
Stability   : experimental

Module to decode JSON (into Tasks, Editors, Values and InputDescription). Module also used to encode Input (into JSON).
-}
module App.Task where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, isBoolean, isNumber, jsonEmptyObject, jsonNull, (.!=), (.:), (.:?), (:=), (~>))
import Data.Argonaut.Decode.Error as JsonDecodeError
import Data.Array (filter, head, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

type Labels
  = Array String

data Task
  = Edit Name Editor
  | Select Name Task Labels
  | Pair Task Task
  | Choose Task Task
  | Step Task
  | Done
  | Fail

instance showTask :: Show Task where
  show (Edit name editor) = "Edit [" <> show name <> "] [" <> show editor <> "]"
  show (Select name task _) = "Select [" <> show name <> "] [" <> show task <> "]"
  show (Pair t1 t2) = "Pair [" <> show t1 <> "] [" <> show t2 <> "]"
  show (Choose t1 t2) = "Choose [" <> show t1 <> "] [" <> show t2 <> "]"
  show (Step t) = "Step [" <> show t <> "]"
  show Done = "Done"
  show Fail = "Fail"

instance decodeJsonTask :: DecodeJson Task where
  decodeJson json = do
    obj <- decodeJson json
    taskType <- obj .: "type"
    case taskType of
      "edit" -> do
        editor <- obj .: "editor"
        name <- obj .:? "name" .!= Unnamed
        pure $ Edit name editor
      "select" -> do
        name <- obj .:? "name" .!= Unnamed
        t <- obj .: "t"
        labels <- obj .: "labels"
        pure $ Select name t labels
      "pair" -> do
        t1 <- obj .: "t1"
        t2 <- obj .: "t2"
        pure $ Pair t1 t2
      "choose" -> do
        t1 <- obj .: "t1"
        t2 <- obj .: "t2"
        pure $ Choose t1 t2
      "step" -> do
        task <- obj .: "task"
        pure $ Step task
      "done" -> do
        pure Done
      "fail" -> do
        pure Fail
      _ -> Left (JsonDecodeError.UnexpectedValue (encodeJson taskType))

data Editor
  = Update Value
  | View Value
  | Enter
  | Change Value
  | Watch Value

instance showEditor :: Show Editor where
  show (Update x) = "Update " <> show x
  show (View x) = "View " <> show x
  show Enter = "Enter "
  show (Change x) = "Change " <> show x
  show (Watch x) = "Watch " <> show x

instance decodeJsonEditor :: DecodeJson Editor where
  decodeJson json = do
    obj <- decodeJson json
    editorType <- obj .: "type"
    case editorType of
      "update" -> do
        value <- obj .: "value"
        pure $ Update value
      "view" -> do
        value <- obj .: "value"
        pure $ View value
      "enter" -> do
        pure Enter
      "change" -> do
        value <- obj .: "value"
        pure $ Change value
      "watch" -> do
        value <- obj .: "value"
        pure $ Watch value
      _ -> Left (JsonDecodeError.UnexpectedValue json)

data Name
  = Named Int
  | Unnamed

derive instance eqName :: Eq Name

instance showName :: Show Name where
  show (Named id) = "Named " <> show id
  show Unnamed = "Unnamed"

instance decodeJsonName :: DecodeJson Name where
  decodeJson json = do
    int <- decodeJson json
    pure $ Named int

instance encodeJsonName :: EncodeJson Name where
  encodeJson name = case name of
    Named id -> encodeJson id
    Unnamed -> jsonNull

data Value
  = Int Int
  | String String
  | Boolean Boolean

instance showValue :: Show Value where
  show (Int int) = show int
  show (String string) = string
  show (Boolean boolean) = show boolean

instance decodeJsonValue :: DecodeJson Value where
  decodeJson json = do
    value <- decodeJson json
    fromValue value
    where
    fromValue v
      | isBoolean v = Boolean <$> decodeJson v
      | isNumber v = Int <$> decodeJson v
      | otherwise = String <$> decodeJson v

instance encodeJsonValue :: EncodeJson Value where
  encodeJson (String string) = encodeJson string
  encodeJson (Int int) = encodeJson int
  encodeJson (Boolean bool) = encodeJson bool

data Input
  = Insert Int Value
  | Decide Int String

instance showInput :: Show Input where
  show (Insert id x) = "Insert [" <> show x <> "] [" <> show id <> "]"
  show (Decide id label) = "Decide [" <> label <> "] [" <> show id <> "]"

instance encodeInput :: EncodeJson Input where
  encodeJson (Insert id value) =
    "type" := "insert"
      ~> "value"
      := value
      ~> "id"
      := id
      ~> jsonEmptyObject
  encodeJson (Decide id label) =
    "type" := "decide"
      ~> "id"
      := id
      ~> "label"
      := label
      ~> jsonEmptyObject

data InputDescription
  = InsertDescription Int String
  | DecideDescription Int String

instance showInputDescription :: Show InputDescription where
  show (InsertDescription id x) = "Insert [" <> show x <> "] [" <> show id <> "]"
  show (DecideDescription name label) = "Decide [" <> label <> "] [" <> show name <> "]"

instance decodeJsonInputDescription :: DecodeJson InputDescription where
  decodeJson json = do
    obj <- decodeJson json
    inputType <- obj .: "type"
    case inputType of
      "insert" -> do
        value <- obj .: "value"
        id <- obj .: "id"
        pure $ InsertDescription id value
      "decide" -> do
        label <- obj .: "label"
        id <- obj .: "id"
        pure $ DecideDescription id label
      _ -> Left (JsonDecodeError.UnexpectedValue json)

isDecide :: InputDescription -> Boolean
isDecide (DecideDescription _ _) = true

isDecide _ = false

hasLabel :: InputDescription -> String -> Boolean
hasLabel (InsertDescription _ s) s' = s == s'

hasLabel (DecideDescription _ s) s' = s == s'

isSelectedInputDescription :: Int -> InputDescription -> Boolean
isSelectedInputDescription id (InsertDescription id' _) = id == id'

isSelectedInputDescription id (DecideDescription id' _) = id == id'

-- Function that returns an array of predefined values of editors.
taskToArray :: Task -> Array Input -> Array Input
taskToArray (Edit (Named id) (Update value)) array = Insert id value : array

taskToArray (Edit (Named id) Enter) array = Insert id (String "") : array

taskToArray (Pair t1 t2) array = taskToArray t2 (taskToArray t1 array)

taskToArray (Choose t1 t2) array = taskToArray t2 (taskToArray t1 array)

taskToArray (Step t) array = taskToArray t array

taskToArray _ _ = []

-- function to filter out a specific InputDescription in a given array of InputDescription.
filterInputsDescription :: Int -> Array InputDescription -> Maybe InputDescription
filterInputsDescription id inputs = head $ filter (isSelectedInputDescription id) inputs

-- function to filter out a specific InputDescription in a given array of InputDescription, under assumption that the InputDescription exists.
selectInputDescription :: Int -> Array InputDescription -> InputDescription
selectInputDescription id inputs = unsafePartial $ fromJust $ (filterInputsDescription id inputs)
