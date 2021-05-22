module App.Task where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, isBoolean, isNumber, jsonEmptyObject, jsonNull, (.!=), (.:), (.:?), (:=), (~>))
import Data.Argonaut.Decode.Error as JsonDecodeError
import Data.Array (filter, head, insert)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)

data Task
  = Edit Name Editor
  | Pair Task Task
  | Step Task

instance decodeJsonTask :: DecodeJson Task where
  decodeJson json = do
    obj <- decodeJson json
    taskType <- obj .: "type"
    case taskType of
      "edit" -> do
        editor <- obj .: "editor"
        name <- obj .:? "name" .!= Unnamed
        pure $ Edit name editor
      "pair" -> do
        t1 <- obj .: "t1"
        t2 <- obj .: "t2"
        pure $ Pair t1 t2
      "step" -> do
        task <- obj .: "task"
        pure $ Step task
      _ -> Left (JsonDecodeError.UnexpectedValue (encodeJson taskType))

instance showTask :: Show Task where
  show (Edit name editor) = "Edit [" <> show name <> "] [" <> show editor <> "]"
  show (Pair t1 t2) = "Pair [" <> show t1 <> "] [" <> show t2 <> "]"
  show (Step t) = "Step [" <> show t <> "]"

data Editor
  = Update Value
  | View Value
  | Enter

instance showEditor :: Show Editor where
  show (Update x) = "Update " <> show x
  show (View x) = "View " <> show x
  show (Enter) = "Enter "

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
        pure $ Enter
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
  | Option Name String

instance showInput :: Show Input where
  show (Insert id x) = "Insert [" <> show x <> "] [" <> show id <> "]"
  show (Option name label) = "Option [" <> label <> "] [" <> show name <> "]"

instance encodeInput :: EncodeJson Input where
  encodeJson (Insert id value) =
    "type" := "insert"
      ~> "value"
      := value
      ~> "id"
      := id
      ~> jsonEmptyObject
  encodeJson (Option name label) =
    "type" := "option"
      ~> "name"
      := name
      ~> "label"
      := label
      ~> jsonEmptyObject

instance eqInput :: Eq Input where
  eq (Insert id1 _) (Insert id2 _) = eq id1 id2
  eq (Option id1 _) (Option id2 _) = eq id1 id2
  eq (Insert _ _) (Option _ _ ) = false
  eq (Option _ _) (Insert _ _) = false

instance ordInput :: Ord Input where
  compare (Insert id1 _) (Insert id2 _) = compare id1 id2
  compare (Option (Named id1) _) (Option (Named id2) _) = compare id1 id2
  compare (Option _ _) (Option _ _) = LT
  compare (Insert _ _) (Option _ _ ) = LT
  compare (Option _ _) (Insert _ _) = GT

taskToArray:: Task -> Array Input -> Array Input
taskToArray (Edit (Named id) (Update value)) array = insert (Insert id value) array
taskToArray (Edit _ (View _)) _ = []
taskToArray (Edit (Named id) Enter)  array = insert (Insert id (String "")) array
taskToArray (Edit Unnamed _) _ = []
taskToArray (Pair t1 t2) array = taskToArray t2 (taskToArray t1 array)
taskToArray (Step t) array = taskToArray t array

isSelectedInput :: Int -> Input -> Boolean
isSelectedInput id' (Insert id _) 
 | id == id' = true
 | otherwise = false
isSelectedInput _ (Option _ _) = false

filterInputs:: Int -> Array Input -> Maybe Input
filterInputs id inputs = head $ filter (isSelectedInput id) inputs

selectInput:: Int -> Array Input -> Input
selectInput id inputs = fromMaybe (Insert 0 (String "")) $ (filterInputs id inputs)

updateInput :: Name -> Value -> Input -> Input
updateInput (Named id) newValue input@(Insert name' _)
  | id == name' = Insert id newValue
  | otherwise = input
updateInput _ _ input@(Option _ _) = input
updateInput (Unnamed) _ input = input

data InputDescription
  = InsertDescription Int String
  | OptionDescription Name String

instance showInputDescription :: Show InputDescription where
  show (InsertDescription id x) = "Insert [" <> show x <> "] [" <> show id <> "]"
  show (OptionDescription name label) = "Option [" <> label <> "] [" <> show name <> "]"

instance decodeJsonInputDescription :: DecodeJson InputDescription where
  decodeJson json = do
    obj <- decodeJson json
    inputType <- obj .: "type"
    case inputType of
      "insert" -> do
        value <- obj .: "value"
        id <- obj .: "id"
        pure $ InsertDescription id value
      "option" -> do
        label <- obj .: "label"
        name <- obj .:? "name" .!= Unnamed
        pure $ OptionDescription name label
      _ -> Left (JsonDecodeError.UnexpectedValue json)

isOption :: InputDescription -> Boolean
isOption (InsertDescription _ _) = false
isOption (OptionDescription _ _) = true

-- Select and update inputDescription
isSelectedInputDescription :: Int -> InputDescription -> Boolean
isSelectedInputDescription id' (InsertDescription id _) 
 | id == id' = true
 | otherwise = false
isSelectedInputDescription _ (OptionDescription _ _) = false

filterInputsDescription:: Int -> Array InputDescription -> Maybe InputDescription
filterInputsDescription id inputs = head $ filter (isSelectedInputDescription id) inputs

selectInputDescription:: Int -> Array InputDescription -> InputDescription
selectInputDescription id inputs = fromMaybe (InsertDescription 0 "") $ (filterInputsDescription id inputs)