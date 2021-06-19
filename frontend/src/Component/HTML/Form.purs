{-|
Module to render form fields as separate components, allowing them to manage
state and validation.

*Very* loosely inspired by
https://github.com/fpco/halogen-form/blob/master/src/Halogen/Form.purs
-}
module Component.HTML.Form (FormState, Validator, FormWidget, ValidationError, intInput, textInput, booleanInput, component) where

import Prelude
import Component.HTML.Utils (css)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Action
  = UpdateValue String

type FormState a
  = { rawValue :: String
    , isValid :: Boolean
    , validate :: Validator a
    , widget :: FormWidget
    }

type Validator a
  = String -> Either ValidationError a

data ValidationError
  = InvalidValue

data FormWidget
  = IntInput
  | TextInput
  | BooleanInput

derive instance eqFormWidget :: Eq FormWidget

-- The component fires the state's event only when the value is valid.
component :: forall query output m. MonadAff m => H.Component query (FormState output) output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: FormState output -> FormState output
  initialState s = s

defaultState :: forall a. String -> Validator a -> FormState a
defaultState value validate =
  { rawValue: value
  , isValid: true
  , validate: validate
  , widget: TextInput
  }

intInput :: Maybe Int -> FormState Int
intInput value =
  let
    s =
      defaultState
        (fromMaybe "" $ show <$> value)
        $ \v -> case fromString v of
            Just v' -> Right v'
            Nothing -> Left InvalidValue
  in
    s { widget = IntInput }

textInput :: Maybe String -> FormState String
textInput value = defaultState (fromMaybe "" value) Right

booleanInput :: Maybe Boolean -> FormState Boolean
booleanInput value =
  let
    s =
      defaultState
        (fromMaybe "" $ show <$> value)
        $ \b -> case parseBoolean b of
            Just b' -> Right b'
            Nothing -> Left InvalidValue
  in
    s { widget = BooleanInput }

handleAction :: forall output m a. MonadAff m => Action -> H.HalogenM (FormState output) Action a output m Unit
handleAction (UpdateValue v) = do
  s <- H.get
  case s.validate v of
    Left _ -> do
      H.put $ s { isValid = false }
    Right v' -> do
      H.put $ s { isValid = true }
      H.raise v'
  H.modify_ \s' -> s' { rawValue = v }

render :: forall m a. FormState a -> H.ComponentHTML Action () m
render s@{ widget: widget } = case widget of
  IntInput -> renderIntInput s
  TextInput -> renderTextInput s
  BooleanInput -> renderBooleanInput s

renderIntInput :: forall m a. FormState a -> H.ComponentHTML Action () m
renderIntInput s =
  let
    cssValue = if s.isValid then "input" else "input is-danger"
  in
    HH.input
      [ css cssValue
      , HP.type_ HP.InputNumber
      , HP.value s.rawValue
      , HE.onValueInput UpdateValue
      ]

renderTextInput :: forall m a. FormState a -> H.ComponentHTML Action () m
renderTextInput s =
  HH.input
    [ css "input"
    , HP.type_ HP.InputText
    , HP.value s.rawValue
    , HE.onValueInput UpdateValue
    ]

renderBooleanInput :: forall m a. FormState a -> H.ComponentHTML Action () m
renderBooleanInput s =
  let
    getCheckedAttribute booleanType = case parseBoolean s.rawValue of
      Just b -> [ HP.checked (b == booleanType) ]
      Nothing -> []

    getRadio booleanType =
      HH.div [ css $ "radio-" <> show booleanType ]
        [ HH.label
            [ css "radio" ]
            [ HH.input
                ( [ HP.type_ HP.InputRadio
                  , HP.name "radiobuttonTrueFalse"
                  , HE.onChange \_ -> UpdateValue (show booleanType)
                  ]
                    <> getCheckedAttribute booleanType
                )
            , HH.text (if booleanType then " True" else " False")
            ]
        ]
  in
    HH.div_
      [ getRadio true, getRadio false ]

parseBoolean :: String -> Maybe Boolean
parseBoolean s = case s of
  "true" -> Just true
  "false" -> Just false
  _ -> Nothing
