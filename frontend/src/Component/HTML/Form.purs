{-|
Module to render form fields as separate components, allowing them to manage
state and validation.

*Very* loosely inspired by
https://github.com/fpco/halogen-form/blob/master/src/Halogen/Form.purs
-}
module Component.HTML.Form (FormState, Validator, FormWidget, ValidationError, intInput, textInput, booleanInput, formComponent) where

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

type FormState a b
  = { rawValue :: String
    , touched :: Boolean
    , isValid :: Boolean
    , validate :: Validator a
    , widget :: FormWidget
    , onValidValue :: a -> b
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
formComponent :: forall query output a m. MonadAff m => H.Component query (FormState a output) output m
formComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: FormState a output -> FormState a output
  initialState s = s

defaultState :: forall a b. String -> (a -> b) -> Validator a -> FormState a b
defaultState value onValidValue validate =
  { rawValue: value
  , touched: false
  , isValid: true
  , validate: validate
  , widget: TextInput
  , onValidValue: onValidValue
  }

intInput :: forall a. Maybe Int -> (Int -> a) -> FormState Int a
intInput value onValidValue =
  let
    s =
      defaultState
        (fromMaybe "" $ show <$> value)
        onValidValue
        $ \v -> case fromString v of
            Just v' -> Right v'
            Nothing -> Left InvalidValue
  in
    s { widget = IntInput }

textInput :: forall a. Maybe String -> (String -> a) -> FormState String a
textInput value onValidValue = defaultState (fromMaybe "" value) onValidValue Right

booleanInput :: forall a. Maybe Boolean -> (Boolean -> a) -> FormState Boolean a
booleanInput value onValidValue =
  let
    s =
      defaultState
        (fromMaybe "" $ show <$> value)
        onValidValue
        $ \b -> case parseBoolean b of
            Just b' -> Right b'
            Nothing -> Left InvalidValue
  in
    s { widget = BooleanInput }

handleAction :: forall output m a b. MonadAff m => Action -> H.HalogenM (FormState b output) Action a output m Unit
handleAction (UpdateValue v) = do
  s <- H.get
  case s.validate v of
    Left _ -> do
      H.put $ s { isValid = false }
    Right v' -> do
      H.put $ s { isValid = true }
      H.raise (s.onValidValue v')
  H.modify_ \s' -> s' { touched = true, rawValue = v }

render :: forall m a b. FormState a b -> H.ComponentHTML Action () m
render s@{ widget: widget } = case widget of
  IntInput -> renderIntInput s
  TextInput -> renderTextInput s
  BooleanInput -> renderBooleanInput s

renderIntInput :: forall m a b. FormState a b -> H.ComponentHTML Action () m
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

renderTextInput :: forall m a b. FormState a b -> H.ComponentHTML Action () m
renderTextInput s =
  HH.input
    [ css "input"
    , HP.type_ HP.InputText
    , HP.value s.rawValue
    , HE.onValueInput UpdateValue
    ]

renderBooleanInput :: forall m a b. FormState a b -> H.ComponentHTML Action () m
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
