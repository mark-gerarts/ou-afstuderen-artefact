{-|
Module to render form fields as separate components, allowing them to manage
state and validation.

*Very* loosely inspired by
https://github.com/fpco/halogen-form/blob/master/src/Halogen/Form.purs
-}
module Component.HTML.Form (intInput, FormInput, textInput, booleanInput, formComponent) where

import Prelude
import Component.HTML.Utils (css)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Action
  = UpdateValue String

type State a b
  = { value :: Maybe a
    , rawValue :: String
    , touched :: Boolean
    , event :: a -> b
    , isValid :: Boolean
    }

data FormInput b
  = IntInput (State Int b)
  | TextInput (State String b)
  | BooleanInput (State Boolean b)

-- The component fires the state's event only when the value is valid.
formComponent :: forall a query m. MonadAff m => H.Component query (FormInput a) a m
formComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: FormInput a -> FormInput a
  initialState s = s

defaultState :: forall a b. Show a => Maybe a -> (a -> b) -> State a b
defaultState v ev =
  { value: v
  , rawValue:
      case v of
        Just v' -> show v'
        Nothing -> ""
  , touched: false
  , event: ev
  , isValid: true
  }

intInput :: forall a. Maybe Int -> (Int -> a) -> FormInput a
intInput v event = IntInput $ defaultState v event

textInput :: forall a. Maybe String -> (String -> a) -> FormInput a
textInput v event = TextInput $ (defaultState v event) { rawValue = fromMaybe "" v }

booleanInput :: forall a. Maybe Boolean -> (Boolean -> a) -> FormInput a
booleanInput v event = BooleanInput $ defaultState v event

handleAction :: forall output m a. MonadAff m => Action -> H.HalogenM (FormInput a) Action output a m Unit
handleAction (UpdateValue v) = do
  s <- H.get
  case s of
    (IntInput s') ->
      let
        newState = s' { touched = true, rawValue = v }

        validatedState = case fromString v of
          Just v' -> newState { value = Just v', isValid = true }
          Nothing -> newState { value = Nothing, isValid = false }
      in
        do
          H.put (IntInput validatedState)
          case validatedState.value of
            Just v' -> H.raise $ validatedState.event v'
            Nothing -> pure unit
    (TextInput s') -> do
      H.put (TextInput s' { value = Just v, rawValue = v })
      H.raise $ s'.event v
    (BooleanInput s') ->
      let
        newValue = v == "true"
      in
        do
          H.put (BooleanInput s' { value = Just newValue })
          H.raise $ s'.event newValue

render :: forall m a. FormInput a -> H.ComponentHTML Action () m
render (IntInput s) = renderIntInput s

render (TextInput s) = renderTextInput s

render (BooleanInput s) = renderBooleanInput s

renderIntInput :: forall m a. State Int a -> H.ComponentHTML Action () m
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

renderTextInput :: forall m a. State String a -> H.ComponentHTML Action () m
renderTextInput s =
  HH.input
    [ css "input"
    , HP.type_ HP.InputText
    , HP.value s.rawValue
    , HE.onValueInput UpdateValue
    ]

renderBooleanInput :: forall m a. State Boolean a -> H.ComponentHTML Action () m
renderBooleanInput s =
  let
    getCheckedAttribute booleanType = case s.value of
      Just v -> [ HP.checked (booleanType == v) ]
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
