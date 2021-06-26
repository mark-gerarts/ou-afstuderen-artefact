{-|
Module to render form fields as separate components, allowing them to manage
state and validation.

*Very* loosely inspired by
https://github.com/fpco/halogen-form/blob/master/src/Halogen/Form.purs
-}
module Component.HTML.Form (FormState, Validator, FormWidget, ValidationError, intInput, textInput, booleanInput, component) where

import Prelude
import Component.HTML.Utils (css)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Math (abs)

data Action a
  = UpdateValue String
  | Notify a

type FormState a
  = { rawValue :: String
    , isValid :: Boolean
    , validate :: Validator a
    , widget :: FormWidget
    , lastChangeAt :: Instant
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

-- Wait `delay` milliseconds after the last value input to fire the event.
delay :: Number
delay = 500.0

-- Form component that renders a specific field. Only when an entered value is
-- valid, an output message will be raised.
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
  , lastChangeAt: bottom
  }

-- Helper functions to construct form fields of a given type.
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

-- function that defines actions.
handleAction :: forall output m a. MonadAff m => Action output -> H.HalogenM (FormState output) (Action output) a output m Unit
handleAction = case _ of
  UpdateValue v -> do
    s <- H.get
    case s.validate v of
      Left _ -> do
        H.put $ s { isValid = false }
      Right v' -> do
        -- Initialize a delay: only when the user stopped typing for 500ms, we
        -- trigger the actual event.
        t <- H.liftEffect now
        H.put $ s { isValid = true, lastChangeAt = t }
        _ <- H.subscribe =<< startDelay (Notify v')
        pure unit
    H.modify_ \s' -> s' { rawValue = v }
  Notify v -> do
    s <- H.get
    delayElapsed <- H.liftEffect $ hasDelayElapsed s
    when delayElapsed do
      H.raise v
  where
  hasDelayElapsed state = do
    t <- now
    let
      (Milliseconds d) = difference t state.lastChangeAt
    pure $ d >= delay

startDelay :: forall m a. MonadAff m => a -> m (HS.Emitter a)
startDelay val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <-
    H.liftAff $ Aff.forkAff
      $ do
          Aff.delay $ Milliseconds delay
          H.liftEffect $ HS.notify listener val
  pure emitter

-- Function that renders an input. Takes a FormState as argument.
render :: forall m a. FormState a -> H.ComponentHTML (Action a) () m
render s@{ widget: widget } = case widget of
  IntInput -> renderIntInput s
  TextInput -> renderTextInput s
  BooleanInput -> renderBooleanInput s

renderIntInput :: forall m a. FormState a -> H.ComponentHTML (Action a) () m
renderIntInput s =
  let
    cssValue = if s.isValid then "input" else "input is-danger"
  in
    HH.input
      [ css cssValue
      , HP.type_ HP.InputNumber
      , HP.value s.rawValue
      , HP.tabIndex 0
      , HE.onValueInput UpdateValue
      ]

renderTextInput :: forall m a. FormState a -> H.ComponentHTML (Action a) () m
renderTextInput s =
  HH.input
    [ css "input"
    , HP.type_ HP.InputText
    , HP.value s.rawValue
    , HE.onValueInput UpdateValue
    ]

renderBooleanInput :: forall m a. FormState a -> H.ComponentHTML (Action a) () m
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

difference :: Instant -> Instant -> Milliseconds
difference x y =
  let
    (Milliseconds x') = unInstant x

    (Milliseconds y') = unInstant y
  in
    Milliseconds $ abs $ y' - x'
