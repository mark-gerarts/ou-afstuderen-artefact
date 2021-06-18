module Component.HTML.FormComponent where

import Prelude
import Component.HTML.Utils (css)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.TouchEvent.EventTypes (touchend)

-- https://github.com/fpco/halogen-form/blob/master/src/Halogen/Form.purs
-- and
-- https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html
{- type State a html
  = { value :: Maybe a
    , html :: html
    } -}
-- @todo
-- - Incorporate errors: Maybe -> Either
-- - Show validation error on error
-- - Deal with required input
-- - Handle the original actions
-- - FormComponent -> Form
-- - Boolean radio
data Action
  = UpdateValue String

type State a b
  = { value :: Maybe a
    , rawValue :: String
    , touched :: Boolean
    , event :: a -> b
    , isValid :: Boolean
    }

data FormInput a
  = IntInput (State Int a)

formComponent :: forall a query m. MonadAff m => H.Component query (FormInput a) a m
formComponent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = const Nothing
              }
    }
  where
  initialState :: FormInput a -> FormInput a
  initialState s = s

intInput :: forall a. Maybe Int -> (Int -> a) -> FormInput a
intInput v event =
  IntInput
    { value: v
    , rawValue:
        case v of
          Just v' -> show v'
          Nothing -> ""
    , touched: false
    , event: event
    , isValid: true
    }

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
            Just v' -> H.raise (validatedState.event v')
            Nothing -> pure unit

render :: forall m a. FormInput a -> H.ComponentHTML Action () m
render (IntInput s) = renderIntInput s

renderIntInput :: forall m a. State Int a -> H.ComponentHTML Action () m
renderIntInput s =
  let
    cssValue = if s.isValid then "input" else "input is-danger"
  in
    HH.input
      [ css cssValue
      , HP.required true
      , HP.type_ HP.InputNumber
      , HP.value s.rawValue
      , HE.onValueInput UpdateValue
      ]
