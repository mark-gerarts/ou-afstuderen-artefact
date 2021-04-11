module Component.TaskLoader (taskLoader) where

import Prelude
import App.Client (getCurrentTask, interact)
import App.Task (Id, Input(..), Task(..), Value(..), ValueType(..), updateTask)
import Component.HTML.Bulma as Bulma
import Component.HTML.Utils (css)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)

type State
  = { isLoading :: Boolean
    , currentTask :: Maybe Task
    }

data Action
  = FetchCurrentTask
  | UpdateValue Id String
  | Interact Task Event
  | LogState -- For debug purposes...

taskLoader :: forall query input output m. MonadAff m => H.Component query input output m
taskLoader =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just FetchCurrentTask
              }
    }
  where
  initialState _ =
    { isLoading: true
    , currentTask: Nothing
    }

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction FetchCurrentTask = do
  t <- H.liftAff $ getCurrentTask
  case t of
    Left err -> logShow err
    Right task -> H.modify_ \s -> s { currentTask = Just task }
  H.modify_ \s -> s { isLoading = false }

handleAction LogState = H.get >>= logShow

handleAction (Interact task event) = do
  H.liftEffect $ Event.preventDefault event
  s <- H.get
  case task of
    Pair _ _ -> logShow "Error"
    Update id (Value value _) -> do
      r <- H.liftAff $ interact (Input id value)
      case r of
        Left err -> logShow err
        Right newTask -> do
          -- For some reason Halogen renders an empty input if we immediately
          -- alter the state. By first setting the state to something else this
          -- doesn't happen...
          H.modify_ \s' -> s' { currentTask = Nothing }
          H.modify_ \s' -> s' { currentTask = Just newTask }

handleAction (UpdateValue id x) = do
  H.modify_ \s -> s { currentTask = (updateTask id x) <$> s.currentTask }

render :: forall a. State -> HH.HTML a Action
render state = case state of
  { isLoading: true } -> renderLoadingScreen
  { currentTask: Just task } -> renderTask task
  _ -> renderError

renderLoadingScreen :: forall a. HH.HTML a Action
renderLoadingScreen =
  HH.div_
    [ HH.h1 [ css "title" ] [ HH.text "Loading" ]
    , HH.p [ css "subtitle" ] [ HH.text "Fetching current task from the server..." ]
    ]

renderError :: forall a. HH.HTML a Action
renderError = HH.p_ [ HH.text "An error occurred :(" ]

renderTask :: forall a. Task -> HH.HTML a Action
renderTask task@(Update id value) =
  Bulma.panel ("Update Task [" <> show id <> "]")
    ( HH.form
        [ HE.onSubmit \e -> Interact task e, css "control" ]
        [ HH.div [ css "field" ]
            [ HH.label_ [ HH.text "Value" ]
            , HH.div [ css "control" ]
                [ renderInput id value ]
            ]
        , HH.div [ css "field is-grouped" ]
            [ HH.div [ css "control" ]
                [ HH.button [ css "button is-link" ] [ HH.text "Submit" ]
                ]
            , HH.div [ css "control" ]
                [ HH.a
                    [ css "button is-link is-light", HE.onClick \e -> LogState ]
                    [ HH.text "Log state" ]
                ]
            ]
        ]
    )

renderTask (Pair t1 t2) =
  HH.div
    [ css "columns" ]
    [ HH.div [ css "column" ] [ renderTask t1 ]
    , HH.div [ css "column" ] [ renderTask t2 ]
    ]

renderInput :: forall a. Id -> Value -> HH.HTML a Action
renderInput id (Value value String) =
  HH.input
    [ css "input"
    , HP.value value
    , HE.onValueInput \s -> UpdateValue id s
    ]

renderInput id (Value value Int) =
  HH.input
    [ css "input"
    , HP.value value
    , HE.onValueInput \s -> UpdateValue id s
    , HP.type_ HP.InputNumber
    ]

renderInput id (Value value Boolean) =
  HH.label
    [ css "checkbox" ]
    [ HH.input
        [ css "checkbox"
        , HP.checked $ value == "True"
        , HP.type_ HP.InputCheckbox
        , HE.onChange \e -> UpdateValue id (toggle value)
        ]
    , HH.text "Enabled"
    ]
  where
  toggle "True" = "False"

  toggle "False" = "True"

  toggle x = x
