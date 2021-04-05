module Component.TaskLoader (taskLoader) where

import Prelude
import App.Client (getCurrentTask, interact)
import App.Task (Input(..), Task(..), updateTask)
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
import Component.HTML.Bulma as Bulma

type State
  = { isLoading :: Boolean
    , currentTask :: Maybe Task
    }

data Action
  = FetchCurrentTask
  | UpdateValue String
  | Interact Event
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

handleAction (Interact event) = do
  H.liftEffect $ Event.preventDefault event
  s <- H.get
  case s.currentTask of
    Nothing -> logShow "Error"
    Just (Update id value) -> do
      let
        input = Input id value
      r <- H.liftAff $ interact input
      case r of
        Left err -> logShow err
        Right task -> H.modify_ \s -> s { currentTask = Just task }

handleAction (UpdateValue x) = do
  H.modify_ \s -> s { currentTask = (updateTask x) <$> s.currentTask }

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
renderTask (Update id value) =
  Bulma.panel ("Update Task [" <> show id <> "]")
    ( HH.form
        [ HE.onSubmit Interact, css "control" ]
        [ HH.div [ css "field" ]
            [ HH.label_ [ HH.text "Value" ]
            , HH.div [ css "control" ]
                [ HH.input
                    [ css "input"
                    , HP.value value
                    , HE.onValueInput UpdateValue
                    ]
                ]
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
