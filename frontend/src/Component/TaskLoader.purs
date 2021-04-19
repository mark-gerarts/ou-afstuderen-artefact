module Component.TaskLoader (taskLoader) where

import Prelude
import App.Client (TaskResponse(..), getInitialTask, interact)
import App.Task (Editor(..), Input(..), Name(..), Task(..), Value(..), isOption, updateTask)
import Component.HTML.Bulma as Bulma
import Component.HTML.Utils (css)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)

type State
  = { isLoading :: Boolean
    , currentTask :: Maybe Task
    , possibleInputs :: Array Input
    }

data Action
  = FetchInitialTask
  | UpdateValue Name Value
  | Interact Input Event
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
              , initialize = Just FetchInitialTask
              }
    }
  where
  initialState _ =
    { isLoading: true
    , currentTask: Nothing
    , possibleInputs: []
    }

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction FetchInitialTask = do
  taskResp <- H.liftAff $ getInitialTask
  case taskResp of
    Left err -> logShow err
    Right (TaskResponse task inputs) ->
      H.modify_ \s ->
        s
          { currentTask = Just task
          , possibleInputs = inputs
          }
  H.modify_ \s -> s { isLoading = false }

handleAction LogState = H.get >>= logShow

handleAction (Interact input event) = do
  H.liftEffect $ Event.preventDefault event
  s <- H.get
  taskResp <- H.liftAff $ interact input
  -- @todo: get rid of the copy paste, introduce a new message?
  case taskResp of
    Left err -> logShow err
    Right (TaskResponse task inputs) ->
      H.modify_ \s ->
        s
          { currentTask = Just task
          , possibleInputs = inputs
          }

handleAction (UpdateValue id x) = do
  H.modify_ \s -> s { currentTask = (updateTask id x) <$> s.currentTask }

render :: forall a. State -> HH.HTML a Action
render state = case state of
  { isLoading: true } -> renderLoadingScreen
  { currentTask: Just task, possibleInputs: inputs } -> renderTaskWithInputs task inputs
  _ -> renderError

renderLoadingScreen :: forall a. HH.HTML a Action
renderLoadingScreen =
  HH.div_
    [ HH.h1 [ css "title" ] [ HH.text "Loading" ]
    , HH.p [ css "subtitle" ] [ HH.text "Fetching current task from the server..." ]
    ]

renderError :: forall a. HH.HTML a Action
renderError = HH.p_ [ HH.text "An error occurred :(" ]

renderTaskWithInputs :: forall a. Task -> Array Input -> HH.HTML a Action
renderTaskWithInputs task inputs =
  HH.div_
    [ renderTask task, renderInputs inputs ]

renderTask :: forall a. Task -> HH.HTML a Action
renderTask task@(Edit name@(Named id) (Update value)) =
  Bulma.panel ("Update Task [" <> show name <> "]")
    ( HH.form
        [ HE.onSubmit \e -> Interact (Insert id value) e, css "control" ]
        [ HH.div [ css "field" ]
            [ HH.label_ [ HH.text "Value" ]
            , HH.div [ css "control" ]
                [ renderEditor name value ]
            ]
        , HH.div [ css "field is-grouped" ]
            [ HH.div [ css "control" ]
                [ HH.button [ css "button is-link" ] [ HH.text "Submit" ]
                ]
            ]
        ]
    )

renderTask task@(Edit name@(Named id) (View value)) =
  Bulma.panel ("Update Task [" <> show name <> "]")
    (HH.p_ [ HH.text $ show value ])

renderTask (Edit Unnamed _) = HH.p_ [ HH.text "An unnamed editor should not be possible?" ]

renderTask (Pair t1 t2) =
  HH.div
    [ css "columns" ]
    [ HH.div [ css "column" ] [ renderTask t1 ]
    , HH.div [ css "column" ] [ renderTask t2 ]
    ]

renderTask (Step t) = renderTask t

renderEditor :: forall a. Name -> Value -> HH.HTML a Action
renderEditor name (String value) =
  HH.input
    [ css "input"
    , HP.value value
    , HE.onValueInput \s -> UpdateValue name (String s)
    ]

renderEditor name (Int value) =
  HH.input
    [ css "input"
    , HP.value $ show value
    , HE.onValueInput \s -> UpdateValue name (Int $ unsafePartial $ fromJust $ fromString s)
    , HP.type_ HP.InputNumber
    ]

renderEditor name (Boolean value) =
  HH.label
    [ css "checkbox" ]
    [ HH.input
        [ css "checkbox"
        , HP.checked value
        , HP.type_ HP.InputCheckbox
        , HE.onChange \e -> UpdateValue name (Boolean (not value))
        ]
    , HH.text "Enabled"
    ]

renderInputs :: forall a. Array Input -> HH.HTML a Action
renderInputs inputs =
  let
    options = filter isOption inputs

    buttons = [ renderLogStateButton ] <> map renderInput options
  in
    HH.div [ css "buttons is-right" ] buttons

renderInput :: forall a. Input -> HH.HTML a Action
renderInput (Option name label) =
  HH.button
    [ css "button is-primary", HE.onClick \e -> Interact (Option name label) (toEvent e) ]
    [ HH.text label ]

renderInput _ = HH.div_ []

renderLogStateButton :: forall a. HH.HTML a Action
renderLogStateButton =
  HH.button
    [ css "button is-link is-light", HE.onClick \e -> LogState ]
    [ HH.text "Log state" ]
