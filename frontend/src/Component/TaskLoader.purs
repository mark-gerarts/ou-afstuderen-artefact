module Component.TaskLoader (taskLoader) where

import Prelude
import App.Client (ApiError, TaskResponse(..), getInitialTask, interact, reset)
import App.Task (Editor(..), Input(..), Name(..), Task(..), Value(..), isOption, updateInput, selectInput)
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
  | UpdateInput Name Value
  | Interact Input Event
  | LogState -- For debug purposes...
  | Reset

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
  taskResp <- H.liftAff getInitialTask
  setFromTaskResponse taskResp
  H.modify_ \s -> s { isLoading = false }

handleAction Reset = do
  taskResp <- H.liftAff reset
  setFromTaskResponse taskResp

handleAction LogState = H.get >>= logShow

handleAction (Interact input event) = do
  H.liftEffect $ Event.preventDefault event
  s <- H.get
  taskResp <- H.liftAff $ interact input
  setFromTaskResponse taskResp

handleAction (UpdateInput id x) = do
  H.modify_ \s -> s { possibleInputs = (updateInput id x) <$> s.possibleInputs }  

setFromTaskResponse :: forall output m. MonadAff m => Either ApiError TaskResponse -> H.HalogenM State Action () output m Unit
setFromTaskResponse taskResp = case taskResp of
  Left err -> logShow err
  Right (TaskResponse task inputs) ->
    H.modify_ \s ->
      s
        { currentTask = Just task
        , possibleInputs = inputs
        }

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
    [ renderTask task inputs, renderInputs inputs ]

renderTask :: forall a. Task -> Array Input -> HH.HTML a Action
renderTask task@(Edit name@(Named id) (Update value)) inputs =
  let
    inputWanted:: Value
    inputWanted = case selectInput id inputs of
      Insert id' value' -> value'
      Option _ _ -> String "Should not be possible?"
  in  
    Bulma.panel ("Update Task [" <> show name <> "]")
      ( HH.form
          [ HE.onSubmit \e -> Interact (Insert id inputWanted) e, css "control" ]
          [ HH.div [ css "field" ]
              [ HH.label_ [ HH.text ("Value: " <> show value)]
              , HH.div [ css "control" ]
                  [ renderEditor name value ]
              ]
          , HH.div [ css "field is-grouped" ]
              [ HH.div [ css "control" ]
                  [ HH.button
                      [ css "button is-link btn-update-submit" ]
                      [ HH.text "Submit" ]
                  ]
              ]
          ]
      )

renderTask task@(Edit name@(Named id) (View value)) inputs =
  Bulma.panel ("Update Task [" <> show name <> "]")
    (HH.p_ [ HH.text $ show value ])

renderTask (Edit Unnamed _) inputs = HH.p_ [ HH.text "An unnamed editor should not be possible?" ]

renderTask (Pair t1 t2) inputs =
  HH.div
    [ css "columns" ]
    [ HH.div [ css "column" ] [ renderTask t1 inputs ]
    , HH.div [ css "column" ] [ renderTask t2 inputs ]
    ]

renderTask (Step t) inputs = renderTask t inputs

renderEditor :: forall a. Name -> Value -> HH.HTML a Action
renderEditor name (String value) =
  HH.input
    [ css "input"
    , HP.value value
    , HE.onValueInput \s -> UpdateInput name (String s)
    , HP.type_ HP.InputText
    ]

renderEditor name (Int value) =
  HH.input
    [ css "input"
    , HP.value $ show value
    , HE.onValueInput \s -> UpdateInput name (Int $ unsafePartial $ fromJust $ fromString s)
    , HP.type_ HP.InputNumber
    ]

renderEditor name (Boolean value) =
  HH.label
    [ css "checkbox" ]
    [ HH.input
        [ css "checkbox"
        , HP.checked value
        , HP.type_ HP.InputCheckbox
        , HE.onChange \_ -> UpdateValue name (Boolean (not value))
        ]
    , HH.text "Enabled"
    ]

renderInputs :: forall a. Array Input -> HH.HTML a Action
renderInputs inputs =
  let
    options = filter isOption inputs

    buttons = renderActionButtons <> map renderInput options
  in
    HH.div [ css "buttons is-right" ] buttons

renderInput :: forall a. Input -> HH.HTML a Action
renderInput (Option name label) =
  HH.button
    [ css "button is-primary", HE.onClick \e -> Interact (Option name label) (toEvent e) ]
    [ HH.text label ]

renderInput _ = HH.div_ []

renderActionButtons :: forall a. Array (HH.HTML a Action)
renderActionButtons =
  [ HH.button
      [ css "button is-danger is-outlined"
      , HP.id "btn-reset"
      , HE.onClick \_ -> Reset
      ]
      [ HH.text "Reset" ]
  , HH.button
      [ css "button is-link is-outlined"
      , HP.id "btn-log"
      , HE.onClick \_ -> LogState
      ]
      [ HH.text "Log state" ]
  ]
