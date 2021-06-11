{-|
Module      : Component.TaskLoader
Description : Module to render User Interface
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : ...
Maintainer  : sample@email.com
Stability   : experimental

Module to render the User Interface of a task
-}

module Component.TaskLoader (taskLoader) where

import Prelude
import App.Client (ApiError, TaskResponse(..), getInitialTask, interact, reset)
import App.Task (Editor(..), Input(..), InputDescription(..), Name(..), Task(..), Value(..), isOption, isSelectedInputDescription, isUnnamed, selectInput, selectInputDescription, taskToArray, updateInput)
import Component.HTML.Bulma as Bulma
import Component.HTML.Form (booleanInput, integerInput, textInput)
import Component.HTML.Utils (css)
import Data.Array (filter)
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
import Web.UIEvent.MouseEvent (toEvent)

-- The State holds a boolean to determine if a task is loading. It also holds the tasks that is rendered, the inputs of editors (possible inputs) and the types of Enter editors (inputDescriptions).
type State
  = { isLoading :: Boolean
    , currentTask :: Maybe Task
    , possibleInputs :: Array Input
    , inputDescriptions :: Array InputDescription
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
    , inputDescriptions: []
    }

-- function that defines actions.
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
  taskResp <- H.liftAff $ interact input
  setFromTaskResponse taskResp

handleAction (UpdateInput id x) = do
  H.modify_ \s -> s { possibleInputs = updateInput id x <$> s.possibleInputs }

-- function that sets te State.
setFromTaskResponse :: forall output m. MonadAff m => Either ApiError TaskResponse -> H.HalogenM State Action () output m Unit
setFromTaskResponse taskResp = case taskResp of
  Left err -> logShow err
  Right (TaskResponse task inputs) ->
    H.modify_ \s ->
      s
        { currentTask = Just task
        , possibleInputs = taskToArray task []
        , inputDescriptions = inputs
        }

-- Function that renders the user interface. Takes a state as argument.
render :: forall a. State -> HH.HTML a Action
render state = case state of
  { isLoading: true } -> renderLoadingScreen
  { currentTask: Just task
  , possibleInputs: possibleInputs
  , inputDescriptions: inputDescriptions
  } -> renderTaskWithInputs task possibleInputs inputDescriptions
  _ -> renderError

renderLoadingScreen :: forall a. HH.HTML a Action
renderLoadingScreen =
  HH.div_
    [ HH.h1 [ css "title" ] [ HH.text "Loading" ]
    , HH.p [ css "subtitle" ] [ HH.text "Fetching current task from the server..." ]
    ]

renderError :: forall a. HH.HTML a Action
renderError = HH.p_ [ HH.text "An error occurred :(" ]

-- Function that renders the user interface of a given Task. Function also renders buttons that do not belong to Select tasks, like Continue.
renderTaskWithInputs :: forall a. Task -> Array Input -> Array InputDescription -> HH.HTML a Action
renderTaskWithInputs task possibleInputs inputDescriptions =
  HH.div_
    [ renderTask task possibleInputs inputDescriptions, renderInputs inputDescriptions ]

-- Render user interface for each support task type. Takes a task, an array of Input and an array of InputDescription as arguments.
-- The difference between the rendering of Update and Enter tasks: the predefined values.  
renderTask :: forall a. Task -> Array Input -> Array InputDescription -> HH.HTML a Action
renderTask (Edit name@(Named id) (Update value)) possibleInputs _ =
  let
    -- Function that selects the corresponding element of the possibleInputs array. This array is used to store values before the values are sent back to the server.
    inputWanted :: Value
    inputWanted = case selectInput id possibleInputs of
      Insert _ value' -> value'
      Option _ _ -> String "Should not be possible?"
  in
    Bulma.panel ("Update Task [" <> show name <> "]")
      ( HH.form
          [ HE.onSubmit \e -> Interact (Insert id inputWanted) e, css "control" ]
          [ HH.div [ css "field" ]
              [ HH.label_ [ HH.text ("Value: ") ]
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

renderTask (Edit name (View value)) _ _ =
  Bulma.panel ("View Task [" <> show name <> "]")
    (HH.p_ [ HH.text $ show value ])

renderTask (Edit name@(Named id) Enter) possibleInputs inputDescriptions =
  let
    -- Function that selects the corresponding element of the possibleInputs array. This array is used to store values before the values are sent back to the server.
    inputWanted :: Value
    inputWanted = case selectInput id possibleInputs of
      Insert _ value -> value
      Option _ _ -> String "Should not be possible?"

    -- Function that selects the corresponding element of the inputDescriptions array. The value (String) is used to determine the type of the editor.
    inputDescriptionWanted :: String
    inputDescriptionWanted = case selectInputDescription id inputDescriptions of
      InsertDescription _ value -> value
      OptionDescription _ _ -> "Should not be possible?"

    -- Auxiliary function that is needed to determine the type of the editor. Initial Values: right type, dummy values. 
    typeOfEditor :: Value
    typeOfEditor = case inputDescriptionWanted of
      "<Text>" -> String ""
      "<Int>" -> Int 0
      "<Bool>" -> Boolean false
      otherwise -> String "should not be possible?"
  in
    Bulma.panel ("Enter Task [" <> show name <> "]")
      ( HH.form
          [ HE.onSubmit \e -> Interact (Insert id inputWanted) e, css "control" ]
          [ HH.div [ css "field" ]
              [ HH.label_ [ HH.text ("Value: ") ]
              , HH.div [ css "control" ]
                  [ renderEditorEnter name typeOfEditor ]
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

renderTask (Edit name Select) _ possibleInputs =
  let
    id = case name of
      Unnamed -> -1
      (Named id') -> id'

    options = filter (\x -> isOption x && isSelectedInputDescription id x) possibleInputs

    buttons = map renderInput options
  in
    Bulma.panel ("Select Task [" <> show name <> "]")
      ( HH.div_
          [ HH.p [ css "my-1" ] [ HH.text "Choose an option below:" ]
          , HH.div [ css "buttons" ] buttons
          ]
      )

renderTask (Edit Unnamed _) _ _ = HH.p_ [ HH.text "An unnamed editor should not be possible?" ]

renderTask (Pair t1 t2) possibleInputs inputDescriptions =
  HH.div
    [ css "columns" ]
    [ HH.div [ css "column" ] [ renderTask t1 possibleInputs inputDescriptions ]
    , HH.div [ css "column" ] [ renderTask t2 possibleInputs inputDescriptions ]
    ]

renderTask (Step t) possibleInputs inputDescriptions = renderTask t possibleInputs inputDescriptions

renderTask (Done) _ _ =
  Bulma.panel ("Done task")
    (HH.p_ [ HH.text $ show Done ])

renderTask (Fail) _ _ =
  Bulma.panel ("Fail task")
    (HH.p_ [ HH.text $ show Fail ])

-- Function that renders editors of Update tasks.
renderEditor :: forall a. Name -> Value -> HH.HTML a Action
renderEditor name (String value) =
  textInput
    (Just value)
    (\s -> UpdateInput name (String s))

renderEditor name (Int value) =
  integerInput
    (Just value)
    (\i -> UpdateInput name (Int i))

renderEditor name (Boolean value) =
  booleanInput
    (Just value)
    (\b -> UpdateInput name (Boolean b))

-- Function that renders editors of Enter tasks.
renderEditorEnter :: forall a. Name -> Value -> HH.HTML a Action
renderEditorEnter name (String _) =
  textInput
    Nothing
    (\s -> UpdateInput name (String s))

renderEditorEnter name (Int _) =
  integerInput
    Nothing
    (\i -> UpdateInput name (Int i))

renderEditorEnter name (Boolean _) =
  booleanInput
    Nothing
    (\b -> UpdateInput name (Boolean b))

-- Function that renders buttons that do not belong to a Select task.
renderInputs :: forall a. Array InputDescription -> HH.HTML a Action
renderInputs inputDescriptions =
  let
    unnamedOptions = filter (\x -> isOption x && isUnnamed x) inputDescriptions

    buttons = renderActionButtons <> map renderInput unnamedOptions
  in
    HH.div [ css "buttons is-right" ] buttons

-- Function that renders buttons that belong to a Select task.
renderInput :: forall a. InputDescription -> HH.HTML a Action
renderInput (OptionDescription name label) =
  HH.button
    [ css "button is-primary", HE.onClick \e -> Interact (Option name label) (toEvent e) ]
    [ HH.text label ]

renderInput _ = HH.div_ []

-- Function that renders Reset and Log buttons.
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
