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
import App.Task (Editor(..), Input(..), InputDescription(..), Name(..), Task(..), Value(..), hasLabel, isDecide, selectInputDescription)
import Component.HTML.Bulma as Bulma
import Component.HTML.Form as Form
import Component.HTML.Utils (css)
import Data.Array (filter, find, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (disabled)
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Type.Proxy (Proxy(..))
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

-- Because our form components can return any type, we have to define a slot
-- type for every scalar value we need.
type Slots
  = ( formInt :: forall query. H.Slot query Int Int
    , formString :: forall query. H.Slot query String Int
    , formBoolean :: forall query. H.Slot query Boolean Int
    )

_formInt = Proxy :: Proxy "formInt"

_formString = Proxy :: Proxy "formString"

_formBoolean = Proxy :: Proxy "formBoolean"

-- The State holds a boolean to determine if a task is loading. It also holds
-- the tasks that is rendered, and the types of Enter editors
-- (inputDescriptions).
type State
  = { isLoading :: Boolean
    , currentTask :: Maybe Task
    , inputDescriptions :: Array InputDescription
    }

data Action
  = Init
  | HandleKey H.SubscriptionId KeyboardEvent
  | Interact Input
  | LogState -- For debug purposes...
  | Reset

taskLoader :: forall query input output m. MonadAff m => H.Component query input output m
taskLoader = do
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }
  where
  initialState _ =
    { isLoading: true
    , currentTask: Nothing
    , inputDescriptions: []
    }

-- function that defines actions.
handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction Init = do
  fetchInitialTask
  addKeyListener
  where
  fetchInitialTask = do
    taskResp <- H.liftAff getInitialTask
    setFromTaskResponse taskResp
    H.modify_ \s -> s { isLoading = false }

  addKeyListener = do
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)

-- Pressing enter defaults to pressing continue (if present).
handleAction (HandleKey _ ev) = do
  when (KE.key ev == "Enter") do
    { inputDescriptions: inputDescriptions } <- H.get
    let
      specialActions = filter isDecide inputDescriptions
    case head specialActions of
      Just (DecideDescription id label) -> handleAction (Interact (Decide id label))
      _ -> pure unit

handleAction Reset = do
  taskResp <- H.liftAff reset
  setFromTaskResponse taskResp

handleAction LogState = H.get >>= logShow

handleAction (Interact input) = do
  taskResp <- H.liftAff $ interact input
  setFromTaskResponse taskResp

setFromTaskResponse :: forall output m. MonadAff m => Either ApiError TaskResponse -> H.HalogenM State Action Slots output m Unit
setFromTaskResponse taskResp = case taskResp of
  Left err -> logShow err
  Right (TaskResponse task inputs) ->
    H.modify_ \s ->
      s
        { currentTask = Just task
        , inputDescriptions = inputs
        }

-- Function that renders the user interface. Takes a state as argument.
render :: forall m. MonadAff m => State -> HH.ComponentHTML Action Slots m
render state = case state of
  { isLoading: true } -> renderLoadingScreen
  { currentTask: Just task
  , inputDescriptions: inputDescriptions
  } -> renderTaskWithInputs task inputDescriptions
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
renderTaskWithInputs :: forall m. MonadAff m => Task -> Array InputDescription -> HH.ComponentHTML Action Slots m
renderTaskWithInputs task inputDescriptions =
  HH.div_
    [ renderTask task inputDescriptions, renderInputs inputDescriptions ]

-- Render user interface for each support task type. Takes a task and an array
-- of InputDescription as arguments. The difference between the rendering of
-- Update and Enter tasks: the predefined values.
renderTask :: forall m. MonadAff m => Task -> Array InputDescription -> HH.ComponentHTML Action Slots m
renderTask (Edit name@(Named id) (Update value)) _ =
  Bulma.panel ("Update Task [" <> show name <> "]")
    ( HH.div [ css "control" ]
        [ HH.div [ css "field" ]
            [ HH.label_ [ HH.text ("Value: ") ]
            , HH.div [ css "control" ]
                [ renderEditor id value ]
            ]
        ]
    )

renderTask (Edit name (View value)) _ =
  Bulma.panel ("View Task [" <> show name <> "]")
    (HH.p [ css "preserve-linebreak" ] [ HH.text $ show value ])

renderTask (Edit name@(Named id) Enter) inputDescriptions =
  let
    -- Function that selects the corresponding element of the inputDescriptions array. The value (String) is used to determine the type of the editor.
    inputDescriptionWanted :: String
    inputDescriptionWanted = case selectInputDescription id inputDescriptions of
      InsertDescription _ value -> value
      DecideDescription _ _ -> "Should not be possible?"

    -- Auxiliary function that is needed to determine the type of the editor. Initial Values: right type, dummy values.
    typeOfEditor :: Value
    typeOfEditor = case inputDescriptionWanted of
      "<Text>" -> String ""
      "<Int>" -> Int 0
      "<Bool>" -> Boolean false
      _ -> String "should not be possible?"
  in
    Bulma.panel ("Enter Task [" <> show name <> "]")
      ( HH.div [ css "control" ]
          [ HH.div [ css "field" ]
              [ HH.label_ [ HH.text ("Value: ") ]
              , HH.div [ css "control" ]
                  [ renderEditorEnter id typeOfEditor ]
              ]
          ]
      )

renderTask (Edit name@(Named id) (Change value)) _ =
  Bulma.panel ("Change Task [" <> show name <> "]")
    ( HH.div [ css "control" ]
        [ HH.div [ css "field" ]
            [ HH.label_ [ HH.text ("Value: ") ]
            , HH.div [ css "control" ]
                [ renderEditor id value ]
            ]
        ]
    )

renderTask (Edit name (Watch value)) _ =
  Bulma.panel ("Watch Task [" <> show name <> "]")
    (HH.p [ css "preserve-linebreak" ] [ HH.text $ show value ])

renderTask (Edit Unnamed _) _ = HH.p_ [ HH.text "An unnamed editor should not be possible?" ]

renderTask (Select name task labels) inputDescriptions =
  let
    nameHasId id = case name of
      Unnamed -> false
      (Named id') -> id == id'

    matches label inputDescription = case inputDescription of
      (InsertDescription _ _) -> false
      (DecideDescription id' label') -> nameHasId id' && label' == label

    labelToInput label = case find (matches label) inputDescriptions of
      Just input -> input
      Nothing -> (DecideDescription (-1) label)

    buttons = map (renderInput <<< labelToInput) labels
  in
    case task of
      Done ->
        Bulma.panel ("Select Task [" <> show name <> "]")
          (HH.div [ css "buttons is-right" ] buttons)
      _ ->
        HH.div [ css "select-task" ]
          [ renderTask task inputDescriptions
          , HH.div [ css "buttons is-right pt-1" ] buttons
          ]

renderTask (Pair t1 t2) inputDescriptions =
  HH.div
    [ css "columns" ]
    [ HH.div [ css "column" ] [ renderTask t1 inputDescriptions ]
    , HH.div [ css "column" ] [ renderTask t2 inputDescriptions ]
    ]

renderTask (Choose t1 t2) inputDescriptions =
  HH.div
    [ css "columns" ]
    [ HH.div [ css "column" ] [ renderTask t1 inputDescriptions ]
    , HH.div [ css "divider is-vertical" ] [ HH.text "OR" ]
    , HH.div [ css "column" ] [ renderTask t2 inputDescriptions ]
    ]

renderTask (Step t) inputDescriptions = renderTask t inputDescriptions

renderTask (Done) _ =
  Bulma.panel ("Done task")
    (HH.p_ [ HH.text $ show Done ])

renderTask (Fail) _ =
  Bulma.panel ("Fail task")
    (HH.p_ [ HH.text $ show Fail ])

-- Function that renders editors of Update tasks.
renderEditor :: forall m. MonadAff m => Int -> Value -> HH.ComponentHTML Action Slots m
renderEditor id value = case value of
  (String s) -> textInput id $ Just s
  (Int i) -> intInput id $ Just i
  (Boolean b) -> booleanInput id $ Just b

-- Function that renders editors of Enter tasks.
renderEditorEnter :: forall m. MonadAff m => Int -> Value -> HH.ComponentHTML Action Slots m
renderEditorEnter id value = case value of
  (String _) -> textInput id Nothing
  (Int _) -> intInput id Nothing
  (Boolean _) -> booleanInput id Nothing

-- Function that renders buttons that do not belong to a Select task.
renderInputs :: forall a. Array InputDescription -> HH.HTML a Action
renderInputs inputDescriptions = HH.div [ css "buttons is-right" ] renderActionButtons

-- Function that renders buttons that belong to a Select task.
renderInput :: forall a. InputDescription -> HH.HTML a Action
renderInput (DecideDescription id label) =
  HH.button
    [ css "button is-primary"
    , case id of
        (-1) -> disabled true
        _ -> HE.onClick \_ -> Interact (Decide id label)
    ]
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

-- Each form field is a child component that manages its owns state. An output
-- event is only fired when the provided value is valid.
textInput :: forall m. MonadAff m => Int -> Maybe String -> HH.ComponentHTML Action Slots m
textInput id value =
  HH.slot
    _formString
    id
    Form.component
    (Form.textInput value)
    (\s -> Interact (Insert id (String s)))

intInput :: forall m. MonadAff m => Int -> Maybe Int -> HH.ComponentHTML Action Slots m
intInput id value =
  HH.slot
    _formInt
    id
    Form.component
    (Form.intInput value)
    (\i -> Interact (Insert id (Int i)))

booleanInput :: forall m. MonadAff m => Int -> Maybe Boolean -> HH.ComponentHTML Action Slots m
booleanInput id value =
  HH.slot
    _formBoolean
    id
    Form.component
    (Form.booleanInput value)
    (\b -> Interact (Insert id (Boolean b)))
