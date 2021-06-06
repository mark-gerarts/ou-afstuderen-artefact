module Component.TaskLoader (taskLoader) where

import Prelude
import App.Client (ApiError, TaskResponse(..), getInitialTask, interact, reset)
import App.Task (Editor(..), Input(..), InputDescription(..), Name(..), Task(..), Value(..), isOption, isSelectedInputDescription, isUnnamed, selectInputDescription)
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

type State
  = { isLoading :: Boolean
    , currentTask :: Maybe Task
    , inputDescriptions :: Array InputDescription
    }

data Action
  = FetchInitialTask
  | Interact Input
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
    , inputDescriptions: []
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

handleAction (Interact input) = do
  taskResp <- H.liftAff $ interact input
  setFromTaskResponse taskResp

setFromTaskResponse :: forall output m. MonadAff m => Either ApiError TaskResponse -> H.HalogenM State Action () output m Unit
setFromTaskResponse taskResp = case taskResp of
  Left err -> logShow err
  Right (TaskResponse task inputs) ->
    H.modify_ \s ->
      s
        { currentTask = Just task
        , inputDescriptions = inputs
        }

render :: forall a. State -> HH.HTML a Action
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

renderTaskWithInputs :: forall a. Task -> Array InputDescription -> HH.HTML a Action
renderTaskWithInputs task inputDescriptions =
  HH.div_
    [ renderTask task inputDescriptions, renderInputs inputDescriptions ]

renderTask :: forall a. Task -> Array InputDescription -> HH.HTML a Action
renderTask (Edit name@(Named id) (Update value)) _ =
  Bulma.panel ("Update Task [" <> show name <> "]")
    ( HH.div [ css "field" ]
        [ HH.label_ [ HH.text ("Value: ") ]
        , HH.div [ css "control" ]
            [ renderEditor id value ]
        ]
    )

renderTask (Edit name (View value)) _ =
  Bulma.panel ("View Task [" <> show name <> "]")
    (HH.p_ [ HH.text $ show value ])

renderTask (Edit name@(Named id) Enter) inputDescriptions =
  let
    inputDescriptionWanted :: String
    inputDescriptionWanted = case selectInputDescription id inputDescriptions of
      InsertDescription _ value -> value
      OptionDescription _ _ -> "Should not be possible?"

    typeOfEditor :: Value
    typeOfEditor = case inputDescriptionWanted of
      -- Initial Values: right type, dummy values
      "<Text>" -> String ""
      "<Int>" -> Int 0
      "<Bool>" -> Boolean false
      _ -> String "should not be possible?"
  in
    Bulma.panel ("Enter Task [" <> show name <> "]")
      ( HH.div [ css "field" ]
          [ HH.label_ [ HH.text ("Value: ") ]
          , HH.div [ css "control" ]
              [ renderEditorEnter id typeOfEditor ]
          ]
      )

renderTask (Edit name Select) inputDescriptions =
  let
    id = case name of
      Unnamed -> -1
      (Named id') -> id'

    options = filter (\x -> isOption x && isSelectedInputDescription id x) inputDescriptions

    buttons = map renderInput options
  in
    Bulma.panel ("Select Task [" <> show name <> "]")
      ( HH.div_
          [ HH.p [ css "my-1" ] [ HH.text "Choose an option below:" ]
          , HH.div [ css "buttons" ] buttons
          ]
      )

renderTask (Edit Unnamed _) _ = HH.p_ [ HH.text "An unnamed editor should not be possible?" ]

renderTask (Pair t1 t2) inputDescriptions =
  HH.div
    [ css "columns" ]
    [ HH.div [ css "column" ] [ renderTask t1 inputDescriptions ]
    , HH.div [ css "column" ] [ renderTask t2 inputDescriptions ]
    ]

renderTask (Step t) inputDescriptions = renderTask t inputDescriptions

renderTask (Done) _ =
  Bulma.panel ("Done task")
    (HH.p_ [ HH.text $ show Done ])

renderTask (Fail) _ =
  Bulma.panel ("Fail task")
    (HH.p_ [ HH.text $ show Fail ])

renderEditor :: forall a. Int -> Value -> HH.HTML a Action
renderEditor id (String value) =
  textInput
    (Just value)
    (\s -> Interact (Insert id (String s)))

renderEditor id (Int value) =
  integerInput
    (Just value)
    (\i -> Interact (Insert id (Int i)))

renderEditor id (Boolean value) =
  booleanInput
    (Just value)
    (\b -> Interact (Insert id (Boolean b)))

renderEditorEnter :: forall a. Int -> Value -> HH.HTML a Action
renderEditorEnter id (String _) =
  textInput
    Nothing
    (\s -> Interact (Insert id (String s)))

renderEditorEnter id (Int _) =
  integerInput
    Nothing
    (\i -> Interact (Insert id (Int i)))

renderEditorEnter id (Boolean _) =
  booleanInput
    Nothing
    (\b -> Interact (Insert id (Boolean b)))

renderInputs :: forall a. Array InputDescription -> HH.HTML a Action
renderInputs inputDescriptions =
  let
    unnamedOptions = filter (\x -> isOption x && isUnnamed x) inputDescriptions

    buttons = renderActionButtons <> map renderInput unnamedOptions
  in
    HH.div [ css "buttons is-right" ] buttons

renderInput :: forall a. InputDescription -> HH.HTML a Action
renderInput (OptionDescription name label) =
  HH.button
    [ css "button is-primary", HE.onClick \_ -> Interact (Option name label) ]
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
