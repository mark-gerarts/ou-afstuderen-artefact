module Component.TaskLoader (taskLoader) where

import Prelude

import App.Client (ApiError, TaskResponse(..), getInitialTask, interact, reset)
import App.Task (Editor(..), Input(..), InputDescription(..), Name(..), Task(..), Value(..), isOption, selectInput, taskToArray, updateInput, selectInputDescription)
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
  H.modify_ \s -> s { possibleInputs = (updateInput id x) <$> s.possibleInputs }  

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


render :: forall a. State -> HH.HTML a Action
render state = case state of
  { isLoading: true } -> renderLoadingScreen
  { currentTask: Just task, possibleInputs: possibleInputs, inputDescriptions: inputDescriptions } -> renderTaskWithInputs task possibleInputs inputDescriptions
  _ -> renderError

renderLoadingScreen :: forall a. HH.HTML a Action
renderLoadingScreen =
  HH.div_
    [ HH.h1 [ css "title" ] [ HH.text "Loading" ]
    , HH.p [ css "subtitle" ] [ HH.text "Fetching current task from the server..." ]
    ]

renderError :: forall a. HH.HTML a Action
renderError = HH.p_ [ HH.text "An error occurred :(" ]

renderTaskWithInputs :: forall a. Task -> Array Input -> Array InputDescription -> HH.HTML a Action
renderTaskWithInputs task possibleInputs inputDescriptions =
  HH.div_
    [ renderTask task possibleInputs inputDescriptions, renderInputs inputDescriptions ]

renderTask :: forall a. Task -> Array Input -> Array InputDescription -> HH.HTML a Action
renderTask (Edit name@(Named id) (Update value)) possibleInputs _ =
  let
    inputWanted:: Value
    inputWanted = case selectInput id possibleInputs of
      Insert _ value' -> value'
      Option _ _ -> String "Should not be possible?"
        
  in  
    Bulma.panel ("Update Task [" <> show name <> "]")
      ( HH.form
          [ HE.onSubmit \e -> Interact (Insert id inputWanted) e, css "control" ]
          [ HH.div [ css "field" ]
              [ HH.label_ [ HH.text ("Value: ")]
              , HH.div [ css "control" ]
                  [ renderEditor name value  ]
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
  Bulma.panel ("Update Task [" <> show name <> "]")
    (HH.p_ [ HH.text $ show value ])

renderTask (Edit name@(Named id) Enter) possibleInputs inputDescriptions =
  let
    inputWanted:: Value
    inputWanted = case selectInput id possibleInputs of
      Insert _ value -> value
      Option _ _ -> String "Should not be possible?"  

    inputDescriptionWanted:: String
    inputDescriptionWanted = case selectInputDescription id inputDescriptions of
      InsertDescription _ value -> value
      OptionDescription _ _ -> "Should not be possible?" 

    typeOfEditor:: Value    
    typeOfEditor = case inputDescriptionWanted of
      -- Initial Values: right type, dummy values
      "<Text>" -> String ""
      "<Int>" ->  Int 0
      "<Bool>" -> Boolean false
      otherwise -> String "should not be possible?"
  in 
  Bulma.panel ("Enter Task [" <> show name <> "]")
      ( HH.form
          [ HE.onSubmit \e -> Interact (Insert id inputWanted) e, css "control" ]
          [ HH.div [ css "field" ]
              [ HH.label_ [ HH.text ("Value: ")]
              , HH.div [ css "control" ]
                  [ renderEditorEnter name typeOfEditor  ]
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
    (HH.p_ [ HH.text $ (show Done) ])

renderTask (Fail) _ _ =
  Bulma.panel ("Fail task")
    (HH.p_ [ HH.text $ (show Fail) ])

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
    , HP.value (show value)  
    , HE.onValueInput \s -> UpdateInput name (Int $ unsafePartial $ fromJust $ fromString s)
    , HP.type_ HP.InputNumber
    ]

renderEditor name (Boolean value) =
  HH.div
    []
      [HH.div [] [  
        HH.label
          [ css "checkbox" ]
          [ HH.input
              [ css "checkbox"
              , HP.type_ HP.InputRadio
              , HP.checked (not value)              
              , HP.name "radiobuttonTrueFalse"
              , HE.onChange \_ -> UpdateInput name (Boolean (false))
              ]
          , HH.text "false"
          ]
      ]
      , HH.div [] [  
        HH.label
          [ css "checkbox" ]
          [ HH.input
              [ css "checkbox"
              , HP.type_ HP.InputRadio
              , HP.checked value
              , HP.name "radiobuttonTrueFalse"
              , HE.onChange \_ -> UpdateInput name (Boolean (true))
              ]
          , HH.text "true"
          ]
        ]
      ]

renderEditorEnter :: forall a. Name -> Value -> HH.HTML a Action
renderEditorEnter name (String _) =
  HH.input
    [ css "input"
    , HE.onValueInput \s -> UpdateInput name (String s)
    , HP.type_ HP.InputText
    ]

renderEditorEnter name (Int _) =
  HH.input
    [ css "input"
    , HE.onValueInput \s -> UpdateInput name (Int $ unsafePartial $ fromJust $ fromString s)
    , HP.type_ HP.InputNumber
    ]

renderEditorEnter name (Boolean _) =
  HH.div
    []
      [HH.div [] [  
        HH.label
          [ css "checkbox" ]
          [ HH.input
              [ css "checkbox"
              , HP.type_ HP.InputRadio       
              , HP.name "radiobuttonTrueFalse"
              , HE.onChange \_ -> UpdateInput name (Boolean (false))
              ]
          , HH.text "false"
          ]
      ]
      , HH.div [] [  
        HH.label
          [ css "checkbox" ]
          [ HH.input
              [ css "checkbox"
              , HP.type_ HP.InputRadio
              , HP.name "radiobuttonTrueFalse"
              , HE.onChange \_ -> UpdateInput name (Boolean (true))
              ]
          , HH.text "true"
          ]
        ]
      ]

renderInputs :: forall a. Array InputDescription -> HH.HTML a Action
renderInputs inputDescriptions =
  let
    options = filter isOption inputDescriptions

    buttons = renderActionButtons <> map renderInput options
  in
    HH.div [ css "buttons is-right" ] buttons

renderInput :: forall a. InputDescription -> HH.HTML a Action
renderInput (OptionDescription name label) =
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