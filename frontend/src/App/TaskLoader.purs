module App.TaskLoader (taskLoader) where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Component.HTML.Utils (css)
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH

type State
  = { isLoading :: Boolean
    , currentTask :: Maybe Task
    }

type Id
  = Int

data Task
  = Update Id String

instance decodeJsonTask :: DecodeJson Task where
  decodeJson json = do
    obj <- decodeJson json
    value <- obj .: "value"
    id <- obj .: "id"
    pure $ Update id value

instance showTask :: Show Task where
  show (Update id x) = "Update [" <> x <> "] [" <> show id <> "]"

data Action
  = FetchCurrentTask

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
  r <- H.liftAff $ AX.get AXRF.json "http://localhost:3000/current-task"
  case r of
    Left err -> logShow $ AX.printError err
    Right response -> case decodeJson response.body of
      Left err -> logShow err
      Right task -> H.modify_ \s -> s { currentTask = Just task }
  H.modify_ \s -> s { isLoading = false }

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
renderTask task = HH.p_ [ HH.text $ "Task received: " <> show task ]
