module Application (application) where

import qualified Data.HashMap.Strict as HM
import Network.HTTP.Types (status200)
import Network.Wai (Response, responseLBS)
import Task (Task, select, update, view)

application :: p -> (Response -> t) -> t
application _ respond =
  respond
    <| responseLBS status200 [("Content-Type", "text/plain")]
    <| encodeUtf8
    <| display (inc 2)
  where
    inc :: Int -> Task h Int
    inc x =
      view (x + 1)

    dec :: Int -> Task h Int
    dec x =
      view (x - 1)

    oneStep :: Task h Int
    oneStep = do
      x <- update 0
      select
        <| HM.fromList
          [ "Increase" ~> inc x,
            "Decrease" ~> dec x
          ]
