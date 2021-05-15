module Main where

import Task (Task, enter, update, view, (>>?))
import Visualize (visualizeTask)

main :: IO ()
main = visualizeTask exampleTask

exampleTask :: Task h Text
exampleTask =
  update ("Hello!" :: Text) >< (update True >< update (1 :: Int)) >>? \(t, (b, i)) ->
    view
      <| unwords
        [ "The left value was \"",
          t,
          "\", the right value was",
          display (b, i)
        ]
