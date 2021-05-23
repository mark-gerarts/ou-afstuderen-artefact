module Main where

import Task (Task, enter, update, view, (>>?))
import Visualize (visualizeTask)

main :: IO ()
main = visualizeTask multBySevenMachine

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

-- Multiplication-by-seven machine

multiplication:: Int -> Int -> Task h Int
multiplication x y = view (x*y)

multBySeven:: Int -> Task h Int
multBySeven x = multiplication x 7


multBySevenMachine :: Task h Int
multBySevenMachine = enter >>? \x ->
      multBySeven x 