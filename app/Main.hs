module Main where

import qualified Data.HashMap.Strict as HashMap
import Task (Label, Task, enter, select, update, view, (<?>), (>>?))
import Visualize (visualizeTask)

main :: IO ()
main = visualizeTask multBySevenMachine

--Example tasks

stepViewUpdate :: Task h Text
stepViewUpdate =
  update ("Hello!" :: Text) >< (update True >< update (1 :: Int)) >>? \(t, (b, i)) ->
    view
      <| unwords
        [ "The left value was \"",
          t,
          "\", the right value was",
          display (b, i)
        ]

oneStep' :: Task h Int
oneStep' =
  update 0 >>? \x ->
    view x

pick2 :: Task h Int
pick2 = view 1 <?> view 2

hashmap :: HashMap Label (Task h Int)
hashmap = HashMap.insert ("Left" :: Label) (view (1 :: Int)) (HashMap.insert ("Right" :: Label) (view (2 :: Int)) HashMap.empty)

pick3' :: Task h Int
pick3' = select hashmap

-- Multiplication-by-seven machine

multiplication :: Int -> Int -> Task h Int
multiplication x y = view (x * y)

multBySeven :: Int -> Task h Int
multBySeven x = multiplication x 7

multBySevenMachine :: Task h Int
multBySevenMachine =
  enter >>? \x ->
    multBySeven x
