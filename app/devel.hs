{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

import "tophat" Task (Task, update, view, (>>?), enter, Task(Done), Task(Fail),(<?>), select, Label)
import "ou-afstuderen-artefact" Visualize (visualizeTaskDevel)
import "tophat" Prelude
import qualified Data.HashMap.Strict as HashMap

-- This file is used for development purposes in combination with yesod-devel.
main :: IO ()
main = visualizeTaskDevel pick3'

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
    view(x)

pick2 :: Task h Int
pick2 = view 1 <?> view 2

hashmap :: HashMap Label (Task h Int)
hashmap = HashMap.insert ("B"::Label) (view (22::Int)) (HashMap.insert ("A"::Label) (view (11::Int)) HashMap.empty)

pick3' :: Task h Int
pick3' = select hashmap

-- Multiplication-by-seven machine

multiplication:: Int -> Int -> Task h Int
multiplication x y = view (x*y)

multBySeven:: Int -> Task h Int
multBySeven x = multiplication x 7


multBySevenMachine :: Task h Int
multBySevenMachine = enter >>? \x ->
      multBySeven x 