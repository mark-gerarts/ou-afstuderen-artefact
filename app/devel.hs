{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

import "tophat" Task (Task, update, view, (>>?), enter, Task(Done), Task(Fail))
import "ou-afstuderen-artefact" Visualize (visualizeTaskDevel)
import "tophat" Prelude

-- This file is used for development purposes in combination with yesod-devel.
main :: IO ()
main = visualizeTaskDevel multBySevenMachine

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