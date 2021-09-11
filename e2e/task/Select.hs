{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Select where

import Task (pick, view, (>>?))
import Task.Syntax (Task)
import Visualize (visualizeTask)
import "tophat" Prelude

-- A pair of selects so we can test if the options are assigned to the correct
-- task. Furthermore, `pickC` is a Step, so we can check if the "Continue"
-- button is rendered outside of the task.
main :: IO ()
main =
  visualizeTask
    <| pickAorB >< pickC

pickAorB :: Task h Text
pickAorB =
  pick
    [ "A" ~> view "Clicked A",
      "B" ~> view "Clicked B"
    ]

pickC :: Task h Text
pickC = pick ["C" ~> view ("Clicked C" :: Text) >>? \_ -> view "Continued"]
