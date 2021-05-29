{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Step where

import Task (Task (..), select, view, (>>?))
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
  select
    [ "A" ~> view "Clicked A",
      "B" ~> view "Clicked B"
    ]

pickC :: Task h Text
pickC = select ["C" ~> view ("Clicked C" :: Text) >>? \_ -> view "Continued"]
