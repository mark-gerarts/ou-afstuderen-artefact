{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Choose where

import Task (enter, pick, update)
import Task.Syntax (Task)
import Visualize (visualizeTask)
import "tophat" Prelude

-- A pair of selects so we can test if the options are assigned to the correct
-- task. Furthermore, `pickC` is a Step, so we can check if the "Continue"
-- button is rendered outside of the task.
main :: IO ()
main =
  visualizeTask chooseCountry

chooseCountry :: Task h Text
chooseCountry =
  pick
    [ "The Netherlands" ~> update "The Netherlands",
      "Belgium" ~> update "Belgium"
    ]
    <|> enter
