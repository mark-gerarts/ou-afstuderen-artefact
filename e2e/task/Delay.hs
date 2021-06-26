{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Step where

import Task (Task (..), enter, view)
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main =
  visualizeTask <| do
    x <- enterInt
    viewComparison x

enterInt :: Task h Int
enterInt = enter

viewComparison :: Int -> Task h Text
viewComparison x = if x >= 10 then view "x >= 10" else view "x < 10"
