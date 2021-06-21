{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Step where

import Task (update, view, (>>?))
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main =
  visualizeTask
    <| update (1 :: Int) >>? \x -> view (x + 1)
