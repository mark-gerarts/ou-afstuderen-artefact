{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Pair where

import Task (update)
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main =
  visualizeTask
    <| update ("Left" :: Text) >< update ("Right" :: Text)
