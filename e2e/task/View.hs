{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module View where

import Task (view)
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main =
  visualizeTask <| view ("Some value" :: Text)
