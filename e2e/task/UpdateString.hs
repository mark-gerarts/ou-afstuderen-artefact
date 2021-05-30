{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UpdateString where

import Task (update)
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main = visualizeTask <| update ("Hello" :: Text)
