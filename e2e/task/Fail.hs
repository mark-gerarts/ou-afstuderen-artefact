{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fail where

import Task (Task (..))
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main = visualizeTask (Fail :: Task h Int)
