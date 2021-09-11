{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fail where

import Task.Syntax (Task (Fail))
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main = visualizeTask (Fail :: Task h Int)
