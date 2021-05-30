{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EnterString where

import Task (Task (..), enter)
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main = visualizeTask <| (enter :: Task h Text)
