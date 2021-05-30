{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UpdateBoolean where

import Task (update)
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main = visualizeTask <| update True
