{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Done where

import Task (Task (..))
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main = visualizeTask <| Done ()
