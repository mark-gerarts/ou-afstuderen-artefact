{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UpdateInt where

import Task (update)
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main = visualizeTask <| update (42 :: Int)
