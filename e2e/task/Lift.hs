{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Done where

import Task.Syntax (Task (..))
import Visualize (visualizeTask)
import "tophat" Prelude

main :: IO ()
main = visualizeTask <| Lift ()
