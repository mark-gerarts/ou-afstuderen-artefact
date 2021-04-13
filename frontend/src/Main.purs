module Main where

import Prelude
import Component.TaskLoader (taskLoader)
import Data.Foldable (for_)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main =
  HA.runHalogenAff do
    entryPoint <- HA.selectElement $ QuerySelector "#halogen-app"
    -- @todo: we ignore all errors by using for_
    for_ entryPoint (runUI taskLoader unit)
