module Component.HTML.Bulma where

import Component.HTML.Utils (css)
import Halogen.HTML as HH

panel :: forall a b. String -> HH.HTML a b -> HH.HTML a b
panel title content =
  HH.article [ css "panel is-info" ]
    [ HH.p [ css "panel-heading" ] [ HH.text title ]
    , HH.div [ css "panel-block" ] [ content ]
    ]
