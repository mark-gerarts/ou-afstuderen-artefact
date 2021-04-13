module Component.HTML.Utils where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- @see https://github.com/thomashoneyman/purescript-halogen-realworld/blob/main/src/Component/HTML/Utils.purs
css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName
