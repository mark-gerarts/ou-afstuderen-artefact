{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "datetime"
  , "math"
  , "now"
  , "halogen"
  , "halogen-subscriptions"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "web-dom"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
