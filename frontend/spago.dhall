{ name = "halogen-project"
, dependencies = [ "console", "effect", "halogen", "psci-support", "affjax", "argonaut" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
