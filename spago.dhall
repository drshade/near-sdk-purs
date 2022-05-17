{ name = "near-sdk-purs"
, dependencies =
  [ "aff", "console", "effect", "either", "prelude", "transformers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
