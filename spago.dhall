{ name = "near-sdk-purs"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "argonaut-codecs"
  , "argonaut-core"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "lists"
  , "maybe"
  , "prelude"
  , "record"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
