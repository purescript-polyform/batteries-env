{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "identity"
  , "maybe"
  , "ordered-collections"
  , "polyform"
  , "polyform-batteries-core"
  , "prelude"
  , "psci-support"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
