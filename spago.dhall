{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ dependencies =
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
, license = "BSD-3-Clause"
, name = "my-project"
, packages = ./packages.dhall
, repository = "https://github.com/purescript-polyform/batteries-env.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
