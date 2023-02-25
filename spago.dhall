{ name = "my-project"
, license = "BSD-3-Clause"
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
, repository = "https://github.com/purescript-polyform/batteries-env.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
