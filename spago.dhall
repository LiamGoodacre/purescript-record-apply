{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "demo"
, dependencies =
    [ "console", "effect", "psci-support", "record", "typelevel-prelude" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
