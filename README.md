
### Installation

In `packages.dhall`:

```dhall
let additions =
  { record-apply =
    { dependencies =
      [ "console"
      , "effect"
      , "psci-support"
      , "record"
      , "typelevel-prelude"
      ]
    , repo = "https://github.com/LiamGoodacre/purescript-record-apply.git"
    , version = "master"
    }
  }
```

### Usage

```purescript
> import Data.Record.Apply (applyRecord)
> { a: \x -> x <> "World", b: (_ > 0) } `applyRecord` { a: "Hello ", b: 7 }
{ a: "Hello World", b: true }
```