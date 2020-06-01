{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}
-------------------------
-- Additional Packages --
-------------------------
let storybook = 
  { dependencies =
    [ "halogen"
    , "routing"
    , "foreign-object"
    ]
  , repo =
      "https://github.com/rnons/purescript-halogen-storybook.git"
  , version =
      "de336410dde6e59ad4930f7e4296d066cb236628"
  }

let html-parser = 
  { dependencies =
    [ "string-parsers"
    , "halogen"
    ]
  , repo =
      "https://github.com/rnons/purescript-html-parser-halogen.git"
  , version =
      "7d37fd6a29bff2a143d91c2ebfe5ca582ca76018"
  }

let svg-parser = 
  { dependencies =
    [ "prelude"
    , "string-parsers"
    , "generics-rep"
    ]
  , repo = 
      "https://github.com/rnons/purescript-svg-parser.git"
  , version =
    "v1.0.0"
  }

let svg-parser-halogen =
  { dependencies =
    [ "svg-parser"
    , "halogen"
    ]
  , repo =
    "https://github.com/rnons/purescript-svg-parser-halogen.git"
  , version =
    "v2.0.0-rc.1"
      -- "ac16a7af82d739b1f9773fbc28fc7e24f0d24550"
  }

let renderless =
  { dependencies =
    [ "prelude"
    , "control"
    ]
  , repo = 
    "https://github.com/purescript-deprecated/purescript-halogen-renderless"
  , version =
    "v0.0.4"
  }

-----------------
-- Main Config --
-----------------

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200507/packages.dhall sha256:9c1e8951e721b79de1de551f31ecb5a339e82bbd43300eb5ccfb1bf8cf7bbd62

let overrides = {=}

let additions = 
  { storybook
  , html-parser
  , svg-parser
  , svg-parser-halogen
  , renderless
  }

in  upstream // overrides // additions
