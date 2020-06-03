{-
Welcome to a Spago project!
You can edit this file as you like.

Authors:
  "Dave Zuch <https://github.com/whoadave>",
  "Chris Cornwell <https://github.com/crcornwell>",
  "Thomas Honeyman <https://github.com/thomashoneyman>",
  "Forest Toney <https://github.com/foresttoney>",
  "Hardy Jones <https://github.com/joneshf-cn>",
  "Vance Palacio <https://github.com/vanceism7>"
-}

{ name = "ocelot"
, dependencies =
  [ "aff-promise"
  , "affjax"
  , "argonaut"
  , "bigints"
  , "console"
  , "debug"
  , "effect"
  , "email-validate"
  , "formatters"
  , "fuzzy"
  , "halogen"
  , "halogen-renderless"
  , "halogen-select"
  , "halogen-storybook"
  , "html-parser-halogen"
  , "js-timers"
  , "numbers"
  , "psci-support"
  , "read"
  , "remotedata"
  , "svg-parser"
  , "svg-parser-halogen"
  , "test-unit"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
