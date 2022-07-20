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
  "Gabe Johnson <https://github.com/citizengabe>"
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
  , "halogen-storybook"
  , "halogen-svg-elems"
  , "halogen-hooks"
  , "html-parser-halogen"
  , "js-timers"
  , "js-uri"
  , "numbers"
  , "psci-support"
  , "read"
  , "remotedata"
  , "svg-parser-halogen"
  , "test-unit"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
