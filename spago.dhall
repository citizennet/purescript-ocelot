{-
Welcome to a Spago project!
You can edit this file as you like.
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
  , "halogen-select"
  , "html-parser-halogen"
  , "js-timers"
  , "numbers"
  , "psci-support"
  , "read"
  , "remotedata"
  , "renderless"
  , "storybook"
  , "svg-parser"
  , "svg-parser-halogen"
  , "test-unit"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
