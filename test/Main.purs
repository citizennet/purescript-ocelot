module Test.Main where

import Prelude
import Effect (Effect)
import Test.Ocelot.Data.Currency as Test.Ocelot.Data.Currency
import Test.Ocelot.Data.IntervalTree as Test.Ocelot.Data.IntervalTree
import Test.Ocelot.HTML.Properties as Test.Ocelot.HTML.Properties
import Test.Ocelot.TimePicker as Test.Ocelot.TimePicker
import Test.Ocelot.Typeahead as Test.Ocelot.Typeahead
import Test.Unit as Test.Unit
import Test.Unit.Main as Test.Unit.Main

main :: Effect Unit
main = Test.Unit.Main.runTest do
  Test.Unit.suite "Ocelet.Data.Currency" Test.Ocelot.Data.Currency.suite
  Test.Ocelot.Data.IntervalTree.suite
  Test.Ocelot.HTML.Properties.suite
  Test.Ocelot.TimePicker.suite
  Test.Ocelot.Typeahead.suite

