module Test.Ocelot.TimePicker (suite) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Time as Data.Time
import Ocelot.Data.DateTime as Ocelot.Data.DateTime
import Ocelot.TimePicker as Ocelot.TimePicker
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Ocelot.TimePicker" do
    isWithinIntervalTests

isWithinIntervalTests :: Test.Unit.TestSuite
isWithinIntervalTests =
  Test.Unit.suite "isWithinInterval" do
    Test.Unit.test "empty Interval always returns true" do
      -- [-,-]
      let
        interval :: Ocelot.TimePicker.Interval
        interval = { start: Nothing, end: Nothing }

        given :: Data.Time.Time
        given = Ocelot.Data.DateTime.defaultTime
      isWithinInterval interval given true

    Test.Unit.suite "left open interval" do
      -- [12h,-]
      let
        interval :: Ocelot.TimePicker.Interval
        interval =
          { start: Just (Ocelot.Data.DateTime.unsafeMkTime 12 0 0 0)
          , end: Nothing
          }
      Test.Unit.test "on" do -- [12h
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 12 0 0 0
        isWithinInterval interval given true
      Test.Unit.test "within" do -- [12h, 13h
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 13 0 0 0
        isWithinInterval interval given true
      Test.Unit.test "outside" do -- 0h [12h,
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 0 0 0 0
        isWithinInterval interval given false

    Test.Unit.suite "right open interval" do
      -- [-,12h]
      let
        interval :: Ocelot.TimePicker.Interval
        interval =
          { start: Nothing
          , end: Just (Ocelot.Data.DateTime.unsafeMkTime 12 0 0 0)
          }
      Test.Unit.test "on" do -- 12h]
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 12 0 0 0
        isWithinInterval interval given true
      Test.Unit.test "within" do -- 11h ,12h]
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 11 0 0 0
        isWithinInterval interval given true
      Test.Unit.test "outside" do -- ,12h] 13h
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 13 0 0 0
        isWithinInterval interval given false

    Test.Unit.suite "closed interval" do
      -- [6h,18h]
      let
        interval :: Ocelot.TimePicker.Interval
        interval =
          { start: Just (Ocelot.Data.DateTime.unsafeMkTime 6 0 0 0)
          , end: Just (Ocelot.Data.DateTime.unsafeMkTime 18 0 0 0)
          }
      Test.Unit.test "outside left boundary" do -- 5h [6h,
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 5 0 0 0
        isWithinInterval interval given false
      Test.Unit.test "on left" do -- [6h,
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 6 0 0 0
        isWithinInterval interval given true
      Test.Unit.test "within" do -- [6h, 12h ,18h]
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 12 0 0 0
        isWithinInterval interval given true
      Test.Unit.test "on right" do -- ,18h]
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 18 0 0 0
        isWithinInterval interval given true
      Test.Unit.test "outside right boundary" do -- ,18h] 19h
        let
          given :: Data.Time.Time
          given = Ocelot.Data.DateTime.unsafeMkTime 19 0 0 0
        isWithinInterval interval given false

isWithinInterval ::
  Ocelot.TimePicker.Interval ->
  Data.Time.Time ->
  Boolean ->
  Test.Unit.Test
isWithinInterval interval time expected =
  let
    actual :: Boolean
    actual = Ocelot.TimePicker.isWithinInterval interval time
  in
    Test.Unit.Assert.equal expected actual
