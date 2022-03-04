module Test.Ocelot.Data.Currency (suite) where

import Prelude

import Ocelot.Data.Currency (Cents(..), formatCentsToStrDollars, parseCentsFromDollarStr, parseCentsFromMicroDollars)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..))
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite = do
  Test.Unit.suite "parsers" do
    Test.Unit.test "parseCentsFromMicroDollars" do
      let expect = Just $ Cents $ BigInt.fromInt 1234
          result = parseCentsFromMicroDollars 12340000.0
      Test.Unit.Assert.equal expect result

    Test.Unit.test "parseCentsFromDollarStr NULL" do
      let expect = Nothing
          result = parseCentsFromDollarStr "NULL"
      Test.Unit.Assert.equal expect result

    Test.Unit.test "parseCentsFromDollarStr 12.34" do
      let expect = Just $ Cents $ BigInt.fromInt 1234
          result = parseCentsFromDollarStr "12.34"
      Test.Unit.Assert.equal expect result

    Test.Unit.test "parseCentsFromDollarStr 1234" do
      let expect = Just $ Cents $ BigInt.fromInt 123400
          result = parseCentsFromDollarStr "1234"
      Test.Unit.Assert.equal expect result

    Test.Unit.test "parseCentsFromDollarStr 1234.00" do
      let expect = Just $ Cents $ BigInt.fromInt 123400
          result = parseCentsFromDollarStr "1234.00"
      Test.Unit.Assert.equal expect result

    Test.Unit.test "parseCentsFromDollarStr 123.4" do
      let expect = Just $ Cents $ BigInt.fromInt 12340
          result = parseCentsFromDollarStr "123.4"
      Test.Unit.Assert.equal expect result

    Test.Unit.test "parseCentsFromDollarStr 1,234" do
      let expect = Just $ Cents $ BigInt.fromInt 123400
          result = parseCentsFromDollarStr "1,234"
      Test.Unit.Assert.equal expect result

    Test.Unit.test "parseCentsFromDollarStr 1,234.00" do
      let expect = Just $ Cents $ BigInt.fromInt 123400
          result = parseCentsFromDollarStr "1,234.00"
      Test.Unit.Assert.equal expect result

    Test.Unit.test "parseCentsFromDollarStr 1,234." do
      let expect = Just $ Cents $ BigInt.fromInt 123400
          result = parseCentsFromDollarStr "1,234."
      Test.Unit.Assert.equal expect result

    Test.Unit.test "parseCentsFromDollarStr -1,234.50" do
      let expect = Just $ Cents $ BigInt.fromInt (-123450)
          result = parseCentsFromDollarStr "-1,234.50"
      Test.Unit.Assert.equal expect result

    Test.Unit.test "formatCentsToStrDollars 0.00" do
      let result = formatCentsToStrDollars $ Cents $ BigInt.fromInt 0
      Test.Unit.Assert.equal "0.00" result

    Test.Unit.test "formatCentsToStrDollars 12.34" do
      let result = formatCentsToStrDollars $ Cents $ BigInt.fromInt 1234
      Test.Unit.Assert.equal "12.34" result

    Test.Unit.test "formatCentsToStrDollars 123.40" do
      let result = formatCentsToStrDollars $ Cents $ BigInt.fromInt 12340
      Test.Unit.Assert.equal "123.40" result

    Test.Unit.test "formatCentsToStrDollars 1,234.00" do
      let result = formatCentsToStrDollars $ Cents $ BigInt.fromInt 123400
      Test.Unit.Assert.equal "1,234.00" result

    Test.Unit.test "formatCentsToStrDollars -1,234.50" do
      let result = formatCentsToStrDollars $ Cents $ BigInt.fromInt (-123450)
      Test.Unit.Assert.equal "-1,234.50" result

    Test.Unit.test "Just cents === parseCentsFromDollarStr (formatCentsToStrDollars cents)" do
      Test.Unit.QuickCheck.quickCheck \int -> do
        let cents = Cents $ BigInt.fromInt int
        Just cents === parseCentsFromDollarStr (formatCentsToStrDollars cents)
