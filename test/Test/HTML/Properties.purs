module Test.Ocelot.HTML.Properties (suite) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Class as Effect.Class
import Effect.Now as Effect.Now
import Ocelot.HTML.Properties (css, extract, (<&>))
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert
import Test.Unit.Console as Test.Unit.Console

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Ocelot.HTML.Properties" do
    Test.Unit.suite "appendIProps" do
      Test.Unit.test "right side overrides left side" do
        let
          ipropsA = [ css "m-10 p-10 px-5 pb-12 min-w-80 w-full shadow overflow-hidden" ]
          ipropsB = [ css "p-5 pb-10 min-w-100 overflow-auto h-full" ]
          (Tuple results _) = extract (ipropsA <&> ipropsB)
          results' = Set.fromFoldable results
          expected = Set.fromFoldable
            [ "m-10"
            , "p-5"
            , "px-5"
            , "pb-10"
            , "min-w-100"
            , "w-full"
            , "shadow"
            , "overflow-auto"
            , "h-full"
            ]
        Test.Unit.Assert.equal expected results'

      Test.Unit.test "appendIProps profile" do
        -- naive appendIProps:
        -- 5 tests at 100,000 computations took
        -- 19940, 16041, 16339, 15695, 15185
        -- milliseconds to run
        -- 5 tests at 10,000 computations took
        -- 2270, 1960, 1804, 1600, 1598
        -- milliseconds to run
        start <- Effect.Class.liftEffect Effect.Now.now
        _ <- Test.Unit.Console.log $ "Start time: " <> show start
        let
          ipropsA = [ css "m-10 p-10 px-5 pb-12 min-w-80 w-full shadow overflow-hidden" ]
          ipropsB = [ css "p-5 pb-10 min-w-100 overflow-auto h-full" ]
          go x acc
            | x <= 0 = acc
            | otherwise = go (x - 1) (ipropsA <&> ipropsB)
          _ = go 10000 []
        end <- Effect.Class.liftEffect Effect.Now.now
        _ <- Test.Unit.Console.log $ "End time: " <> show end
        let
          (Milliseconds start') = unInstant start
          (Milliseconds end') = unInstant end
        Test.Unit.Console.log $ "Total ellapsed time: (Milliseconds " <> (show $ end' - start') <> ")"
