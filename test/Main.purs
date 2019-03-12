module Test.Main where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Ocelot.HTML.Properties (appendIProps, css, extract)
import Test.Currency (currencyCheck)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (log)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "Currency" currencyCheck
  suite "HTML.Properties" do
    test "appendIProps" do
      let ipropsA = [ css "m-10 p-10 px-5 pb-12 min-w-80 w-full shadow overflow-hidden" ]
          ipropsB = [ css "p-5 pb-10 min-w-100 overflow-auto h-full" ]
          (Tuple results _) = extract $ appendIProps ipropsA ipropsB
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
      equal expected results'

    -- naive appendIProps:
    -- 5 tests at 100,000 computations took
    -- 19940, 16041, 16339, 15695, 15185
    -- milliseconds to run
    -- 5 tests at 10,000 computations took
    -- 2270, 1960, 1804, 1600, 1598
    -- milliseconds to run
    test "appendIProps profile" do
      start <- liftEffect now
      _ <- log $ "Start time: " <> show start
      let ipropsA = [ css "m-10 p-10 px-5 pb-12 min-w-80 w-full shadow overflow-hidden" ]
          ipropsB = [ css "p-5 pb-10 min-w-100 overflow-auto h-full" ]
          go x acc
            | x <= 0     = acc
            | otherwise  = go (x - 1) $ appendIProps ipropsA ipropsB
          run = go 10000 []
      end <- liftEffect now
      _ <- log $ "End time: " <> show end
      let (Milliseconds start') = unInstant start
          (Milliseconds end')   = unInstant end
      log $ "Total ellapsed time: (Milliseconds " <> ( show $ end' - start' ) <> ")"
