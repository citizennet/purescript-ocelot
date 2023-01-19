module Test.Ocelot.Data.IntervalTree (suite) where

import Prelude

import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Ocelot.Data.IntervalTree as Ocelot.Data.IntervalTree
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Ocelot.Data.IntervalTree" do
    findGreatestLowerBoundTests
    findLeastUpperBoundTests
    insertIntervalTests
    fromIntervalsTests
    toIntervalsTests
    lookupIntervalTests

insertIntervalTests :: Test.Unit.TestSuite
insertIntervalTests =
  Test.Unit.suite "insertIntervals" do
    let
      -- (1 3) (5 7)
      intervalTree :: Ocelot.Data.IntervalTree.IntervalTree Int
      intervalTree =
        Data.Map.fromFoldable
          [ 1 /\ Ocelot.Data.IntervalTree.StartPoint
          , 3 /\ Ocelot.Data.IntervalTree.EndPoint
          , 5 /\ Ocelot.Data.IntervalTree.StartPoint
          , 7 /\ Ocelot.Data.IntervalTree.EndPoint
          ]
    Test.Unit.test "non-overlapping" do
      let
        -- (1 3) (5 7)
        --             (9 11)
        interval :: { start :: Int, end :: Int }
        interval = { start: 9, end: 11 }

        -- (1 3) (5 7) (9 11)
        expected :: Ocelot.Data.IntervalTree.IntervalTree Int
        expected =
          Data.Map.fromFoldable
            [ 1 /\ Ocelot.Data.IntervalTree.StartPoint
            , 3 /\ Ocelot.Data.IntervalTree.EndPoint
            , 5 /\ Ocelot.Data.IntervalTree.StartPoint
            , 7 /\ Ocelot.Data.IntervalTree.EndPoint
            , 9 /\ Ocelot.Data.IntervalTree.StartPoint
            , 11 /\ Ocelot.Data.IntervalTree.EndPoint
            ]

        actual :: Ocelot.Data.IntervalTree.IntervalTree Int
        actual = Ocelot.Data.IntervalTree.insertInterval intervalTree interval
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "overlapping" do
      let
        -- (1 3) (5 7)
        --   (3    6)
        interval :: { start :: Int, end :: Int }
        interval = { start: 3, end: 6 }

        -- (1       7)
        expected :: Ocelot.Data.IntervalTree.IntervalTree Int
        expected =
          Data.Map.fromFoldable
            [ 1 /\ Ocelot.Data.IntervalTree.StartPoint
            , 7 /\ Ocelot.Data.IntervalTree.EndPoint
            ]

        actual :: Ocelot.Data.IntervalTree.IntervalTree Int
        actual = Ocelot.Data.IntervalTree.insertInterval intervalTree interval
      Test.Unit.Assert.equal expected actual

fromIntervalsTests :: Test.Unit.TestSuite
fromIntervalsTests =
  Test.Unit.suite "fromIntervals" do
    Test.Unit.test "non-overlapping" do
      let
        intervals :: Array { start :: Int, end :: Int }
        intervals =
          [ { start: 1, end: 4 }
          , { start: 8, end: 10 }
          , { start: 12, end: 15 }
          ]

        expected :: Ocelot.Data.IntervalTree.IntervalTree Int
        expected =
          Data.Map.fromFoldable
            [ 1 /\ Ocelot.Data.IntervalTree.StartPoint
            , 4 /\ Ocelot.Data.IntervalTree.EndPoint
            , 8 /\ Ocelot.Data.IntervalTree.StartPoint
            , 10 /\ Ocelot.Data.IntervalTree.EndPoint
            , 12 /\ Ocelot.Data.IntervalTree.StartPoint
            , 15 /\ Ocelot.Data.IntervalTree.EndPoint
            ]

        actual :: Ocelot.Data.IntervalTree.IntervalTree Int
        actual = Ocelot.Data.IntervalTree.fromIntervals intervals
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "overlapping" do
      let
        intervals :: Array { start :: Int, end :: Int }
        intervals =
          [ { start: 1, end: 4 }
          , { start: 4, end: 14 }
          , { start: 12, end: 15 }
          ]

        expected :: Ocelot.Data.IntervalTree.IntervalTree Int
        expected =
          Data.Map.fromFoldable
            [ 1 /\ Ocelot.Data.IntervalTree.StartPoint
            , 15 /\ Ocelot.Data.IntervalTree.EndPoint
            ]

        actual :: Ocelot.Data.IntervalTree.IntervalTree Int
        actual = Ocelot.Data.IntervalTree.fromIntervals intervals
      Test.Unit.Assert.equal expected actual

toIntervalsTests :: Test.Unit.TestSuite
toIntervalsTests =
  Test.Unit.test "toIntervals" do
    let
      intervalTree :: Ocelot.Data.IntervalTree.IntervalTree Int
      intervalTree =
        Data.Map.fromFoldable
          [ 1 /\ Ocelot.Data.IntervalTree.StartPoint
          , 4 /\ Ocelot.Data.IntervalTree.EndPoint
          , 8 /\ Ocelot.Data.IntervalTree.StartPoint
          , 10 /\ Ocelot.Data.IntervalTree.EndPoint
          , 12 /\ Ocelot.Data.IntervalTree.StartPoint
          , 15 /\ Ocelot.Data.IntervalTree.EndPoint
          ]

      expected :: Array { start :: Int, end :: Int }
      expected =
        [ { start: 1, end: 4 }
        , { start: 8, end: 10 }
        , { start: 12, end: 15 }
        ]

      actual :: Array { start :: Int, end :: Int }
      actual = Ocelot.Data.IntervalTree.toIntervals intervalTree
    Test.Unit.Assert.equal expected actual

findGreatestLowerBoundTests :: Test.Unit.TestSuite
findGreatestLowerBoundTests =
  Test.Unit.suite "findGreatestLowerBound" do
    let
      -- (1 4) (8 10) (12 15)
      intervalTree :: Ocelot.Data.IntervalTree.IntervalTree Int
      intervalTree =
        Data.Map.fromFoldable
          [ 1 /\ Ocelot.Data.IntervalTree.StartPoint
          , 4 /\ Ocelot.Data.IntervalTree.EndPoint
          , 8 /\ Ocelot.Data.IntervalTree.StartPoint
          , 10 /\ Ocelot.Data.IntervalTree.EndPoint
          , 12 /\ Ocelot.Data.IntervalTree.StartPoint
          , 15 /\ Ocelot.Data.IntervalTree.EndPoint
          ]
    Test.Unit.test "Just" do
      let
        -- (1 4) (8 10) (12 15)
        --       <- ^^
        point :: Int
        point = 10

        expected :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        expected = Just { key: 8, value: Ocelot.Data.IntervalTree.StartPoint }

        actual :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        actual = Ocelot.Data.IntervalTree.findGreatestLowerBound point intervalTree
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "Nothing" do
      let
        --   (1 4) (8 10) (12 15)
        -- <- ^
        point :: Int
        point = 1

        expected :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        expected = Nothing

        actual :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        actual = Ocelot.Data.IntervalTree.findGreatestLowerBound point intervalTree
      Test.Unit.Assert.equal expected actual

findLeastUpperBoundTests :: Test.Unit.TestSuite
findLeastUpperBoundTests =
  Test.Unit.suite "findLeastUpperBound" do
    let
      -- (1 4) (8 10) (12 15)
      intervalTree :: Ocelot.Data.IntervalTree.IntervalTree Int
      intervalTree =
        Data.Map.fromFoldable
          [ 1 /\ Ocelot.Data.IntervalTree.StartPoint
          , 4 /\ Ocelot.Data.IntervalTree.EndPoint
          , 8 /\ Ocelot.Data.IntervalTree.StartPoint
          , 10 /\ Ocelot.Data.IntervalTree.EndPoint
          , 12 /\ Ocelot.Data.IntervalTree.StartPoint
          , 15 /\ Ocelot.Data.IntervalTree.EndPoint
          ]
    Test.Unit.test "Just" do
      let
        -- (1 4) (8 10) (12 15)
        --          ^^ ->
        point :: Int
        point = 10

        expected :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        expected = Just { key: 12, value: Ocelot.Data.IntervalTree.StartPoint }

        actual :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        actual = Ocelot.Data.IntervalTree.findLeastUpperBound point intervalTree
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "Nothing" do
      let
        -- (1 4) (8 10) (12 15)
        --                  ^^ ->
        point :: Int
        point = 15

        expected :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        expected = Nothing

        actual :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
        actual = Ocelot.Data.IntervalTree.findLeastUpperBound point intervalTree
      Test.Unit.Assert.equal expected actual

lookupIntervalTests :: Test.Unit.TestSuite
lookupIntervalTests =
  Test.Unit.suite "lookupInterval" do
    Test.Unit.test "empty tree" do
      let
        emptyTree :: Ocelot.Data.IntervalTree.IntervalTree Int
        emptyTree = Data.Map.empty

        expected ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        expected =
          { left: Nothing
          , right: Nothing
          }

        actual ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        actual = Ocelot.Data.IntervalTree.lookupInterval 0 emptyTree
      Test.Unit.Assert.equal expected actual

    let
      -- (1 3) (5 7)
      intervalTree :: Ocelot.Data.IntervalTree.IntervalTree Int
      intervalTree =
        Data.Map.fromFoldable
          [ 1 /\ Ocelot.Data.IntervalTree.StartPoint
          , 3 /\ Ocelot.Data.IntervalTree.EndPoint
          , 5 /\ Ocelot.Data.IntervalTree.StartPoint
          , 7 /\ Ocelot.Data.IntervalTree.EndPoint
          ]
    Test.Unit.test "lower than all interval points in the tree" do
      let
        --   (1 3) (5 7)
        -- ^
        point :: Int
        point = 0

        expected ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        expected =
          { left: Nothing
          , right: Just { key: 1, value: Ocelot.Data.IntervalTree.StartPoint }
          }

        actual ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        actual = Ocelot.Data.IntervalTree.lookupInterval point intervalTree
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "overlaps with a StartPoint in the tree" do
      let
        -- (1 3) (5 7)
        --  ^
        point :: Int
        point = 1

        expected ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        expected =
          { left: Just { key: 1, value: Ocelot.Data.IntervalTree.StartPoint }
          , right: Just { key: 3, value: Ocelot.Data.IntervalTree.EndPoint }
          }

        actual ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        actual = Ocelot.Data.IntervalTree.lookupInterval point intervalTree
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "lying within an interval in the tree" do
      let
        -- (1 3) (5 7)
        --   ^
        point :: Int
        point = 2

        expected ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        expected =
          { left: Just { key: 1, value: Ocelot.Data.IntervalTree.StartPoint }
          , right: Just { key: 3, value: Ocelot.Data.IntervalTree.EndPoint }
          }

        actual ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        actual = Ocelot.Data.IntervalTree.lookupInterval point intervalTree
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "overlaps with an EndPoint in the tree" do
      let
        -- (1 3) (5 7)
        --    ^
        point :: Int
        point = 3

        expected ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        expected =
          { left: Just { key: 1, value: Ocelot.Data.IntervalTree.StartPoint }
          , right: Just { key: 3, value: Ocelot.Data.IntervalTree.EndPoint }
          }

        actual ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        actual = Ocelot.Data.IntervalTree.lookupInterval point intervalTree
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "lying outsides all intervals in the tree" do
      let
        -- (1 3) (5 7)
        --      ^
        point :: Int
        point = 4

        expected ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        expected =
          { left: Just { key: 3, value: Ocelot.Data.IntervalTree.EndPoint }
          , right: Just { key: 5, value: Ocelot.Data.IntervalTree.StartPoint }
          }

        actual ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        actual = Ocelot.Data.IntervalTree.lookupInterval point intervalTree
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "greater than all interval points in the tree" do
      let
        -- (1 3) (5 7)
        --             ^
        point :: Int
        point = 8

        expected ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        expected =
          { left: Just { key: 7, value: Ocelot.Data.IntervalTree.EndPoint }
          , right: Nothing
          }

        actual ::
          { left :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          , right :: Maybe { key :: Int, value :: Ocelot.Data.IntervalTree.IntervalPoint }
          }
        actual = Ocelot.Data.IntervalTree.lookupInterval point intervalTree
      Test.Unit.Assert.equal expected actual

