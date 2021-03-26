-- | An optimal data structure for storing **closed** intervals
-- |
-- | Implementation is inspired by [Data structure for handling intervals](https://stackoverflow.com/questions/1982409/data-structure-for-handling-intervals)
module Ocelot.Data.IntervalTree
  ( IntervalPoint(..)
  , IntervalTree
  , findGreatestLowerBound
  , findLeastUpperBound
  , fromIntervals
  , insertInterval
  , lookupInterval
  , toIntervals
  ) where

import Prelude

import Data.Array as Data.Array
import Data.FoldableWithIndex as Data.FoldableWithIndex
import Data.Generic.Rep as Data.Generic.Rep
import Data.Generic.Rep.Show as Data.Generic.Rep.Show
import Data.Map as Data.Map
import Data.Maybe (Maybe(..))
import Data.Tuple as Data.Tuple

type IntervalTree a = Data.Map.Map a IntervalPoint

data IntervalPoint
  = StartPoint
  | EndPoint

derive instance eqIntervalPoint :: Eq IntervalPoint

derive instance genericIntervalPoint :: Data.Generic.Rep.Generic IntervalPoint _

instance showIntervalPoint :: Show IntervalPoint where
  show = Data.Generic.Rep.Show.genericShow

-- | O(n log n)
fromIntervals ::
  forall a.
  Ord a =>
  Array { start :: a, end :: a } ->
  IntervalTree a
fromIntervals = Data.Array.foldl insertInterval mempty

-- | O(log n)
-- |
-- | example: insert (4 9) into [ (1 4) (8 10) (12 15) ]
-- | 1. mergeStart: merge StartPoint of the interval into the tree
-- |   * 4 already exists but as an EndPoint so we remove it from the tree
-- |   * result: [ (1 (8 10) (12 15) ]
-- |     * NOTE temporarily invalid pairings will be fixed when clearBetween
-- | 2. mergeEnd: merge EndPoint of the interval into the tree
-- |   * 9 doesn't exist so we insert it into the tree as an EndPoint
-- |   * result: [ (1 (8 9) 10) (12 15) ]
-- | 3. clearBetween: remove values lying within the interval from the tree
-- |   * lower bound of (4 is (1 in the original tree
-- |   * upper bound of 9) is 10) in the original tree
-- |   * 8 and 9 are within (1 10) so we remove them from the tree
-- |   * result: [ (1 10) (12 15) ]
insertInterval ::
  forall a.
  Ord a =>
  IntervalTree a ->
  { start :: a, end :: a } ->
  IntervalTree a
insertInterval old { start, end }
  | start >= end = old
  | otherwise = old # mergeStart >>> mergeEnd >>> clearBetween
  where
  mergeStart :: IntervalTree a -> IntervalTree a
  mergeStart xs = case Data.Map.lookup start xs of
    Nothing -> Data.Map.insert start StartPoint  xs
    Just intervalPoint -> case intervalPoint of
      StartPoint -> xs
      EndPoint -> Data.Map.delete start xs

  mergeEnd :: IntervalTree a -> IntervalTree a
  mergeEnd xs = case Data.Map.lookup end xs of
    Nothing -> Data.Map.insert end EndPoint xs
    Just intervalPoint -> case intervalPoint of
      StartPoint -> Data.Map.delete end xs
      EndPoint -> xs

  clearBetween :: IntervalTree a -> IntervalTree a
  clearBetween xs =
    Data.Map.submap Nothing (Just lowerBound) xs
      <> Data.Map.submap (Just upperBound) Nothing xs

  lowerBound :: a
  lowerBound = case findGreatestLowerBound start old of
    Nothing -> start
    Just greatestLowerBound -> case greatestLowerBound.value of
      StartPoint -> greatestLowerBound.key
      EndPoint -> start

  upperBound :: a
  upperBound = case findLeastUpperBound end old of
    Nothing -> end
    Just leastUpperBound -> case leastUpperBound.value of
      StartPoint -> end
      EndPoint -> leastUpperBound.key

-- | Get immediate interval around a point
-- | O(log n)
-- |
-- | example: [ (1 3) (5 7) ]
-- | * around 0: __ (1
-- | * around 1: (1 3)
-- | * around 2: (1 3)
-- | * around 3: (1 3)
-- | * around 4: 3) (5
-- | * around 5: (5 7)
-- | * around 6: (5 7)
-- | * around 7: (5 7)
-- | * around 8: 7) __
lookupInterval ::
  forall a.
  Ord a =>
  a ->
  IntervalTree a ->
  { left :: Maybe { key :: a, value :: IntervalPoint }
  , right :: Maybe { key :: a, value :: IntervalPoint }
  }
lookupInterval x xs = case Data.Map.lookup x xs of
  Nothing ->
    { left: findGreatestLowerBound x xs
    , right: findLeastUpperBound x xs
    }
  Just intervalPoint -> case intervalPoint of
    StartPoint ->
      { left: Just { key: x, value: StartPoint }
      , right: findLeastUpperBound x xs
      }
    EndPoint ->
      { left: findGreatestLowerBound x xs
      , right: Just { key: x, value: EndPoint }
      }

-- | O(log n)
findGreatestLowerBound ::
  forall a.
  Ord a =>
  a ->
  IntervalTree a ->
  Maybe { key :: a, value :: IntervalPoint }
findGreatestLowerBound x =
  Data.Map.findMax
    <<< Data.Map.delete x
    <<< Data.Map.submap Nothing (Just x)

-- | O(log n)
findLeastUpperBound ::
  forall a.
  Ord a =>
  a ->
  IntervalTree a ->
  Maybe { key :: a, value :: IntervalPoint }
findLeastUpperBound x =
  Data.Map.findMin
    <<< Data.Map.delete x
    <<< Data.Map.submap (Just x) Nothing

-- | O(n)
toIntervals ::
  forall a.
  IntervalTree a ->
  Array { start :: a, end :: a }
toIntervals = partitionsToIntervals <<< partitionIntervalTree
  where
  partitionsToIntervals ::
    { start :: Array a , end :: Array a } ->
    Array { start :: a, end :: a }
  partitionsToIntervals partition =
    map (Data.Tuple.uncurry { start: _, end: _ })
      $ Data.Array.zip partition.start partition.end

  partitionIntervalTree ::
    IntervalTree a ->
    { start :: Array a
    , end :: Array a
    }
  partitionIntervalTree =
    Data.FoldableWithIndex.foldlWithIndex reducer { start: [], end: [] }

  reducer ::
    a ->
    { start :: Array a
    , end :: Array a
    } ->
    IntervalPoint ->
    { start :: Array a
    , end :: Array a
    }
  reducer x old = case _ of
    StartPoint -> old { start = Data.Array.snoc old.start x }
    EndPoint -> old { end = Data.Array.snoc old.end x }
