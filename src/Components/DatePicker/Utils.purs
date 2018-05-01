module Ocelot.Components.DatePicker.Utils where

import Prelude

import Data.Date (Date, Day, Year, canonicalDate, lastDayOfMonth, year, month, day, Weekday(..), weekday)
import Data.DateTime (Month(..), adjust, date)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Enum (class Enum, toEnum, pred, succ)
import Data.Array ((:), reverse, drop, take)
import Data.Maybe (Maybe, fromJust)
import Data.Time.Duration (Days(..))
import Partial.Unsafe (unsafePartial)

----------
-- Padded Dates

-- Summary helper function that creates a full grid calendar layout
-- from a year and a month.
align :: Year -> Month -> Array (Array Date)
align y m = rowsFromAligned (alignByWeek y m)

-- A type to help assist making grid-based calendar layouts. Calendars
-- can use dates directly or use array lengths as offsets.
type Aligned =
  { pre  :: Array Date   -- Dates before the first of the month
  , body :: Array Date   -- Dates within the month
  , post :: Array Date   -- Dates after the last of the month
  , all  :: Array Date } -- The full 35-day range

-- Break a set of Sunday-aligned dates into rows, each 7 in length.
rowsFromAligned :: Aligned -> Array (Array Date)
rowsFromAligned { all } = rowsFromArray all

-- Break a set of Sunday-aligned dates into rows, each 7 in length.
rowsFromArray :: ∀ a. Array a -> Array (Array a)
rowsFromArray all = go all []
  where
    go [] acc = reverse acc
    go xs acc = go (drop 7 xs) (take 7 xs : acc)

-- A special case for when you need to match days of the month to a grid
-- that's bound to weekdays Sun - Sat.
alignByWeek :: Year -> Month -> Aligned
alignByWeek y m = { pre: pre, body: body, post: post, all: pre <> body <> post }
 where
   start = firstDateOfMonth y m
   end = lastDateOfMonth y m
   body = dateRange start end
   pre =
     let pad = padPrev $ weekday start
      in if pad == 0.0 then [] else dateRange (adjustDaysBy pad start) (prevDay start)
   post =
     let pad = padNext $ weekday end
      in if pad == 0.0 then [] else dateRange (nextDay end) (adjustDaysBy pad end)


-- Represents the number of days that will need to be "filled in"
-- when the first day of the month is this weekday. For example, if the
-- first day of the month is Tuesday, then Sunday and Monday will need
-- to be padded
padPrev :: Weekday -> Number
padPrev Sunday    = 0.0
padPrev Monday    = (-1.0)
padPrev Tuesday   = (-2.0)
padPrev Wednesday = (-3.0)
padPrev Thursday  = (-4.0)
padPrev Friday    = (-5.0)
padPrev Saturday  = (-6.0)

-- Represents the number of days that will need to be "filled in"
-- when the last day of the month is this weekday. For example, if the
-- last day of the month is Tuesday, then Wednesday through Saturday
-- will need to be padded
padNext :: Weekday -> Number
padNext Sunday    = 6.0
padNext Monday    = 5.0
padNext Tuesday   = 4.0
padNext Wednesday = 3.0
padNext Thursday  = 2.0
padNext Friday    = 1.0
padNext Saturday  = 0.0


----------
-- Date Ranges

-- List each day in the month. Relies on `dateRange` under the hood.
monthRange :: Year -> Month -> Array Date
monthRange y m = dateRange start end
  where
    start = firstDateOfMonth y m
    end   = lastDateOfMonth y m

-- Provided two dates, generates the range (inclusive) between them.
-- Note: Preserves ordering. If you provide a future then past date,
-- your return range will be from the future to the past.
dateRange :: Date -> Date -> Array Date
dateRange = \s e -> go s e []
  where
    go start end acc
      | start == end = reverse $ start : acc
      | start >  end = reverse $ go end start acc
      | otherwise    = go (nextDay start) end (start : acc)



----------
-- Conditional Selections

firstDateOfMonth :: Year -> Month -> Date
firstDateOfMonth y m = canonicalDate y m bottom

lastDateOfMonth :: Year -> Month -> Date
lastDateOfMonth y m = canonicalDate y m (lastDayOfMonth y m)


----------
-- Round

-- Note: Functions are all partial, which should be OK because
-- all rely on bounded enums. Also rely on `canonical`, which resolves
-- out-of-bounds dates by making up the difference (Feb 29 on a leap year
-- becomes March 1 automatically). This could be the source of some obscure
-- issues in succession which will warrant a rewrite.

nextDay :: Date -> Date
nextDay = adjustDaysBy 1.0

prevDay :: Date -> Date
prevDay = adjustDaysBy (-1.0)

adjustDaysBy :: Number -> Date -> Date
adjustDaysBy n = unsafePartial fromJust <<< next n
  where
    next :: Number -> Date -> Maybe Date
    next dur d = date <$> (adjust (Days dur) (toDateTime $ fromDate d))

-- Months
-- Note: Attempts to preserve the same day, but due to the use of
-- `canonical` this can cause odd behavior. Fix if necessary.
nextMonth :: Date -> Date
nextMonth d = case (month d) of
  December -> canonicalDate (unsafeSucc (year d)) January (day d)
  other    -> canonicalDate (year d) (unsafeSucc (month d)) (day d)

prevMonth :: Date -> Date
prevMonth d = case (month d) of
  January -> canonicalDate (unsafePred (year d)) December (day d)
  other   -> canonicalDate (year d) (unsafePred (month d)) (day d)

-- Years
-- Note: Attempts to preserve the same day, but due to the use of
-- `canonical` this can cause odd behavior (ex: Feb 29 -> leap year)
-- Fix if necessary.
nextYear :: Date -> Date
nextYear d = canonicalDate (unsafeSucc (year d)) (month d) (day d)

prevYear :: Date -> Date
prevYear d = canonicalDate (unsafePred (year d)) (month d) (day d)

----------
-- Unsafe Functions

-- These should only be used when you are 100% sure that the date
-- you are providing is valid; they buy you some convenience.

-- Pred & Succ
unsafeSucc :: ∀ a. Enum a => a -> a
unsafeSucc = unsafePartial fromJust <<< succ

unsafePred :: ∀ a. Enum a => a -> a
unsafePred = unsafePartial fromJust <<< pred

-- Provide a year, month, and day to construct a date. Due to the
-- use of `canonicalDate`, if your provided date is out of bounds,
-- it will wrap around. Ex: Jan 32 -> Feb 1
unsafeMkDate :: Int -> Int -> Int -> Date
unsafeMkDate y m d = canonicalDate year month day
  where
    year  = unsafeMkYear y
    month = unsafeMkMonth m
    day   = unsafeMkDay d

unsafeMkYear :: Int -> Year
unsafeMkYear = unsafePartial fromJust <<< toEnum

unsafeMkMonth :: Int -> Month
unsafeMkMonth = unsafePartial fromJust <<< toEnum

unsafeMkDay :: Int -> Day
unsafeMkDay = unsafePartial fromJust <<< toEnum
