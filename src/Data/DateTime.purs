module Ocelot.Data.DateTime where

import Prelude

import Data.Array (reverse)
import Data.Date (Date, Day, Year, canonicalDate, day, lastDayOfMonth, month, year)
import Data.DateTime (DateTime(..), Month(..))
import Data.DateTime (adjust, date) as DT
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Enum (class Enum, pred, succ, toEnum)
import Data.Formatter.DateTime (Formatter)
import Data.Formatter.DateTime as Formatter
import Data.List (List(..), (:))
import Data.Maybe (Maybe, fromJust)
import Data.Time (Hour, Millisecond, Minute, Second, Time(..))
import Data.Time (adjust) as T
import Data.Time.Duration (Days(..), Hours(..), Minutes(..), negateDuration)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartial)


----------
-- Defaults
defaultTime :: Time
defaultTime = unsafeMkTime 0 0 0 0

defaultDate :: Date
defaultDate = unsafeMkDate 2000 1 1

-- Array of 23 hour intervals, and one 59 min one
defaultTimeRange :: Array Time
defaultTimeRange = hourRange defaultTime 23 <> [prevMinute defaultTime]

----------
-- Time Ranges

-- Generates an array of hours
hourRange :: Time -> Int -> Array Time
hourRange start hrs = go start 0 []
  where
    go cur count acc
      | count == hrs = acc <> [cur]
      | otherwise    = go (nextHour cur) (count + 1) (acc <> [cur])


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
      | start == end = reverse $ [start] <> acc
      | start >  end = reverse $ go end start acc
      | otherwise    = go (nextDay start) end ([start] <> acc)


----------
-- Time Manipulation
nextHour :: Time -> Time
nextHour = snd <<< T.adjust (Hours 1.0)

prevMinute :: Time -> Time
prevMinute = snd <<< T.adjust (negateDuration $ Minutes 1.0)


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
    next dur d = DT.date <$> (DT.adjust (Days dur) (toDateTime $ fromDate d))

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

yearsForward :: Int -> Date -> Date
yearsForward n d | n <= 0    = d
                 | otherwise = yearsForward (n - 1) (nextYear d)

yearsBackward :: Int -> Date -> Date
yearsBackward n d | n <= 0   = d
                  | otherwise = yearsBackward (n - 1) (prevYear d)

extractYearMonth :: Date -> Tuple Year Month
extractYearMonth d = Tuple (year d) (month d)

----------
-- Formatting
formatTime :: Time -> String
formatTime = (either (const "-") identity)
  <<< Formatter.formatDateTime "hh:mm a"
  <<< DateTime defaultDate

formatDate :: Date -> String
formatDate = Formatter.format readableFormat <<< flip DateTime defaultTime

readableFormat :: Formatter
readableFormat
  = Formatter.DayOfWeekNameShort
  : Formatter.Placeholder ", "
  : Formatter.MonthShort
  : Formatter.Placeholder " "
  : Formatter.DayOfMonth
  : Formatter.Placeholder " "
  : Formatter.YearFull
  : Nil

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

unsafeMkTime :: Int -> Int -> Int -> Int -> Time
unsafeMkTime h m s ms = Time hour minute second millisecond
  where
    hour        = unsafeMkHour h
    minute      = unsafeMkMinute m
    second      = unsafeMkSecond s
    millisecond = unsafeMkMillisecond ms

unsafeMkHour :: Int -> Hour
unsafeMkHour = unsafePartial fromJust <<< toEnum

unsafeMkMinute :: Int -> Minute
unsafeMkMinute = unsafePartial fromJust <<< toEnum

unsafeMkSecond :: Int -> Second
unsafeMkSecond = unsafePartial fromJust <<< toEnum

unsafeMkMillisecond :: Int -> Millisecond
unsafeMkMillisecond = unsafePartial fromJust <<< toEnum
