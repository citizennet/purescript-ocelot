module Ocelot.Component.DatePicker.Utils where

import Prelude

import Data.Array (drop, find, reverse, sort, take)
import Data.Date (Date, Weekday(..), Year, day, month, weekday, year)
import Data.DateTime (Month)
import Data.Either (either)
import Data.Enum (fromEnum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Fuzzy (Fuzzy(..))
import Data.Fuzzy (match) as Fuzz
import Data.Maybe (Maybe(..), maybe)
import Data.Rational ((%))
import Data.String.Regex (parseFlags, regex, replace)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object, fromFoldable)
import Ocelot.Data.DateTime (adjustDaysBy, dateRange, firstDateOfMonth, lastDateOfMonth, nextDay, nextYear, prevDay, yearsForward)

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
    go xs acc = go (drop 7 xs) ([take 7 xs] <> acc)

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
-- Search
-- Generates a date range to search through for search term, if it doesn't
-- match on first past it will generate more dates to search through until
-- a specified range limit is reached, then return Nothing
newtype MaxYears = MaxYears Int

guessDate :: Date -> MaxYears -> String -> Maybe Date
guessDate start (MaxYears max) text =
  let text' :: String -- replace dashes and slashes with spaces
      text' = either
        (const text)
        (\r -> replace r " " text)
        (regex "-|\\/|," $ parseFlags "g")

      text'' :: String -- consolidate all consecutive whitespaceg
      text'' = either
        (const text')
        (\r -> replace r " " text')
        (regex "\\s+" $ parseFlags "g")

      matcher :: Int -> Date -> Tuple (Fuzzy Date) Int
      matcher i d = Tuple (Fuzz.match true toObject text'' d) i

      guess :: Array Date -> Int -> Maybe Date
      guess dates = findIn (firstMatch $ sort $ matcher `mapWithIndex` dates)

      findIn :: Maybe (Fuzzy Date) -> Int -> Maybe Date
      findIn (Just (Fuzzy { original })) _ = Just original
      findIn Nothing pass
        | pass > max = Nothing
        | otherwise  = guess (dateRange (yearsForward pass start) (yearsForward (pass + 1) start)) (pass + 1)

   in guess (dateRange start $ nextYear start) 0

firstMatch :: Array (Tuple (Fuzzy Date) Int) -> Maybe (Fuzzy Date)
firstMatch = maybe Nothing (Just <<< fst) <<< find match'
  where
    match' (Tuple (Fuzzy { ratio }) _) = ratio == (1 % 1)

toObject :: Date -> Object String
toObject d =
  fromFoldable
    [ Tuple "mdy1" $ sYearMonth <> " " <> sDay <> " " <> sYear
    , Tuple "mdy2" $ sMonth <> " " <> sDay <> " " <> sYear
    , Tuple "weekday" $ sWeekDay
    , Tuple "wmdy1" $ sWeekDay <> " " <> sYearMonth <> " " <> sDay <> " " <> sYear
    , Tuple "ymd" $ sYear <> " " <> sMonth <> " " <> sDay
    ]
    where
      sYear = show $ fromEnum $ year d
      sMonth = show $ fromEnum $ month d
      sYearMonth = show $ month d
      sDay = show $ fromEnum $ day d
      sWeekDay = show $ weekday d
