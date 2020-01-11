module Ocelot.Components.TimePicker.Utils where

import Prelude

import Data.Array (length)
import Data.Array.NonEmpty (catMaybes, head)
import Data.DateTime (time)
import Data.Either (either, hush)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.String.Regex (match, parseFlags, regex)
import Data.Time (Time)


----------
-- Search
guessTime :: String -> Maybe Time
guessTime ""   = Nothing
guessTime text =
  let meridian :: Maybe String
      meridian = either
        (const Nothing)
        (\r -> join $ head <$> match r text)
        (regex "p" $ parseFlags "i")

      digits :: Array String
      digits = either
        (const [])
        (\r -> fromMaybe [] (catMaybes <$> match r text))
        (regex "\\d" $ parseFlags "g")

      digits' :: String
      digits' = joinWith "" digits

      hourMin :: Maybe String
      hourMin = case length digits of
        1 -> pure $ "0" <> digits' <> "00"
        2 -> pure $ digits' <> "00"
        3 -> pure $ "0" <> digits'
        4 -> pure $ digits'
        _ -> Nothing

      format :: String
      format = (maybe "HHmm" (const "hhmm a") meridian)

      guess :: Maybe String
      guess = (_ <> maybe "" (const " PM") meridian) <$> hourMin

   in time <$> (join $ hush <<< unformatDateTime format <$> guess)
