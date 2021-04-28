module Ocelot.TimePicker.Utils where

import Prelude
import Control.Alt ((<|>))
import Data.Array (length)
import Data.Array.NonEmpty (catMaybes, head)
import Data.DateTime (time)
import Data.Either (either, hush)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith, toLower)
import Data.String.Regex (match, parseFlags, regex)
import Data.Time (Time)

data Meridiem
  = AM
  | PM

meridiemToString :: Meridiem -> String
meridiemToString = case _ of
  AM -> "AM"
  PM -> "PM"

----------
-- Search
guessTime :: String -> Maybe Time
guessTime ""   = Nothing
guessTime text =
  let meridiem :: Maybe Meridiem
      meridiem = do
        let
          regexFlags = parseFlags "i"
        regexA <- hush (regex "a" $ regexFlags)
        regexP <- hush (regex "p" $ regexFlags)
        matched <- match regexA text <|> match regexP text
        meridiemString <- head matched
        case toLower meridiemString of
          "a" -> Just AM
          "p" -> Just PM
          _ -> Nothing

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
      format = (maybe "HHmm" (const "hhmm a") meridiem)

      suffix :: String
      suffix = maybe "" (\m -> " " <> meridiemToString m) meridiem


      guess :: Maybe String
      guess = (_ <> suffix) <$> hourMin

   in time <$> (join $ hush <<< unformatDateTime format <$> guess)
