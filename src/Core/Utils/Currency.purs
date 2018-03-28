-- | Constructors are not exported for Cents or Dollars
module Ocelot.Core.Utils.Currency
  ( Cents
  , parseCentsFromDollarStr
  , parseCentsFromCentInt
  , Dollars
  , parseDollarsFromDollarStr
  , parseDollarsFromDollarInt
  , centsToDollars
  , dollarsToCents
  , formatCentsToStrDollars
  , formatDollarsToStrDollars
  )
  where

import Prelude

import Data.Rational (Rational, toNumber, (%))
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Foldable (foldr)
import Data.Int as Int
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), Replacement(..), length, take, split)
import Data.String.Yarn (toChars, (:), substitute)
import Data.Tuple (Tuple(..), snd)


-- | Cents

newtype Cents = Cents Int
derive instance newtypeCents :: Newtype Cents _
instance encodeJsonCents :: EncodeJson Cents where
  encodeJson (Cents n) = encodeJson n
instance decodeJsonCents :: DecodeJson Cents where
  decodeJson json = Cents <$> decodeJson json

-- Given a string that might be formatted to an int representation in cents. This is
-- the default parser: if you can parse to Cents, you can always get a StrDollars.
parseCentsFromDollarStr :: String -> Maybe Cents
parseCentsFromDollarStr str = parseCentsFromCentInt <$>
  case split (Pattern ".") str of
    [d] -> Int.fromString $ strip d <> "00"
    [d, c] -> (+) <$> Int.fromString (strip d <> "00") <*> Int.fromString (round c)
    otherwise -> Nothing
    where
      strip :: String -> String
      strip = substitute (Pattern ",") (Replacement "")

      round :: String -> String
      round c
        | length c == 0 = "00"
        | length c == 1 = c <> "0"
        | otherwise = take 2 c

-- WARN: Expects an integer already in cents, not dollars.
parseCentsFromCentInt :: Int -> Cents
parseCentsFromCentInt n = Cents n

-- | Dollars

newtype Dollars = Dollars Rational
derive instance newtypeDollars :: Newtype Dollars _

-- We can decode by first verifying legitimate `Cents` and then
parseDollarsFromDollarStr :: String -> Maybe Dollars
parseDollarsFromDollarStr str = centsToDollars <$> parseCentsFromDollarStr str

parseDollarsFromDollarInt :: Int -> Dollars
parseDollarsFromDollarInt n = Dollars $ n % 100


-- | Conversions

dollarsToCents :: Dollars -> Cents
dollarsToCents (Dollars r) = Cents $ (Int.floor <<< (*) 100.00 <<< toNumber) r

centsToDollars :: Cents -> Dollars
centsToDollars (Cents n) = Dollars (n % 100)

-- Given some dollars, format a string representation
formatDollarsToStrDollars :: Dollars -> String
formatDollarsToStrDollars = formatCentsToStrDollars <<< dollarsToCents

-- Given some cents, format a string representation
-- in dollars.
formatCentsToStrDollars :: Cents -> String
formatCentsToStrDollars (Cents n)
  | n < 10  = "0.0" <> show n
  | n < 100 = "0." <> show n
  | otherwise = snd $ foldr formatCentsToDollars' (Tuple 0 "") (chars n)
    where
      chars :: Int -> Array Char
      chars = toChars <<< show

      formatCentsToDollars' :: Char -> Tuple Int String -> Tuple Int String
      formatCentsToDollars' d (Tuple i acc)
        | i == 2 = next i (d : '.' : acc)
        | (i - 2) `mod` 3 == 0 = next i (d : ',' : acc)
        | otherwise = next i (d : acc)

      next :: Int -> String -> Tuple Int String
      next i = Tuple (i + 1)
