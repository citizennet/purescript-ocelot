module Ocelot.Data.Currency where

import Prelude

import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (all, drop, head, (:))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (foldr)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), Replacement(..), length, replaceAll, split, take, null)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.Tuple (Tuple(..), snd)

----------
-- CENTS

-- | Cents represent USD as a BigInt
newtype Cents = Cents BigInt
derive instance newtypeCents :: Newtype Cents _
derive newtype instance eqCents :: Eq Cents

instance encodeJsonCents :: EncodeJson Cents where
  encodeJson = encodeJson <<< BigInt.toNumber <<< unwrap

instance showCents :: Show Cents where
  show (Cents n) = "Cents " <> BigInt.toString n

----------------
-- MICRO DOLLARS

newtype MicroDollars = MicroDollars BigInt
derive instance newtypeMicroDollars :: Newtype MicroDollars _
derive newtype instance eqMicroDollars :: Eq MicroDollars

instance encodeJsonMicroDollars :: EncodeJson MicroDollars where
  encodeJson = encodeJson <<< BigInt.toNumber <<< unwrap

instance showMicroDollars :: Show MicroDollars where
  show (MicroDollars n) = "MicroDollars " <> BigInt.toString n

-------------------------
-- Parsing Functions

-- | Will parse cents from a 32bit int
parseCentsFromNumber :: Number -> Maybe Cents
parseCentsFromNumber = map Cents <<< BigInt.fromNumber

parseCentsFromMicroDollars :: Number -> Maybe Cents
parseCentsFromMicroDollars = parseCentsFromNumber <<< flip (/) 10000.0

centsToMaybeInt :: Cents -> Maybe Int
centsToMaybeInt = Int.fromString <<< BigInt.toString <<< unwrap

parseCentsFromDollarStr :: String -> Maybe Cents
parseCentsFromDollarStr = map Cents <<< parseCurrencyFromDollarStr 100

parseMicroDollarsFromDollarStr :: String -> Maybe MicroDollars
parseMicroDollarsFromDollarStr = map MicroDollars <<< parseCurrencyFromDollarStr 1000000

-- | Will attempt to parse cents from a string representing a dollar amount. It will
-- strip any trailing cents beyond the hundredths place. WARNING: Do not use this on
-- a string meant to represent cents! It assumes non-decimal numbers are whole dollars.
parseCurrencyFromDollarStr :: Int -> String -> Maybe BigInt
parseCurrencyFromDollarStr multiplier str =
  case splitDecimal $ stripCommas str of
    [ whole ] ->
      convertCurrency whole

    [ whole, d ] ->
      let decimal = padRight 2 "0" d
      in convertCurrency whole `fPlus` (convertCurrency decimal `bigDiv` 100)

    _ ->
      Nothing
  where
    stripCommas = replaceAll (Pattern ",") (Replacement "")
    splitDecimal = split (Pattern ".")
    convertCurrency c = bigIntIs64Bit =<< (BigInt.fromInt multiplier) `fMult` (BigInt.fromString c)
    padRight n char s = take n $ s <> char

    fMult a b = (*) a <$> b
    fPlus a b = (+) <$> a <*> b -- Best grade
    bigDiv a b = BigInt.quot <$> a <*> (Just $ BigInt.fromInt b)


-- Given some cents, format a string representation
-- in dollars.
formatCentsToStrDollars :: Cents -> String
formatCentsToStrDollars (Cents n)
  | BigInt.toNumber n < 10.0  = "0.0" <> BigInt.toString n
  | BigInt.toNumber n < 100.0 = "0." <> BigInt.toString n
  | otherwise = fromCharArray <<< snd $ foldr formatCentsToDollars' (Tuple 0 []) (chars n)
    where
      chars :: BigInt -> Array Char
      chars = toCharArray <<< BigInt.toString

      formatCentsToDollars' :: Char -> Tuple Int (Array Char) -> Tuple Int (Array Char)
      formatCentsToDollars' d (Tuple i acc)
        | i == 2 = next i (d : '.' : acc)
        | (i - 2) `mod` 3 == 0 = next i (d : ',' : acc)
        | otherwise = next i (d : acc)

      next :: Int -> Array Char -> Tuple Int (Array Char)
      next i = Tuple (i + 1)

-- | Simple test to ensure a string can be parsed to a BigInt before
-- other steps are taken, or for validation purposes.
canParseToBigInt :: String -> Boolean
canParseToBigInt = isJust <<< BigInt.fromString

-- | Verify cents are within 64 bit bounds
bigIntIs64Bit :: BigInt -> Maybe BigInt
bigIntIs64Bit n = do
  max <- BigInt.fromString "9223372036854775807"
  if n <= max
    then pure n
    else Nothing

-- | Simple check to see if the parsed int would fit within
-- 64 bits (9,223,372,036,854,775,807)
canParseTo64Bit :: String -> Boolean
canParseTo64Bit str = isJust $ do
  max <- BigInt.fromString "9223372036854775807"
  cmp <- BigInt.fromString str
  if cmp <= max then Just str else Nothing

-- | Simple check to see if the parsed int would fit within
-- 32 bits (2,147,483,647)
canParseTo32Bit :: String -> Boolean
canParseTo32Bit str = isJust $ do
  max <- BigInt.fromString "2147483647"
  cmp <- BigInt.fromString str
  if cmp <= max then Just str else Nothing
