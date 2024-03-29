module Ocelot.Data.Currency where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (all, drop, head, (:))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (note)
import Data.Foldable (foldr)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (abs)
import Data.String (Pattern(..), Replacement(..), length, null, replaceAll, split, take)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), snd)
import Prim.TypeError (class Warn, Text)

----------
-- CENTS

-- | Cents represent USD as a BigInt
newtype Cents = Cents BigInt

derive instance newtypeCents :: Newtype Cents _
derive newtype instance eqCents :: Eq Cents
derive newtype instance ordCents :: Ord Cents

instance encodeJsonCents :: EncodeJson Cents where
  encodeJson = encodeJson <<< centsToNumber

instance decodeJsonCents :: DecodeJson Cents where
  decodeJson json = lmap (Named "Cents") do
    num <- decodeJson json
    note (UnexpectedValue $ Data.Argonaut.Core.fromNumber num) (parseCentsFromNumber num)

instance showCents :: Show Cents where
  show (Cents n) = "(Cents " <> BigInt.toString n <> ")"

----------
-- FROM CENTS

centsToMaybeInt :: Cents -> Maybe Int
centsToMaybeInt = Int.fromString <<< BigInt.toString <<< unwrap

centsToNumber :: Cents -> Number
centsToNumber = BigInt.toNumber <<< unwrap

centsToString :: Cents -> String
centsToString = BigInt.toString <<< unwrap

----------
-- TO CENTS

centsFromInt :: Int -> Cents
centsFromInt = Cents <<< BigInt.fromInt

parseCentsFromNumber :: Number -> Maybe Cents
parseCentsFromNumber = map Cents <<< BigInt.fromNumber

parseCentsFromString :: String -> Maybe Cents
parseCentsFromString = map Cents <<< BigInt.fromString

parseCentsFromMicroDollars ::
  Warn (Text "parseCentsFromMicroDollars is deprecated and will be removed in a future release") =>
  Number ->
  Maybe Cents
parseCentsFromMicroDollars = parseCentsFromNumber <<< (_ / 10000.0)

-----------
-- STRING FORMATTING

-- | Will attempt to parse cents from a string representing a dollar amount. It will
-- strip any trailing cents beyond the hundredths place. WARNING: Do not use this on
-- a string meant to represent cents! It will overstate by 100x.
parseCentsFromDollarStr :: String -> Maybe Cents
parseCentsFromDollarStr str = Cents <$> case split (Pattern ".") str of
  -- There is no decimal; treat as a full dollar string
  [ dollars ] -> bigIntIs64Bit =<< dollarsPlace dollars

  -- There is one decimal; truncate the cents to 2 places and
  -- add them to the dollars
  [ dollars, cents ] -> do
    dollars' <- dollarsPlace dollars
    cents' <- BigInt.fromString (take 2 $ cents <> "0")
    let bigInt = if dollars' < zero then dollars' - cents' else dollars' + cents'
    bigIntIs64Bit bigInt

  -- Unable to parse
  _ -> Nothing

  where
  -- Expects only the dollars place, no cents. Cents will overstate
  -- by 100x! Verifies within 64 bit bounds.
  dollarsPlace :: String -> Maybe BigInt
  dollarsPlace s
    | null s = Nothing
    | otherwise =
        pure
          <<< (*) (BigInt.fromInt 100)
          =<< BigInt.fromString
          =<< cleanDollars s

  cleanDollars :: String -> Maybe String
  cleanDollars s =
    let
      split = splitCommas s
      verified = noCommas s || checkHead split && checkTail split
    in
      if verified then Just (stripCommas s) else Nothing

  noCommas s = s == stripCommas s
  splitCommas = split (Pattern ",")
  stripCommas = replaceAll (Pattern ",") (Replacement "")
  checkHead = fromMaybe false <<< map ((_ <= 3) <<< length) <<< head
  checkTail = all (_ == 3) <<< map length <<< drop 1

-- Given some cents, format a string representation
-- in dollars.
formatCentsToStrDollars :: Cents -> String
formatCentsToStrDollars (Cents n)
  | abs (BigInt.toNumber n) < 10.0 = (if n < zero then "-" else "") <> "0.0" <> BigInt.toString n
  | abs (BigInt.toNumber n) < 100.0 = (if n < zero then "-" else "") <> "0." <> BigInt.toString n
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
  if n <= max then pure n
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
