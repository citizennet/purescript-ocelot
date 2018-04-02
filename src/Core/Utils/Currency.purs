-- | Constructors are not exported for Cents or Dollars
module Ocelot.Core.Utils.Currency
  ( Cents
  , parseCentsFromDollarStr
  , formatCentsToStrDollars
  , canParseToBigInt
  , canParseTo64Bit
  , canParseTo32Bit
  )
  where

import Prelude

import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (all, drop, head)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), Replacement(..), length, take, split)
import Data.String.Yarn (substitute, toChars, (:))
import Data.Tuple (Tuple(..), snd)

----------
-- CENTS

-- | Cents represent USD as a BigInt
newtype Cents = Cents BigInt
derive instance newtypeCents :: Newtype Cents _

instance encodeJsonCents :: EncodeJson Cents where
  encodeJson = encodeJson <<< BigInt.toNumber <<< unwrap

instance showCents :: Show Cents where
  show (Cents n) = "Cents: " <> show n

-- | Will attempt to parse cents from a string representing a dollar amount. It will
-- strip any trailing cents beyond the hundredths place. WARNING: Do not use this on
-- a string meant to represent cents! It will overstate by 100x.
parseCentsFromDollarStr :: String -> Maybe Cents
parseCentsFromDollarStr str = case split (Pattern ".") str of
  -- There is no decimal; treat as a full dollar string
  [ dollars ] -> Cents <$> dollarsPlace dollars
  [ dollars, cents ] -> map Cents $
    (+) <$> dollarsPlace dollars <*> BigInt.fromString (take 2 cents)
  otherwise -> Nothing
  where
    -- Expects only the dollars place, no cents. Cents will overstate
    -- by 100x!
    dollarsPlace :: String -> Maybe BigInt
    dollarsPlace s = pure
      <<< (*) (BigInt.fromInt 100)
      =<< BigInt.fromString
      =<< cleanDollars s

    cleanDollars :: String -> Maybe String
    cleanDollars s =
      let split = splitCommas s
          verified = noCommas s || checkHead split && checkTail split
       in if verified then Just (stripCommas s) else Nothing

    noCommas s = s == stripCommas s
    splitCommas = split (Pattern ",")
    stripCommas = substitute (Pattern ",") (Replacement "")
    checkHead = fromMaybe false <<< map ((_ <= 3) <<< length) <<< head
    checkTail = all (_ == 3) <<< map length <<< drop 1

----------
-- CONVERSIONS

-- Given some cents, format a string representation
-- in dollars.
formatCentsToStrDollars :: Cents -> String
formatCentsToStrDollars (Cents n)
  | BigInt.toNumber n < 10.0  = "0.0" <> show n
  | BigInt.toNumber n < 100.0 = "0." <> show n
  | otherwise = snd $ foldr formatCentsToDollars' (Tuple 0 "") (chars n)
    where
      chars :: BigInt -> Array Char
      chars = toChars <<< BigInt.toString

      formatCentsToDollars' :: Char -> Tuple Int String -> Tuple Int String
      formatCentsToDollars' d (Tuple i acc)
        | i == 2 = next i (d : '.' : acc)
        | (i - 2) `mod` 3 == 0 = next i (d : ',' : acc)
        | otherwise = next i (d : acc)

      next :: Int -> String -> Tuple Int String
      next i = Tuple (i + 1)

-- | Simple test to ensure a string can be parsed to a BigInt before
-- other steps are taken, or for validation purposes.
canParseToBigInt :: String -> Boolean
canParseToBigInt = isJust <<< BigInt.fromString

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

