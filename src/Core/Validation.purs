module Ocelot.Core.Validation where

import Prelude

import Data.Variant (SProxy(..), Variant, inj)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, length)
import Data.Int as Integer
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Num
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import Ocelot.Core.Utils.Currency (Cents, canParseTo32Bit, parseCentsFromDollarStr)
import Text.Email.Validate (isValid)

-----
-- Possible validations to run on any field

type Error err = Array (Variant err)

validateNonEmptyStr
  :: ∀ err
   . String
  -> V (Error (emptyField :: String | err)) String
validateNonEmptyStr str
  | String.null str = invalid [ inj (SProxy :: SProxy "emptyField") "Required" ]
  | otherwise = pure str

validateNonEmptyArr
  :: ∀ err a
   . Array a
  -> V (Error (emptyField :: String | err)) (Array a)
validateNonEmptyArr [] =
  invalid [ inj (SProxy :: SProxy "emptyField") "Required" ]
validateNonEmptyArr xs = pure xs

validateNonEmptyMaybe
  :: ∀ err a
   . Maybe a
  -> V (Error (emptyField :: String | err)) a
validateNonEmptyMaybe (Just a) = pure a
validateNonEmptyMaybe Nothing =
  invalid [ inj (SProxy :: SProxy "emptyField") "Required" ]

validateStrIsEmail
  :: ∀ err
   . String
  -> String
  -> V (Error (badEmail :: String | err)) String
validateStrIsEmail msg email
  | isValid email = pure email
  | otherwise = invalid [ inj (SProxy :: SProxy "badEmail") msg ]

validateStrIsNumber
  :: ∀ err
   . String
  -> String
  -> V (Error (invalidNumber :: String | err)) Number
validateStrIsNumber msg = Num.fromString >>>
  maybe (invalid [ inj (SProxy :: SProxy "invalidNumber") msg ] ) pure

validateStrIsCents
  :: ∀ err
   . String
  -> String
  -> V (Error (invalidCurrency :: String | err)) Cents
validateStrIsCents msg = parseCentsFromDollarStr >>>
  maybe (invalid [ inj (SProxy :: SProxy "invalidCurrency") msg ]) pure

validateStrIsInt
  :: ∀ err
   . String
  -> String
  -> V (Error (invalidInteger :: String | err)) Int
validateStrIsInt msg s
  | canParseTo32Bit s = s # Integer.fromString >>>
      maybe (invalid [ inj (SProxy :: SProxy "invalidInteger") msg ]) pure
  | otherwise = invalid [ inj (SProxy :: SProxy "invalidInteger") msg ]

validateMinLength
  :: ∀ err f a
   . Foldable f
  => Int
  -> String
  -> f a
  -> V (Error (underMinLength :: Tuple Int String | err)) (f a)
validateMinLength n msg f
  | length f >= n = pure f
  | otherwise = invalid [ inj (SProxy :: SProxy "underMinLength") (Tuple n msg) ]

validateInRange
  :: ∀ err a
   . Ord a
  => a
  -> a
  -> String
  -> a
  -> V (Error (outOfRange :: String | err)) a
validateInRange low high msg num
  | low <= num && num <= high = pure num
  | otherwise = invalid [ inj (SProxy :: SProxy "outOfRange") msg ]

validateGreaterThan
  :: ∀ err a
   . Ord a
  => a
  -> String
  -> a
  -> V (Error (notGreaterThan :: String | err)) a
validateGreaterThan min msg num
  | num > min = pure num
  | otherwise = invalid [ inj (SProxy :: SProxy "notGreaterThan") msg ]

validateLessThan :: ∀ err a
  . Ord a
 => a
 -> String
 -> a
 -> V (Error (notLessThan :: String | err)) a
validateLessThan max msg num
  | num < max = pure num
  | otherwise = invalid [ inj (SProxy :: SProxy "notLessThan") msg ]

validateDependence :: ∀ err a b
  . (a -> b -> Boolean)
 -> String
 -> a
 -> b
 -> V (Error (dependency :: String | err)) a
validateDependence f msg item1 item2
  | f item1 item2 = pure item1
  | otherwise = invalid [ inj (SProxy :: SProxy "dependency") msg ]


-----
-- Additional helpers for converting to and from Either
-- Useful for combining monadic and applicative validation

toEither :: ∀ err a. Semigroup err => V err a -> Either err a
toEither = unV Left Right

fromEither :: ∀ err a. Semigroup err => Either err a -> V err a
fromEither = either invalid pure
