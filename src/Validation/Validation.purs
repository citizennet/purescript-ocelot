module Ocelot.Validation where

import Prelude

import Data.Array (singleton)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, length)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Integer
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Num
import Data.String as String
import Data.Validation.Semigroup (V, invalid, unV)
import Halogen.HTML as HH
import Ocelot.Utils.Currency (Cents, canParseTo32Bit, parseCentsFromDollarStr)
import Text.Email.Validate (isValid)

-----
-- Validation types

type ValidationErrors = Array ValidationError

data ValidationError
  = EmptyField
  | InvalidEmail
  | InvalidNumber
  | InvalidCurrency
  | InvalidInteger
  | UnderMinLength Int String
  | OutOfRange String
  | NotGreaterThan String
  | NotLessThan String
  | Dependency String

derive instance genericValidationError :: Generic ValidationError _

instance eqValidationError :: Eq ValidationError where
  eq = genericEq

instance showValidationError :: Show ValidationError where
  show = genericShow

type ErrorMessage = String

-----
-- Possible validations to run on any field

validateNonEmptyStr :: String -> V ValidationErrors String
validateNonEmptyStr str
  | String.null str = invalid $ pure EmptyField
  | otherwise = pure str

validateNonEmptyArr :: ∀ a. Array a -> V ValidationErrors (Array a)
validateNonEmptyArr [] = invalid $ pure EmptyField
validateNonEmptyArr xs = pure xs

validateNonEmptyMaybe :: ∀ a. Maybe a -> V ValidationErrors a
validateNonEmptyMaybe (Just a) = pure a
validateNonEmptyMaybe Nothing = invalid $ pure EmptyField

validateStrIsEmail :: String -> V ValidationErrors String
validateStrIsEmail email
  | isValid email = pure email
  | otherwise = invalid $ pure InvalidEmail

validateStrIsNumber :: String -> V ValidationErrors Number
validateStrIsNumber = maybe (invalid $ pure InvalidNumber) pure <<< Num.fromString

validateStrIsCents :: String -> V ValidationErrors Cents
validateStrIsCents s = maybe (invalid $ pure InvalidCurrency) pure <<< parseCentsFromDollarStr $ s

validateStrIsInt :: String -> V ValidationErrors Int
validateStrIsInt s
  | canParseTo32Bit s = maybe (invalid $ pure InvalidInteger) pure <<< Integer.fromString $ s
  | otherwise = invalid $ pure InvalidInteger

validateMinLength :: ∀ f a. Foldable f => Int -> ErrorMessage -> f a -> V ValidationErrors (f a)
validateMinLength n msg f
  | length f >= n = pure f
  | otherwise = invalid $ pure (UnderMinLength n msg)

validateInRange :: ∀ a. Ord a => a -> a -> ErrorMessage -> a -> V ValidationErrors a
validateInRange low high msg num
  | low <= num && num <= high = pure num
  | otherwise = invalid $ pure (OutOfRange msg)

validateGreaterThan :: ∀ a. Ord a => a -> ErrorMessage -> a -> V ValidationErrors a
validateGreaterThan min msg num
  | num > min = pure num
  | otherwise = invalid $ pure (NotGreaterThan msg)

validateLessThan :: ∀ a. Ord a => a -> ErrorMessage -> a -> V ValidationErrors a
validateLessThan max msg num
  | num < max = pure num
  | otherwise = invalid $ pure (NotLessThan msg)

validateDependence :: ∀ a b. (a -> b -> Boolean) -> ErrorMessage -> a -> b -> V ValidationErrors a
validateDependence f msg item1 item2
  | f item1 item2 = pure item1
  | otherwise = invalid $ pure (Dependency msg)


-----
-- Helper functions for printing error messages from ValidationErrors

showE :: ValidationError -> String
showE EmptyField = "Required"
showE InvalidEmail = "Must be a valid email"
showE InvalidNumber = "Must be a valid number"
showE InvalidCurrency = "Must be a valid dollar amount, like $500 or $2,250.90."
showE InvalidInteger = "Must be a valid integer"
showE (UnderMinLength _ msg) = msg
showE (OutOfRange msg) = msg
showE (NotGreaterThan msg) = msg
showE (NotLessThan msg) = msg
showE (Dependency msg) = msg

htmlE :: ValidationErrors -> Array HH.PlainHTML
htmlE es | length es == 1 = HH.text <<< showE <$> es
         | otherwise = toHTML
  where
    toHTML =
      [ HH.p_ [ HH.text "You have errors:" ]
      , HH.ul_ $ HH.li_ <<< singleton <<< HH.text <<< showE <$> es
      ]

-----
-- Additional helpers for converting to and from Either
-- Useful for combining monadic and applicative validation

toEither :: ∀ err a. Semigroup err => V err a -> Either err a
toEither = unV Left Right

fromEither :: ∀ err a. Semigroup err => Either err a -> V err a
fromEither = either invalid pure
