module CN.UI.Core.Validation where

import Prelude

import Data.Array (singleton)
import Data.Foldable (class Foldable, length)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.Validation.Semigroup (V, invalid)
import Halogen.HTML as HH
import Text.Email.Validate (isValid)

-----
-- Validation types

type ValidationErrors = Array ValidationError

data ValidationError
  = EmptyField
  | InvalidEmail
  | UnderMinLength Int String
  | OutOfRange String
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
  | null str = invalid $ pure EmptyField
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

validateMinLength :: ∀ f a. Foldable f => Int -> ErrorMessage -> f a -> V ValidationErrors (f a)
validateMinLength n msg f
  | length f >= n = pure f
  | otherwise = invalid $ pure (UnderMinLength n msg)

validateInRange :: ∀ a. Ord a => a -> a -> ErrorMessage -> a -> V ValidationErrors a
validateInRange low high msg num
  | low <= num && num <= high = pure num
  | otherwise = invalid $ pure (OutOfRange msg)

validateDependence :: ∀ a b. (a -> b -> Boolean) -> ErrorMessage -> a -> b -> V ValidationErrors a
validateDependence f msg item1 item2
  | f item1 item2 = pure item1
  | otherwise = invalid $ pure (Dependency msg)


-----
-- Helper functions for printing error messages from ValidationErrors

showE :: ValidationError -> String
showE EmptyField = "Required"
showE InvalidEmail = "Must be a valid email"
showE (UnderMinLength _ msg) = msg
showE (Dependency msg) = msg
showE (OutOfRange msg) = msg

htmlE :: ValidationErrors -> Array HH.PlainHTML
htmlE es | length es == 1 = HH.text <<< showE <$> es
         | otherwise = toHTML
  where
    toHTML =
      [ HH.p_ [ HH.text "You have errors:" ]
      , HH.ul_ $ HH.li_ <<< singleton <<< HH.text <<< showE <$> es
      ]

