module CN.UI.Core.Validation where

import Prelude

import Data.Array (singleton)
import Data.Foldable (class Foldable, length)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.String (null)
import Data.Validation.Semigroup (V, invalid)
import Halogen.HTML as HH
import Text.Email.Validate (isValid)

-----
-- Validation types

type ValidationErrors = Array ValidationError

data ValidationError
  = EmptyField
  | InvalidEmail String
  | UnderMinLength Int String
  | OutOfRange String
  | Dependency String

derive instance genericValidationError :: Generic ValidationError _

instance eqValidationError :: Eq ValidationError where
  eq = genericEq

instance showValidationError :: Show ValidationError where
  show = genericShow

-----
-- Possible validations to run on any field

validateNonEmptyStr :: String -> V ValidationErrors String
validateNonEmptyStr str
  | null str = invalid $ pure EmptyField
  | otherwise = pure str

validateNonEmptyArr :: ∀ a. Array a -> V ValidationErrors (Array a)
validateNonEmptyArr [] = invalid $ pure EmptyField
validateNonEmptyArr xs = pure xs

validateStrIsEmail :: String -> String -> V ValidationErrors String
validateStrIsEmail msg email
  | isValid email = pure email
  | otherwise = invalid $ pure (InvalidEmail msg)

validateMinLength :: ∀ f a. Foldable f => Int -> String -> f a -> V ValidationErrors (f a)
validateMinLength n msg f
  | length f >= n = pure f
  | otherwise = invalid $ pure (UnderMinLength n msg)

validateInRange :: ∀ a. Ord a => a -> a -> a -> String -> V ValidationErrors a
validateInRange low high num msg
  | low <= num && num <= high = pure num
  | otherwise = invalid $ pure (OutOfRange msg)

validateDependence :: ∀ a b. (a -> b -> Boolean) -> a -> b -> String -> V ValidationErrors a
validateDependence f item1 item2 msg
  | f item1 item2 = pure item1
  | otherwise = invalid $ pure (Dependency msg)


-----
-- Helper functions for printing error messages from ValidationErrors

showE :: ValidationError -> String
showE EmptyField = "Required"
showE (InvalidEmail msg) = msg
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

