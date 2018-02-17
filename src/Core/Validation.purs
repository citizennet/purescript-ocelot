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
  = EmptyField String
  | InvalidEmail String
  | UnderMinLength Int String
  | Dependency String

derive instance genericValidationError :: Generic ValidationError _

instance eqValidationError :: Eq ValidationError where
  eq = genericEq

instance showValidationError :: Show ValidationError where
  show = genericShow

-----
-- Possible validations to run on any field

validateNonEmptyStr :: String -> String -> V ValidationErrors String
validateNonEmptyStr msg str
  | null str = invalid $ pure (EmptyField msg)
  | otherwise = pure str

validateNonEmptyArr :: ∀ a. String -> Array a -> V ValidationErrors (Array a)
validateNonEmptyArr msg [] = invalid $ pure (EmptyField msg)
validateNonEmptyArr _ xs = pure xs

validateStrIsEmail :: String -> String -> V ValidationErrors String
validateStrIsEmail msg email
  | isValid email = pure email
  | otherwise = invalid $ pure (InvalidEmail msg)

validateMinLength :: ∀ f a. Foldable f => Int -> String -> f a -> V ValidationErrors (f a)
validateMinLength n msg f
  | length f >= n = pure f
  | otherwise = invalid $ pure (UnderMinLength n msg)

validateDependence :: ∀ a b. (a -> b -> Boolean) -> a -> b -> String -> V ValidationErrors a
validateDependence f item1 item2 msg
  | f item1 item2 = pure item1
  | otherwise = invalid $ pure (Dependency msg)


-----
-- Helper functions for printing error messages from ValidationErrors

showE :: ValidationError -> String
showE (EmptyField msg) = msg
showE (InvalidEmail msg) = msg
showE (UnderMinLength n msg) = msg
showE (Dependency msg) = msg

htmlE :: ValidationErrors -> Array HH.PlainHTML
htmlE es | length es == 1 = HH.text <<< showE <$> es
         | otherwise = toHTML
  where
    toHTML =
      [ HH.p_ [ HH.text "You have errors:" ]
      , HH.ul_ $ HH.li_ <<< singleton <<< HH.text <<< showE <$> es
      ]

