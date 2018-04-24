module Ocelot.Core.Validation where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, length)
import Data.Int as Integer
import Data.Lens (set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Num
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), Variant, inj)
import Ocelot.Core.Form (Endo(..), _validated)
import Ocelot.Core.Utils.Currency (Cents, canParseTo32Bit, parseCentsFromDollarStr)
import Polyform.Validation (V(..))
import Text.Email.Validate as Email


-----
-- Possible validations to run on any field

type Err err = Array (Variant err)

validateNonEmptyStr
  :: ∀ err
   . String
  -> V (Err (emptyField :: String | err)) String
validateNonEmptyStr str
  | String.null str =
      Invalid [ inj (SProxy :: SProxy "emptyField") "Required" ]
  | otherwise = pure str

validateNonEmptyArr
  :: ∀ err a
   . Array a
  -> V (Err (emptyField :: String | err)) (Array a)
validateNonEmptyArr [] =
  Invalid [ inj (SProxy :: SProxy "emptyField") "Required" ]
validateNonEmptyArr xs = pure xs

validateNonEmptyMaybe
  :: ∀ err a
   . Maybe a
  -> V (Err (emptyField :: String | err)) a
validateNonEmptyMaybe (Just a) = pure a
validateNonEmptyMaybe Nothing =
  Invalid [ inj (SProxy :: SProxy "emptyField") "Required" ]

validateStrIsEmail
  :: ∀ err
   . String
  -> String
  -> V (Err (badEmail :: String | err)) String
validateStrIsEmail msg str
  | Email.isValid str = pure str
  | otherwise = Invalid [ inj (SProxy :: SProxy "badEmail") msg ]

validateStrIsNumber
  :: ∀ err
   . String
  -> String
  -> V (Err (invalidNumber :: String | err)) Number
validateStrIsNumber msg = Num.fromString >>>
  maybe (Invalid [ inj (SProxy :: SProxy "invalidNumber") msg ] ) pure

validateStrIsCents
  :: ∀ err
   . String
  -> String
  -> V (Err (invalidCurrency :: String | err)) Cents
validateStrIsCents msg = parseCentsFromDollarStr >>>
  maybe (Invalid [ inj (SProxy :: SProxy "invalidCurrency") msg ]) pure

validateStrIsInt
  :: ∀ err
   . String
  -> String
  -> V (Err (invalidInteger :: String | err)) Int
validateStrIsInt msg s
  | canParseTo32Bit s = s # Integer.fromString >>>
      maybe (Invalid [ inj (SProxy :: SProxy "invalidInteger") msg ]) pure
  | otherwise = Invalid [ inj (SProxy :: SProxy "invalidInteger") msg ]

validateMinLength
  :: ∀ err f a
   . Foldable f
  => Int
  -> String
  -> f a
  -> V (Err (underMinLength :: Tuple Int String | err)) (f a)
validateMinLength n msg f
  | length f >= n = pure f
  | otherwise =
      Invalid [ inj (SProxy :: SProxy "underMinLength") (Tuple n msg) ]

validateInRange
  :: ∀ err a
   . Ord a
  => a
  -> a
  -> String
  -> a
  -> V (Err (outOfRange :: String | err)) a
validateInRange low high msg num
  | low <= num && num <= high = pure num
  | otherwise = Invalid [ inj (SProxy :: SProxy "outOfRange") msg ]

validateGreaterThan
  :: ∀ err a
   . Ord a
  => a
  -> String
  -> a
  -> V (Err (notGreaterThan :: String | err)) a
validateGreaterThan min msg num
  | num > min = pure num
  | otherwise = Invalid [ inj (SProxy :: SProxy "notGreaterThan") msg ]

validateLessThan :: ∀ err a
  . Ord a
 => a
 -> String
 -> a
 -> V (Err (notLessThan :: String | err)) a
validateLessThan max msg num
  | num < max = pure num
  | otherwise = Invalid [ inj (SProxy :: SProxy "notLessThan") msg ]

validateDependence :: ∀ err a b
  . (a -> b -> Boolean)
 -> String
 -> a
 -> b
 -> V (Err (dependency :: String | err)) a
validateDependence f msg item1 item2
  | f item1 item2 = pure item1
  | otherwise = Invalid [ inj (SProxy :: SProxy "dependency") msg ]


-----
-- Additional helpers for converting to and from Either
-- Useful for combining monadic and applicative validation

toEither :: ∀ err a. Semigroup err => V err a -> Either err a
toEither (Invalid e) = Left e
toEither (Valid _ a) = Right a

-----
-- Multi-field and dependent validation

collapseIfEqual a b symA = case a == b of
  true -> pure a
  false -> Invalid $ Endo setErrors
    where
      err = inj (SProxy :: SProxy "notEqual") (Tuple a b)
      setErrors rec =
        rec
        # set
          (prop symA <<< _validated)
          (case view (prop symA <<< _validated) rec of
            Just (Left errs) -> Just $ Left ( err : errs )
            otherwise -> Just $ Left [ err ]
          )
