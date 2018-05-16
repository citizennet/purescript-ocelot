module Ocelot.Form where

import Prelude

import Data.Array (head)
import Data.Either (Either(..))
import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (unwrap)
import Data.Record (get)
import Data.Symbol (class IsSymbol, SProxy(..))
import Ocelot.Data.Record (class SequenceRecord, sequenceRecord)
import Polyform.Validation (Validation(..), V(..))
import Type.Row (class RowToList)

-----
-- Custom form-building monoid (credit: @paluh)

-- Functions have monoid instances, but <> is defined
-- as composition (<<<). For our validation, we want
-- composition running the other way (>>>).
--
-- This will allow us to build forms monoidally.
newtype Endo a = Endo (a -> a)

instance semigroupEndo :: Semigroup (Endo f) where
  append (Endo f0) (Endo f1) = Endo (f0 >>> f1)

instance monoidEndo :: Monoid (Endo a) where
  mempty = Endo id

-----
-- Input and field types

-- An input represents something that can be stored in our
-- form representation. Our form will be a record filled with
-- these input types.
--
-- - vl: A variant pointing to the value field in the accompanying
--       raw form for our form representation.
-- - vd: A variant pointing to the validation field in the accompanying
--       raw form for our form representation
-- - e: Our error type for our input value if it fails validation
-- - a: Our raw input type from user input
-- - b: Our parsed type if it passes validation
--
-- Note: Value wrapped in Maybe to represent a case where we haven't
-- validated the field yet.
type FormInput iv vv e a b =
  { input          :: a
  , result         :: Maybe (Either e b)
  , setInput       :: iv
  , setValidate    :: vv
  , validate       :: Boolean
  }

-- Can be used to unwrap a `Maybe (Either (Array (Variant err))) a)` into a string
-- error message using `match` or another variant case.
check :: ∀ err a. Maybe (Either (Array err) a) -> (err -> String) -> Maybe String
check Nothing           _ = Nothing
check (Just (Right _))  _ = Nothing
check (Just (Left err)) f = f <$> head err



-----
-- Higher kinded data

-- We can use a single base row type to determine multiple variants and
-- records with different types but the same field names. For example,
-- given the type below:
--
-- `type FormFieldsT f = ( name :: f (label :: String) String )`
--
-- applying these types will construct new records:
--
-- `Record (FormFieldsT Snd) -> { name :: String }
--
-- or variants:
--
-- `Variant (FormFieldsT Fst) -> Variant ( name :: ( label :: String ) )
-- `Variant (FormFieldsT Snd) -> Variant ( name :: String )
-- `Variant (FormFieldsT (Const Int)) -> Variant ( name :: Int )

type First a b c = a
type Second a b c = b
type Third a b c = c
type K a b c d = a

-----
-- Form construction

-- A type that represents a form that can be composed with other
-- forms. This `Validation` type doesn't hold errors in its `e`
-- constructor but rather the fields of the form.

type Form m i o = Validation m (Endo i) i o

-- Turn a regular validation type into a form that can be composed
-- with others by running its validation and then either producing
-- the output value or producing a transformation to apply to the
-- underlying form. For example, below we transform the underlying
-- record to hold an error value if validation failed and the actual
-- parsed value if it succeeded.
--
-- Necessary to use Polyform `V` because we want to transform the
-- underlying record on success AND failure, in the case of partial
-- validation.
--
-- When fields are not meant to be validated, they will be skipped
-- and parsing will result in Nothing; however, errors will still
-- collect for other fields.
formFromField :: ∀ sym form t0 m vl vd e a b
   . IsSymbol sym
  => Monad m
  => RowCons sym (FormInput vl vd e a b) t0 form
  => SProxy sym
  -> Validation m e a b
  -> Form m (Record form) (Maybe b)
formFromField name validation = Validation $ \inputForm -> do
  let { input, validate } = get name inputForm
      set' = set (prop name <<< prop (SProxy :: SProxy "result"))
  if validate
    then do
      result <- unwrap validation input
      pure $ case result of
        Valid _ v ->
          Valid (Endo $ set' $ Just $ Right v) (Just v)
        Invalid e ->
          Invalid (Endo $ set' $ Just $ Left e)
    else pure $
      Valid (Endo $ set' Nothing) Nothing

-- This function will take some raw input and run it through validation
-- to create a transformation: Endo (form -> form). Then, it will run that
-- transformation on the initial record provided. If validation succeeds,
-- then you'll also receive the parsed output.
runForm :: ∀ m form output0 output1 outputl
  . Monad m
 => RowToList output0 outputl
 => SequenceRecord outputl output0 output1
 => Validation m (Endo (Record form)) (Record form) (Record output0)
 -> Record form
 -> m (V (Record form) (Maybe (Record output1)))
runForm formValidation initial = do
  result <- unwrap formValidation $ initial
  pure $ case result of
    Valid (Endo transform) v -> Valid (transform initial) (sequenceRecord v)
    Invalid (Endo transform) -> Invalid $ transform initial
