module Ocelot.Core.Form where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Record (get)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Validation.Semigroup (V, invalid, unV)
import Unsafe.Coerce (unsafeCoerce)

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
-- - attrs: Arbitrary additional labels we want available for
--          input, like help text
-- - vl: A variant pointing to the value field in the accompanying
--       raw form for our form representation.
-- - vd: A variant pointing to the validation field in the accompanying
--       raw form for our form representation
-- - e: Our error type for our input value if it fails validation
-- - a: Our successfully parsed type if it passes validation
--
-- Note: Value wrapped in Maybe to represent a case where we haven't
-- validated the field yet.
type FormInput attrs vl vd e a =
  { validated   :: Either e a
  , setValue    :: vl
  , setValidate :: vd
  | attrs
  }

-- Lens to access the `validated` field of a FormInput. Used to transform
-- our output type on validation.
_validated :: ∀ t r. Lens' { validated :: t | r } t
_validated = prop $ SProxy :: SProxy "validated"

-- A field represents a raw form field that can accept user input. All
-- forms should have the exact same named fields in FormInput format
-- and in FormField format. DOM interaction happens in FormField;
-- validation control and results happen in FormInput.
--
-- a: The raw DOM value for this input. Should be the same input type
--    as what is required for the accompanying FormInput validation.
type FormField a =
  { value          :: a
  , shouldValidate :: Boolean
  }

_value :: ∀ t r. Lens' { value :: t | r } t
_value = prop (SProxy :: SProxy "value")

_shouldValidate :: ∀ t r. Lens' { shouldValidate :: t | r } t
_shouldValidate = prop (SProxy :: SProxy "shouldValidate")

_validate :: ∀ t r. Lens' { validate :: t | r } t
_validate = prop (SProxy :: SProxy "validate")


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

type Fst (a :: # Type) b = a
type Snd (a :: # Type) b = b
type Const z (a :: # Type) b = z


-----
-- Form construction

-- A type that represents a form that can be composed with other
-- forms. This `Validation` type doesn't hold errors in its `e`
-- constructor but rather the fields of the form.
type Form m form input output =
  Validation m (Endo (Record form)) (Record input) output

-- Turn a regular validation type into a form that can be composed
-- with others by running its validation and then either producing
-- the output value or producing a transformation to apply to the
-- underlying form. For example, below we transform the underlying
-- record to hold an error value if validation failed and the actual
-- parsed value if it succeeded.
formFromField :: ∀ sym input form t0 t1 m attrs vl vd e a b
   . IsSymbol sym
  => Monad m
  => RowCons sym (FormField a) t0 input
  => RowCons sym (FormInput attrs vl vd e b) t1 form
  => SProxy sym
  -> Validation m e a b
  -> Form m form input b
formFromField name validation = Validation $ \inputForm -> do
  result <- unwrap validation <<< _.value <<< get name $ inputForm
  pure $ unV
    (\e -> invalid $ Endo (set (prop name <<< _validated) (Left e)))
    (\v -> pure v)
    result

-- Given a form, produces either the original input form after transforming
-- it, or produces the output value you wanted.
runForm :: ∀ m form input output
  . Monad m
 => Form m form input output
 -> Record input
 -> m (Either (Record form) output)
runForm formValidation input = do
  result <- unwrap formValidation $ input
  pure $ unV
    (\(Endo transform) -> Left $ transform $ unsafeCoerce {})
    (\a -> Right a)
    result

-----
-- Custom variation on the Polyform validation type

newtype Validation m e a b = Validation (a -> m (V e b))

derive instance newtypeValidation
  :: Newtype (Validation m e a b) _

derive instance functorValidation
  :: (Functor m) => Functor (Validation m e a)

instance semigroupValidation
  :: (Semigroup (m (V e b))) => Semigroup (Validation m e a b)
  where
    append (Validation v0) (Validation v1) =
      Validation $ \a -> v0 a <> v1 a

instance monoidValidation
  :: (Applicative m, Monoid e, Monoid b, Semigroup (m (V e b)))
  => Monoid (Validation m e a b)
  where
    mempty = Validation <<< const <<< pure $ mempty

instance applyValidation
  :: (Semigroup e, Monad m) => Apply (Validation m e a)
  where
    apply vf va = Validation \i -> do
      vf' <- unwrap vf i
      va' <- unwrap va i
      pure $ vf' <*> va'

instance applicativeValidation
  :: (Monoid e, Monad m) => Applicative (Validation m e a)
  where
    pure = Validation <<< const <<< pure <<< pure

hoistFn :: ∀ m e a b
  . Monad m
 => Monoid e
 => (a -> b)
 -> Validation m e a b
hoistFn = hoistFnV <<< map pure

hoistFnV :: ∀ m e a b
  . Monad m
 => Monoid e
 => (a -> V e b)
 -> Validation m e a b
hoistFnV = hoistFnMV <<< map pure

hoistFnMV :: ∀ m e a b
  . Monad m
 => Monoid e
 => (a -> m (V e b))
 -> Validation m e a b
hoistFnMV = Validation