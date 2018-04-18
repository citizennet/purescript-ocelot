module UIGuide.Utilities.Form where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Record (get)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, unV)
import Data.Variant (Variant, inj)
import Ocelot.Core.Validation (validateNonEmptyStr)
import Polyform.Validation as Polyform
import Unsafe.Coerce (unsafeCoerce)

----------
-- Constants

type Two (a :: # Type) b = b
type Const z (a :: # Type) b = z

type InputConfig attrs vl vd e a =
  { validated   :: Maybe (Either e a)
  , setValue    :: vl
  , setValidate :: vd
  | attrs
  }

type FieldConfig a =
  { value    :: a
  , validate :: Boolean
  }

_value :: ∀ t r. Lens' { value :: t | r } t
_value = prop (SProxy :: SProxy "value")

_validated :: ∀ t r. Lens' { validated :: t | r } t
_validated = prop (SProxy :: SProxy "validated")

_validate :: ∀ t r. Lens' { validate :: t | r } t
_validate = prop (SProxy :: SProxy "validate")

-- Unnecessary duplication here because of the fucking lens types. But this is a function that verifies
-- two fields are equal, and if so, collapses their output to a single field. If there is an error, then
-- both fields collect the error.
collapseIfEqual a b symA symB = case a == b of
  true -> Polyform.Valid (FunctionR id) a
  false -> Polyform.Invalid $ FunctionR setErrors
    where
      err = inj (SProxy :: SProxy "notEqual") (Tuple a b)
      setErrors rec =
        rec
        # set
          (prop symA <<< _value)
          (case view (prop symA <<< _value) rec of
            Right _ -> Left [ err ]
            Left errs -> Left ( err : errs )
          )
        # set
          (prop symB <<< _value)
          (case view (prop symB <<< _value) rec of
            Right _ -> Left [ err ]
            Left errs -> Left ( err : errs )
          )


----------
-- Make forms from fields

mkForm :: ∀ sym input form trash0 trash1 m attrs vl vd e a b
   . IsSymbol sym
  => Monad m
  => RowCons sym (FieldConfig a) trash0 input
  => RowCons sym (InputConfig attrs vl vd e b) trash1 form
  => SProxy sym
  -> (a -> V e b)
  -> Polyform.Validation m (FunctionR (Record form)) (Record input) (Maybe b)
mkForm fieldSymbol validator = Polyform.hoistFnV \inputForm -> do
  let field = get fieldSymbol inputForm
      result = validator $ _.value field
  case _.validate field of
    true  -> unV
      (\e -> Polyform.Invalid
             $ FunctionR
             $ set (prop fieldSymbol <<< _validated) (Just $ Left e))
      (\v -> Polyform.Valid
             (FunctionR (set (prop fieldSymbol <<< _validated) (Just $ Right v)))
             (Just v))
        result
    false -> Polyform.Valid (FunctionR id) Nothing


----------
-- Form monoid

-- Necessary to define custom instances for functions so that
-- composition runs in the opposite direction
newtype FunctionR a = FunctionR (a -> a)

instance semigroupFunctionR :: Semigroup (FunctionR a) where
  append (FunctionR a) (FunctionR b) = FunctionR (a >>> b)

instance monoidFunctionR :: Monoid (FunctionR a) where
  mempty = FunctionR id

----------
-- Validation newtype

-- Will evaluate the form on an input, resulting either in the form returned with errors, or a successful parse
-- to an output value.
runValidation :: ∀ m form a input
  . Monad m
 => Polyform.Validation m (FunctionR form) input (Maybe a)
 -> input
 -> m (Either form a)
runValidation form input = do
  result <- Polyform.runValidation form input
  pure $ case result of
    Polyform.Valid _ (Just a) -> Right a
    Polyform.Valid (FunctionR transform) _ -> Left $ transform (unsafeCoerce {})
    Polyform.Invalid (FunctionR transform) -> Left $ transform (unsafeCoerce {})
