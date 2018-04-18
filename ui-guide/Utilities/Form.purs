module UIGuide.Utilities.Form where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
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

type InputConfig e a =
  { value :: Either e a
  , validate :: Boolean
  }

_value :: ∀ t r. Lens' { value :: t | r } t
_value = prop (SProxy :: SProxy "value")

_validate :: ∀ t r. Lens' { validate :: t | r } t
_validate = prop (SProxy :: SProxy "validate")


----------
-- Concrete Example

type PasswordFormT f =
  ( password1 :: f ("emptyField" :: String) String
  , password2 :: f ("emptyField" :: String) String
  )

_password1 = SProxy :: SProxy "password1"
_password2 = SProxy :: SProxy "password2"

type PasswordFormValue = Variant (PasswordFormT Two)
type PasswordFormValidate = Variant (PasswordFormT (Const Boolean))


-- Creates a form awaiting whatever the validation is awaiting and parsing to whetever
-- validation parses to, which can be combined with other forms to parse to a record
-- of what they all parse to
password1 = mkForm _password1 (validateNonEmptyStr *> validateNonEmptyStr)
password2 = mkForm _password1 (validateNonEmptyStr *> validateNonEmptyStr)

-- This is the composition, creating a particular form; we've also added a custom validation 'checkEqual'
-- which serves to collapse two input fields into a single parsed output field.
myForm = ({ p1: _, p2: _ } <$> password1 <*> password2)
  >>> Polyform.hoistFnV \{ p1, p2 } ->
    collapseIfEqual p1 p2 _password1 _password2

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

mkForm :: ∀ sym input form trash0 trash1 m e a
   . IsSymbol sym
  => Monad m
  => RowCons sym a trash0 input
  => RowCons sym (InputConfig e a) trash1 form
  => SProxy sym
  -> (a -> V e a)
  -> Polyform.Validation m (FunctionR (Record form)) (Record input) a
mkForm fieldSymbol validator = Polyform.hoistFnV \inputForm -> do
  let result = validator (get fieldSymbol inputForm)
  unV
    (\e -> Polyform.Invalid $ FunctionR $ set (prop fieldSymbol <<< _value) (Left e))
    (\v -> Polyform.Valid (FunctionR $ set (prop fieldSymbol <<< _value) (Right v)) v)
    result


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
runValidation :: ∀ m form a input. Monad m => Polyform.Validation m (FunctionR form) input a -> input -> m (Either form a)
runValidation form input = do
  result <- Polyform.runValidation form input
  pure $ case result of
    Polyform.Valid (FunctionR transform) a -> Right a
    Polyform.Invalid (FunctionR transform) -> Left $ transform (unsafeCoerce {})
