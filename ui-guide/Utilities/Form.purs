module UIGuide.Utilities.Form where

import Ocelot.Core.Validation
import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Monoid (class Monoid)
import Data.Record (get)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, unV)
import Data.Variant (inj)
import Polyform.Validation as Polyform
import Unsafe.Coerce (unsafeCoerce)

----------
-- Constants

type Id a = a
type K a b = a

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

-- Creates a form awaiting whatever the validation is awaiting and parsing to whetever
-- validation parses to, which can be combined with other forms to parse to a record
-- of what they all parse to
password1 = mkForm (SProxy :: SProxy "password1") (validateNonEmptyStr *> validateNonEmptyStr)
password2 = mkForm (SProxy :: SProxy "password2") (validateNonEmptyStr *> validateNonEmptyStr)

-- This is the composition, creating a particular form.
myForm = ({ p1: _, p2: _ } <$> password1 <*> password2)
  >>> Polyform.hoistFnV \{ p1, p2 } ->
    if p1 == p2
      then Polyform.Valid (FunctionR id) p1
      else
        let err = inj (SProxy :: SProxy "notEqual") (Tuple p1 p2)
         in Polyform.Invalid
            $ FunctionR
            $ \r -> r
              { password2
                { value = case r.password2.value of
                    Right _ -> Left [ err ]
                    Left errs -> Left ( err : errs )
                }
              }

r =
  runValidation
    myForm
    { password1: "est", password2: "lkasdlkfalsdj" }


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

runValidation :: ∀ m form a input. Monad m => Polyform.Validation m (FunctionR form) input a -> input -> m (Either form a)
runValidation form input = do
  result <- Polyform.runValidation form input
  pure $ case result of
    Polyform.Valid (FunctionR transform) a -> Right a
    Polyform.Invalid (FunctionR transform) -> Left $ transform (unsafeCoerce {})
