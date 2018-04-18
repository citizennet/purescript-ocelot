module UIGuide.Utilities.Form where

import Prelude

import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Monoid (class Monoid)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Validation.Semigroup (V, invalid, unV)
import Polyform.Validation as Polyform

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
-- Custom form builder

-- Necessary to define custom instances for functions so that
-- composition runs in the opposite direction
newtype FunctionR a = FunctionR (a -> a)

instance semigroupFunctionR :: Semigroup (FunctionR a) where
  append (FunctionR a) (FunctionR b) = FunctionR (a >>> b)

instance monoidFunctionR :: Monoid (FunctionR a) where
  mempty = FunctionR id


-- Used to convert an input field into a valid form
-- in Polyform

mkForm :: ∀ sym input form trash0 trash1 m e a
   . IsSymbol sym
  => Monad m
  => RowCons sym String trash0 input
  => RowCons sym (InputConfig a) trash1 form
  => SProxy sym
  -> V e a
  -> Polyform.Validation m (FunctionR (Record form)) (Record input) a
mkForm fieldName validator = Polyform.hoistFnMV \input -> do
  pure $ Invalid $ FunctionR $ set (prop name <<< _value) (Left ?a) ?a
