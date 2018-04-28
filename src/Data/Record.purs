module Ocelot.Data.Record where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Record (get, insert)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, inj)
import Ocelot.Data.Default (class Default, def)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), RProxy(..), kind RowList)

-----
-- Sequencing form records

-- To support partial validation, each field in our record will
-- parse to `Maybe value`. If the field isn't meant to be validated
-- it will return `Nothing`, and if it is, it will return `Just a` if
-- the validation is successful.
--
-- This will produce a record like this:
-- { a :: Maybe Email, b :: Maybe Password }
--
-- However, our data types are going to be like this:
-- type UserLogin = { a :: Email, p :: Password }
--
-- This class provides a function `sequenceImpl` which can be used to
-- create a record fold that will invert the applicative. Given
-- that initial record, you'll produce:
-- Maybe { a :: Email, p :: Password }

-- The `SequenceRecord` class here is implemented for Maybe, but it could be
-- changed to support any arbitrary applicative `f`.
class SequenceRecord (rl :: RowList) (r :: # Type) (o :: # Type) | rl -> o where
  sequenceImpl :: RLProxy rl -> Record r -> Maybe (Record o)

-- In the base case when we have an empty record, we'll return it.
instance nilSequenceRecord :: SequenceRecord Nil r () where
  sequenceImpl _ _ = Just {}

-- Otherwise we'll accumulate the value at the head of the list into
-- our base (Just {}) and then recursively call `sequenceImpl` on our tail.
-- If we have a `Nothing` at any point, the entire structure will short-
-- circuit with `Nothing` as the result.
instance consSequenceRecord
  :: ( IsSymbol name
     , RowCons name a tail' o
     , RowCons name (Maybe a) t0 r , RowLacks name tail'
     , SequenceRecord tail r tail'
     )
  => SequenceRecord (Cons name (Maybe a) tail) r o
  where
    sequenceImpl _ r =
      -- This has to be defined in a variable for some reason; it won't
      -- compile otherwise, but I don't know why not.
      let tail' = sequenceImpl (RLProxy :: RLProxy tail) r
       in insert (SProxy :: SProxy name)
          <$> get (SProxy :: SProxy name) r
          <*> tail'

-- With our `SequenceRecord` class we can define this function, which will turn
-- a record of `{ Maybe a }` into `Maybe { a }`.
sequenceRecord
  :: ∀ r rl o
   . SequenceRecord rl r o
  => RowToList r rl
  => Record r
  -> Maybe (Record o)
sequenceRecord r = sequenceImpl (RLProxy :: RLProxy rl) r


----------
-- Default Record Builders

-- We want to generate raw form representations from a form spec.

class DefaultFormInputs (rl :: RowList) (r :: # Type) (o :: # Type) | rl -> o where
  defaultFormInputs :: RLProxy rl -> RProxy r -> Record o

-- In the base case when we have an empty record, we'll return it.
instance nilDefaultFormInputs :: DefaultFormInputs Nil r () where
  defaultFormInputs _ _ = {}

-- Otherwise we'll accumulate the value at the head of the list into
-- our base.
instance consDefaultFormInputs
  :: ( IsSymbol name
     , Default a
     , RowCons name { value :: a
                    , shouldValidate :: Boolean
                    , validated :: Maybe (Either e b)
                    , setValue :: a -> (Variant r0)
                    , setValidate :: Boolean -> (Variant r1)
                    } tail' o
     , RowCons name a t0 r0
     , RowCons name Boolean t1 r1
     , RowLacks name tail'
     , DefaultFormInputs tail r0 tail'
     )
  => DefaultFormInputs (Cons name a tail) r0 o
  where
    defaultFormInputs _ r =
      let tail' = defaultFormInputs (RLProxy :: RLProxy tail) (RProxy :: RProxy r0)
          _name = SProxy :: SProxy name
       in insert _name { value: def, shouldValidate: false, validated: Nothing, setValue: inj _name, setValidate: inj _name } tail'

makeDefaultFormInputs
  :: ∀ r rl o
   . DefaultFormInputs rl r o
  => RowToList r rl
  => RProxy r
  -> Record o
makeDefaultFormInputs r = defaultFormInputs (RLProxy :: RLProxy rl) r
