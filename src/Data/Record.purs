module Ocelot.Data.Record where

import Prelude

import Data.Either (Either)
import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Record (get, insert)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, inj, on)
import Ocelot.Data.Default (class Default, def)
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), RProxy(..), kind RowList)

-----
-- Sequencing form records

-- The `SequenceRecord` class here is implemented for Maybe, but it could be
-- changed to support any arbitrary applicative `f`. Turns a record of Maybes
-- into a Maybe record.
class SequenceRecord (rl :: RowList) (r :: # Type) (o :: # Type) | rl -> o where
  sequenceImpl :: RLProxy rl -> Record r -> Maybe (Record o)

instance nilSequenceRecord :: SequenceRecord Nil r () where
  sequenceImpl _ _ = Just {}

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

instance nilDefaultFormInputs :: DefaultFormInputs Nil r () where
  defaultFormInputs _ _ = {}

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


---------
-- Record Updaters from Variants

-- We also want to be able to get functions to update
-- our state from a Variant.
class BuildValueSetters rl rin fin rout fout | rl rin fin -> rout fout where
  buildValueSetters
    :: RLProxy rl
    -> RProxy fin
    -> (Variant rin -> Record fout -> Record fout)
    -> Variant rout
    -> Record fout
    -> Record fout

instance valueSetterNil :: BuildValueSetters Nil r fin r fout where
  buildValueSetters _ _ = id

instance valueSetterCons ::
  ( IsSymbol sym
  , RowCons sym { value :: a | r } fin fout
  , RowCons sym a rout' rout
  , BuildValueSetters tail rin fin' rout' fout
  ) => BuildValueSetters (Cons sym a tail) rin fin rout fout
  where
  buildValueSetters _ _ =
    let
      sym  = SProxy  :: SProxy sym
      tail = RLProxy :: RLProxy tail
      fin  = RProxy  :: RProxy fin'
    in
      on sym (\a -> set (prop sym <<< prop (SProxy :: SProxy "value")) a )
        <<< buildValueSetters tail fin

valueSetter
  :: ∀ rl vals rin fin rout fout
   . RowToList vals rl
  => BuildValueSetters rl rin fin rout fout
  => RProxy vals
  -> (Variant rin -> Record fout -> Record fout)
  -> Variant rout
  -> Record fout
  -> Record fout
valueSetter k =
  buildValueSetters (RLProxy :: RLProxy rl) (RProxy :: RProxy fin)


class BuildValidateSetters rl rin fin rout fout | rl rin fin -> rout fout where
  buildValidateSetters
    :: RLProxy rl
    -> RProxy fin
    -> (Variant rin -> Record fout -> Record fout)
    -> Variant rout
    -> Record fout
    -> Record fout

instance validateSetterNil :: BuildValidateSetters Nil r fin r fout where
  buildValidateSetters _ _ = id

instance validateSetterCons ::
  ( IsSymbol sym
  , RowCons sym { shouldValidate :: a | r } fin fout
  , RowCons sym a rout' rout
  , BuildValidateSetters tail rin fin' rout' fout
  ) => BuildValidateSetters (Cons sym a tail) rin fin rout fout
  where
  buildValidateSetters _ _ =
    let
      sym  = SProxy  :: SProxy sym
      tail = RLProxy :: RLProxy tail
      fin  = RProxy  :: RProxy fin'
    in
      on sym (\a -> set (prop sym <<< prop (SProxy :: SProxy "shouldValidate")) a )
        <<< buildValidateSetters tail fin

validateSetter
  :: ∀ rl vals rin fin rout fout
   . RowToList vals rl
  => BuildValidateSetters rl rin fin rout fout
  => RProxy vals
  -> (Variant rin -> Record fout -> Record fout)
  -> Variant rout
  -> Record fout
  -> Record fout
validateSetter k =
  buildValidateSetters (RLProxy :: RLProxy rl) (RProxy :: RProxy fin)
