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
     , RowCons name { input :: a
                    , validate :: Boolean
                    , result :: Maybe (Either e b)
                    , setInput :: a -> (Variant r0)
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
       in insert _name { input: def, validate: false, result: Nothing, setInput: inj _name, setValidate: inj _name } tail'

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
class BuildInputSetters rl rin fin rout fout | rl rin fin -> rout fout where
  buildInputSetters
    :: RLProxy rl
    -> RProxy fin
    -> (Variant rin -> Record fout -> Record fout)
    -> Variant rout
    -> Record fout
    -> Record fout

instance inputSetterNil :: BuildInputSetters Nil r fin r fout where
  buildInputSetters _ _ = id

instance inputSetterCons ::
  ( IsSymbol sym
  , RowCons sym { input :: a | r } fin fout
  , RowCons sym a rout' rout
  , BuildInputSetters tail rin fin' rout' fout
  ) => BuildInputSetters (Cons sym a tail) rin fin rout fout
  where
  buildInputSetters _ _ =
    let
      sym  = SProxy  :: SProxy sym
      tail = RLProxy :: RLProxy tail
      fin  = RProxy  :: RProxy fin'
    in
      on sym (\a -> set (prop sym <<< prop (SProxy :: SProxy "input")) a )
        <<< buildInputSetters tail fin

inputSetter
  :: ∀ rl vals rin fin rout fout
   . RowToList vals rl
  => BuildInputSetters rl rin fin rout fout
  => RProxy vals
  -> (Variant rin -> Record fout -> Record fout)
  -> Variant rout
  -> Record fout
  -> Record fout
inputSetter k =
  buildInputSetters (RLProxy :: RLProxy rl) (RProxy :: RProxy fin)


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
  , RowCons sym { validate :: a | r } fin fout
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
      on sym (\a -> set (prop sym <<< prop (SProxy :: SProxy "validate")) a )
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
