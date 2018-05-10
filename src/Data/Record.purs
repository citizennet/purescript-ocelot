module Ocelot.Data.Record where

import Prelude

import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Record (delete, get, insert)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, inj, on)
import Ocelot.Data.Default (class Default, def)
import Type.Row
  ( class ListToRow
  , class RowLacks
  , class RowToList
  , Cons
  , Nil
  , RLProxy(..)
  , RProxy(..)
  , kind RowList
  )

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

-- We can generate default records with just their raw values
class DefaultRecord (rl :: RowList) (r :: # Type) (o :: # Type) | rl -> o where
  defaultRecord :: RLProxy rl -> RProxy r -> Record o

instance nilDefaultRecord :: DefaultRecord Nil r () where
  defaultRecord _ _ = {}

instance consDefaultRecord
  :: ( IsSymbol name
     , Default a
     , RowCons name a tail' o
     , RowLacks name tail'
     , DefaultRecord tail r0 tail'
     )
  => DefaultRecord (Cons name a tail) r0 o
  where
    defaultRecord _ r =
      let tail' = defaultRecord (RLProxy :: RLProxy tail) (RProxy :: RProxy r0)
          _name = SProxy :: SProxy name
       in insert _name def tail'

makeDefaultRecord
  :: ∀ r rl o
   . DefaultRecord rl r o
  => RowToList r rl
  => RProxy r
  -> Record o
makeDefaultRecord r = defaultRecord (RLProxy :: RLProxy rl) r


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
       in insert _name { input: def
                       , validate: false
                       , result: Nothing
                       , setInput: inj _name
                       , setValidate: inj _name
                       } tail'

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

----------
-- From @liamgoodacre

-- Apply a record of functions to a record of values to
-- result in a record of outputs

class ApplyRecord
  (io :: # Type)
  (i :: # Type)
  (o :: # Type)
  | io -> i o
  , i -> io o
  , o -> io i where
    applyRecord ::
      Record io ->
      Record i ->
      Record o

instance applyRecordImpl ::
  ( RowToList io lio
  , RowToList i li
  , RowToList o lo
  , ApplyRowList lio li lo io i o
  , ListToRow lio io
  , ListToRow li i
  , ListToRow lo o ) =>
  ApplyRecord io i o where
    applyRecord io i =
      applyRowList
        (RLProxy :: RLProxy lio)
        (RLProxy :: RLProxy li)
        (RLProxy :: RLProxy lo)
        io
        i

class
  ( ListToRow io ior
  , ListToRow i ir
  , ListToRow o or ) <=
  ApplyRowList
    (io :: RowList)
    (i :: RowList)
    (o :: RowList)
    (ior :: # Type)
    (ir :: # Type)
    (or :: # Type)
    | io -> i o ior ir or
    , i -> io o ior ir or
    , o -> io i ior ir or where
      applyRowList ::
        RLProxy io ->
        RLProxy i ->
        RLProxy o ->
        Record ior ->
        Record ir ->
        Record or

instance applyRowListNil ::
  ApplyRowList Nil Nil Nil () () () where
    applyRowList a b c d e = e

rltail :: forall k v t. RLProxy (Cons k v t) -> RLProxy t
rltail _ = RLProxy

instance applyRowListCons ::
  ( RowCons k (i -> o) tior ior
  , RowCons k i tir ir
  , RowCons k o tor or
  , RowLacks k tior
  , RowLacks k tir
  , RowLacks k tor
  , ListToRow tio tior
  , ListToRow ti tir
  , ListToRow to tor
  , ApplyRowList tio ti to tior tir tor
  , IsSymbol k ) =>
  ApplyRowList
    (Cons k (i -> o) tio)
    (Cons k i ti)
    (Cons k o to)
    ior
    ir
    or
  where
    applyRowList io i o ior ir =
      let key = SProxy :: SProxy k
          f = get key ior
          x = get key ir
          tior = delete key ior :: Record tior
          tir = delete key ir :: Record tir
          tor = applyRowList (rltail io) (rltail i) (rltail o) tior tir
       in insert key (f x) tor
