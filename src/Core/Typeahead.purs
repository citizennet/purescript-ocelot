module Ocelot.Core.Typeahead where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store, seeks)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (logShow, CONSOLE)
import DOM (DOM)
import Data.Array (difference, filter, head, length, sort, (:))
import Data.Fuzzy (Fuzzy(..))
import Data.Fuzzy as Fuzz
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap)
import Data.Rational ((%))
import Data.StrMap (StrMap)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(Success, Failure, Loading))
import Select.Primitives.Container as C
import Select.Primitives.Search as S
import Select.Primitives.SearchContainer as SC
import Select.Primitives.State (getState)


----------
-- Component types

-- The render function is provided outside the component, so we rely
-- on the `Store` type here to make that possible.
type State o item source err eff m =
  Store
    (TypeaheadState item source err)
    (H.ParentHTML (TypeaheadQuery o item source err eff m) (ChildQuery o (Fuzzy item) eff m) ChildSlot m)

-- Items are wrapped in `SyncMethod` to account for failure and loading cases
-- in asynchronous typeaheads. Selections are wrapped in `SelectionType` to
-- account for various limits on what can be selected. The component is responsible
-- for managing its items and selections.
type TypeaheadState item source err =
  { items :: SyncMethod source err (Array item)
  , selections :: SelectionType item
  , search :: String
  , config :: Config item
  }

type TypeaheadInput o item source err eff m =
  { items :: SyncMethod source err (Array item)
  , search :: Maybe String
  , initialSelection :: SelectionType item
  , render
    :: TypeaheadState item source err
    -> H.ParentHTML (TypeaheadQuery o item source err eff m) (ChildQuery o (Fuzzy item) eff m) ChildSlot m
  , config :: Config item
  }

-- `item` is wrapped in `Fuzzy` to support highlighting in render functions
-- `HandleContainer` & `HandleSearch`: Manage routing for child messages
-- `Remove`: The user has removed a currently-selected item.
-- `Selections`: The parent wants to know the current selections.
-- `FulfillRequest`: The parent has fetched data for an async typeahead.
-- `Initialize`: Async typeaheads should fetch their data.
-- `TypeaheadReceiver`: Refresh the typeahead with new input
data TypeaheadQuery o item source err eff m a
  = HandleSearchContainer (SC.Message o (Fuzzy item)) a
  | Remove item a
  | Selections (SelectionType item -> a)
  | FulfillRequest (SyncMethod source err (Array item)) a
  | Initialize a
  | TypeaheadReceiver (TypeaheadInput o item source err eff m) a

-- The parent is notified when items are selected or removed and when a
-- new search is performed, but does not need to take action. This is just
-- for observation purposes. However, `RequestData` represents that a
-- typeahead needs data; the parent is responsible for fetching it and using
-- the `FulfillRequest` method to return the data.
data TypeaheadMessage o item source err
  = SelectionsChanged SelectionChange item (SelectionType item)
  | NewSearch String
  | RequestData (SyncMethod source err (Array item))
  | Emit (o Unit)

-- Selections change because something was added or removed.
data SelectionChange
  = ItemSelected
  | ItemRemoved


----------
-- Child types

-- The typeahead relies on the Search and Container primitives.
type ChildSlot = Unit
type ChildQuery o item eff m = SC.SearchContainerQuery o item eff m


data Slot
  = ContainerSlot
  | SearchSlot
derive instance eqPrimitiveSlot :: Eq Slot
derive instance ordPrimitiveSlot :: Ord Slot


----------
-- Data modeling

type Config item =
  { filterType :: FilterType item
  , insertable :: Insertable item
  , keepOpen   :: Boolean
  , toStrMap   :: item -> StrMap String
  }

data FilterType item
  = NoFilter
  | FuzzyMatch
  | CustomMatch (String -> item -> Boolean)


-- If an item is meant to be insertable, you must provide a function
-- describing how to move from a search string to an item. Note: this
-- only inserts the item; if you want to take more actions like add to
-- your database, make sure you provide a way to identify 'inserted' items
-- in your custom Item type.
data Insertable item
  = NotInsertable
  | Insertable (String -> item)

-- Sync represents a traditional typeahead. Provide data on input and it will
-- only refresh if you send new input.
--
-- Async behaves like Sync, but fetches its own data on initialization.
--
-- ContinuousAsync fetches data on every user search. Recommended to set a
-- reasonable debounce time on the component.
--
-- `source` is an arbitrary representation for data the parent needs to fetch.
-- Typically this will be a record the parent can use to perform a request.
data SyncMethod source err a
  = Sync a
  | Async source (RemoteData err a)
  | ContinuousAsync Milliseconds String source (RemoteData err a)
derive instance functorSyncMethod :: Functor (SyncMethod source err)


-- A convenience type for when you mount a sync typeahead, filling in the
-- source and error fields with Void on your behalf.
type TypeaheadSyncMessage o item = TypeaheadMessage o item Void Void

-- How many items it is possible to select on the typeahead. NOTE: The limit
-- case will not allow more than N items to be selected, but you can provide
-- an array with more than N items. This will disallow further selections until
-- there are less than N selections.
data SelectionType item
  = One (Maybe item)
  | Limit Int (Array item)
  | Many (Array item)
derive instance functorSelectionType :: Functor SelectionType


----------
-- Component

-- NOTE: Effects must be unified among input, component, HTML, and DSL types. Not all of
-- these will be defined in the same file. As an architectural decision, I have included
-- the effects type with the component definition. You are expected, when defining anything
-- else using the component types, to import and use this effects row. For example, if you
-- define default inputs or render functions elsewhere, make sure to import these effects.
--
-- The best practice is to avoid composition of synonyms at all costs because they make
-- it extremely difficult to tell when problems and duplication arise. Also defer applying
-- effects until the last possible moment: usually, this is in an actual function definition.
-- You will see that synonyms like `ChildQuery` do not explicitly list row effects.
--
-- So while you should import and use this effects type anywhere you write functions for
-- this component specifically, you should NOT import or use this effects type for any other
-- components or components higher up the chain. This effects type should be as minimal as
-- possible.
--
-- For example, this effects row must use `DOM` and `AVAR` because it mounts components in
-- slots that themselves use those effects. But rather than compose with their effects types,
-- they are written out and applied to this component for the first time here. The same is
-- done when _this_ component is used.
--
-- This may be overly-cautious, but given how difficult it is to track down the real source
-- of effect row unification errors, I advise following this practice for our components. This
-- is especially true because effects may compile in one file, but fail to compile once actually
-- used in context.
type Effects eff = ( dom :: DOM, avar :: AVAR, console :: CONSOLE | eff )

-- The TypeaheadQuery, TypeaheadInput, ChildQuery, and component types use the same effects,
-- so make sure to apply the Effects type to each. NOTE: Avoid prematurely applying effects
-- by applying them in synonyms. Only use them in function signatures where it is necessary.
component :: ∀ o item source err eff m
  . MonadAff (Effects eff) m
 => Eq item
 => Show err
 => H.Component
      HH.HTML
      (TypeaheadQuery o item source err (Effects eff) m)
      (TypeaheadInput o item source err (Effects eff) m)
      (TypeaheadMessage o item source err)
      m
component =
  H.lifecycleParentComponent
    { initialState
    , render: extract
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where
    initialState
      :: (TypeaheadInput o item source err (Effects eff) m)
      -> State o item source err (Effects eff) m
    initialState i = store i.render
      { items: i.items
      , selections: i.initialSelection
      , search: fromMaybe "" i.search
      , config: i.config
      }

    eval
      :: (TypeaheadQuery o item source err (Effects eff) m)
      ~> H.ParentDSL
          (State o item source err (Effects eff) m)
          (TypeaheadQuery o item source err (Effects eff) m)
          (ChildQuery o (Fuzzy item) (Effects eff) m)
          (ChildSlot)
          (TypeaheadMessage o item source err)
          m
    eval = case _ of
      HandleSearchContainer message a -> case message of
        SC.Emit query -> H.raise (Emit query) *> pure a

        SC.ContainerMessage message' -> case message' of

          -- Select an item, removing it from the list of
          -- available items in the container.
          -- Does not remove the item from the parent state.
          C.ItemSelected (Fuzzy { original: item }) -> do
            (Tuple _ st) <- getState
            let newSelections = selectItem item st.items st.selections
            H.modify $ seeks _ { selections = newSelections }
            _ <- if st.config.keepOpen
                 then pure Nothing
                 else H.query unit <<< SC.inContainer $ C.SetVisibility C.Off
            H.raise $ SelectionsChanged ItemSelected item newSelections
            eval (FulfillRequest st.items a)

          otherwise -> pure a

        SC.SearchMessage message' -> case message' of
          -- S.ContainerQuery query -> H.query unit <<< SC.inContainer query *> pure a

          -- Perform a new search, fetching data if ContinuousAsync.
          S.NewSearch text -> do
            H.modify $ seeks _ { search = text }
            H.raise $ NewSearch text

            (Tuple _ st) <- getState
            case st.items of
              ContinuousAsync db _ src _ -> do
                -- ContinuousAsync may take some time to complete. Set status
                -- to Loading and request the data. When the data is fulfilled,
                -- the status will change again.
                let cont = ContinuousAsync db text src Loading
                H.modify $ seeks $ _ { items = cont }
                H.raise $ RequestData cont
                pure a
              _ -> eval (FulfillRequest st.items a)

          otherwise -> pure a

      -- Remove a currently-selected item.
      Remove item a -> do
        (Tuple _ st) <- getState
        let selections = removeItem item st.items st.selections
        H.modify $ seeks _ { selections = selections }
        H.raise $ SelectionsChanged ItemRemoved item selections
        eval (FulfillRequest st.items a)

      -- Tell the parent what the current state of the Selections list is.
      Selections reply -> do
        (Tuple _ st) <- getState
        pure $ reply st.selections

      -- The callback: when the parent has fetched data, they'll call this to update
      -- the typeahead with that data.
      FulfillRequest items a -> do
        H.modify $ seeks $ _ { items = items }
        (Tuple _ st) <- getState
        _ <- updateContainer (getNewItems st)
        pure a

      -- If synchronous, do nothing; if not, request the data for the component.
      Initialize a -> do
        (Tuple _ st) <- getState

        case st.items of
          (Sync xs) -> eval (FulfillRequest st.items a)
          -- TODO: Move into its own query so this can be regularly triggered
          -- by the parent
          Async src _ -> do
            let async = Async src Loading
            H.modify $ seeks $ _ { items = async }
            H.raise $ RequestData async
            pure a
          ContinuousAsync db text src _ -> do
            let cont = ContinuousAsync db text src Loading
            H.modify $ seeks $ _ { items = cont }
            H.raise $ RequestData cont
            pure a

      TypeaheadReceiver input a -> do
        H.put (initialState input)
        pure a

    -- If there is new data to send, then send it;
    -- if not, empty and close the container.
    updateContainer
      :: SyncMethod source err (Array (Fuzzy item))
      -> H.ParentDSL
          (State o item source err (Effects eff) m)
          (TypeaheadQuery o item source err (Effects eff) m)
          (ChildQuery o (Fuzzy item) (Effects eff) m)
          (ChildSlot)
          (TypeaheadMessage o item source err)
          m
          Unit
    updateContainer (Sync items) = do
      _ <- H.query unit <<< SC.inContainer $ C.ReplaceItems items
      pure unit
    updateContainer (Async _ (Success items)) = do
      _ <- H.query unit <<< SC.inContainer $ C.ReplaceItems items
      pure unit
    updateContainer (Async _ (Failure e)) = do
      _ <- H.query unit <<< SC.inContainer $ C.SetVisibility C.Off
      _ <- H.query unit <<< SC.inContainer $ C.ReplaceItems []
      H.liftAff $ logShow e
      pure unit
    updateContainer (ContinuousAsync _ _ _ (Success items)) = do
      _ <- H.query unit <<< SC.inContainer $ C.ReplaceItems items
      pure unit
    updateContainer (ContinuousAsync _ _ _ (Failure e)) = do
      _ <- H.query unit <<< SC.inContainer $ C.SetVisibility C.Off
      _ <- H.query unit <<< SC.inContainer $ C.ReplaceItems []
      H.liftAff $ logShow e
      pure unit
    updateContainer _ = pure unit



----------
-- Internal helpers

-- Filter items dependent on the filterable configuration.
-- NOTE: Min Ratio threshold is hardcoded for now.
applyFilter :: ∀ item. FilterType item -> String -> Array (Fuzzy item) -> Array (Fuzzy item)
applyFilter filterType text items = case filterType of
  NoFilter -> items
  CustomMatch match -> filter (\item -> match text $ (_.original <<< unwrap) item) items
  FuzzyMatch -> filter (\(Fuzzy { ratio }) -> ratio > (2 % 3)) items

-- Update items dependent on the insertable configuration. Only provide insertable if there is an exact match.
-- Fuzzy is always run over the container items so this match is available.
applyInsertable :: ∀ item. (item -> Fuzzy item) -> Insertable item -> String -> Array (Fuzzy item) -> Array (Fuzzy item)
applyInsertable match insertable text items = case insertable of
  NotInsertable -> items
  Insertable mkItem | length (filter isExactMatch items) > 0 -> items
                    | otherwise -> (match $ mkItem text) : items
    where
      isExactMatch (Fuzzy { distance }) = distance == Fuzz.Distance 0 0 0 0 0 0

-- Remove an item from the selections and place it in the items list.
removeItem :: ∀ item source err
  . Eq item
 => item
 -> SyncMethod source err (Array item)
 -> SelectionType item
 -> SelectionType item
removeItem item items selections = case selections of
  One  _  -> One Nothing
  Limit n xs  -> Limit n $ filter ((/=) item) xs
  Many xs -> Many $ filter ((/=) item) xs

-- Remove an item from the items and place it in the selections list.
selectItem :: ∀ item source err
  . Eq item
 => item
 -> SyncMethod source err (Array item)
 -> SelectionType item
 -> SelectionType item
selectItem item items selections = case selections of
  One _ -> One $ Just item
  Many xs -> Many $ item : xs
  Limit n xs -> if length xs >= n then selections else Limit n $ item : xs

--  Construct new array of fuzzy items to send to the container by diffing the
--  original items & current selections
removeSelections :: ∀ item source err
  . Eq item
 => SyncMethod source err (Array item)
 -> SelectionType item
 -> SyncMethod source err (Array item)
removeSelections items selections = case items of
  (Sync _) -> Sync getDiff
  (Async src d) -> Async src (const getDiff <$> d)
  (ContinuousAsync ms sch src d) -> ContinuousAsync ms sch src (const getDiff <$> d)
  where
    getDiff = difference (fromMaybe [] $ maybeUnpackItems items) (unpackSelections selections)

-- Attempt to match new items against the user's search.
getNewItems :: ∀ item source err. Eq item => TypeaheadState item source err -> SyncMethod source err (Array (Fuzzy item))
getNewItems st = (sort <<< applyF <<< applyI) <$> (fuzzyItems <<< removeSelections st.items) st.selections
  where
    matcher :: item -> Fuzzy item
    matcher = Fuzz.match true st.config.toStrMap st.search

    fuzzyItems :: SyncMethod source err (Array item) -> SyncMethod source err (Array (Fuzzy item))
    fuzzyItems = (map <<< map) matcher

    applyI :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyI = applyInsertable matcher st.config.insertable st.search

    applyF :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyF = applyFilter st.config.filterType st.search


----------
-- External Helpers

unpackSelection :: ∀ item. SelectionType item -> Maybe item
unpackSelection (One x) = x
unpackSelection (Limit _ xs) = head xs
unpackSelection (Many xs) = head xs

unpackSelections :: ∀ item. SelectionType item -> Array item
unpackSelections (One Nothing) = []
unpackSelections (One (Just i)) = [i]
unpackSelections (Limit _ xs) = xs
unpackSelections (Many xs) = xs

maybeUnpackItems :: ∀ source err item. SyncMethod source err (Array item) -> Maybe (Array item)
maybeUnpackItems (Sync xs) = Just xs
maybeUnpackItems (Async _ (Success xs)) = Just xs
maybeUnpackItems (ContinuousAsync _ _ _ (Success xs)) = Just xs
maybeUnpackItems _ = Nothing

maybeReplaceItems :: ∀ source err item
  . RemoteData err (Array item)
 -> SyncMethod source err (Array item)
 -> Maybe (SyncMethod source err (Array item))
maybeReplaceItems _ (Sync _) = Nothing
maybeReplaceItems xs (Async src _) = Just $ Async src xs
maybeReplaceItems xs (ContinuousAsync time search src _)
  = Just $ ContinuousAsync time search src xs
