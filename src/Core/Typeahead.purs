module Ocelot.Core.Typeahead where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store, seeks)
import Control.Monad.Aff.AVar (AVAR)
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Aff (Aff)
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
import Network.RemoteData (RemoteData(..))
import Select as Select
import Select.Internal.State (getState, updateStore)

----------
-- Component types

-- The render function is provided outside the component, so we rely
-- on the `Store` type here to make that possible.
type StateStore o item err eff m =
  Store
    (State item err eff)
    (H.ParentHTML (Query o item err eff m) (ChildQuery o (Fuzzy item) eff) ChildSlot m)

-- Items are wrapped in `SyncMethod` to account for failure and loading cases
-- in asynchronous typeaheads. Selections are wrapped in `SelectionType` to
-- account for various limits on what can be selected. The component is responsible
-- for managing its items and selections.
type State item err eff =
  { items :: RemoteData err (Array item)
  , selections :: SelectionType item
  , search :: String
  , config :: Config item err eff
  }

type Input o item err eff m =
  { items :: RemoteData err (Array item)
  , search :: Maybe String
  , initialSelection :: SelectionType item
  , render
      :: State item err eff
      -> H.ParentHTML (Query o item err eff m) (ChildQuery o (Fuzzy item) eff) ChildSlot m
  , config :: Config item err eff
  }

-- `item` is wrapped in `Fuzzy` to support highlighting in render functions
-- `HandleContainer` & `HandleSearch`: Manage routing for child messages
-- `Remove`: The user has removed a currently-selected item.
-- `Selections`: The parent wants to know the current selections.
-- `FulfillRequest`: The parent has fetched data for an async typeahead.
-- `Initialize`: Async typeaheads should fetch their data.
-- `TypeaheadReceiver`: Refresh the typeahead with new input
data Query o item err eff m a
  = Remove item a
  | Synchronize a
  | HandleSelect (Select.Message o (Fuzzy item)) a
  | GetSelections (SelectionType item -> a)
  | ReplaceSelections (SelectionType item) a
  | ReplaceItems (RemoteData err (Array item)) a
  | Reset a
  | Receive (Input o item err eff m) a

-- The parent is notified when items are selected or removed and when a
-- new search is performed, but does not need to take action. This is just
-- for observation purposes. However, `RequestData` represents that a
-- typeahead needs data; the parent is responsible for fetching it and using
-- the `FulfillRequest` method to return the data.
data Message o item
  = Searched String
  | SelectionsChanged SelectionChange item (SelectionType item)
  | VisibilityChanged Select.Visibility
  | Emit (o Unit)

-- Selections change because something was added or removed.
data SelectionChange
  = ItemSelected
  | ItemRemoved

----------
-- Child types

-- The typeahead relies on the Search and Container primitives.
type ChildSlot = Unit
type ChildQuery o item eff = Select.Query o item eff

----------
-- Data modeling

type Config item err eff =
  { filterType :: FilterType item
  , insertable :: Insertable item
  , keepOpen   :: Boolean
  , syncMethod :: SyncMethod item err eff
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
-- ContinuousAsync fetches data on every user search. Recommended to set a
-- reasonable debounce time on the component.
--
-- `source` is an arbitrary representation for data the parent needs to fetch.
-- Typically this will be a record the parent can use to perform a request.
data SyncMethod item err eff
  = Sync
  | Async (AsyncConfig item err eff)

type AsyncConfig item err eff =
  { debounceTime :: Milliseconds
  , fetchItems   :: String -> Aff eff (RemoteData err (Array item))
  }

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
type Effects eff = ( ajax :: AJAX, dom :: DOM, avar :: AVAR, console :: CONSOLE | eff )

-- The Query, Input, ChildQuery, and component types use the same effects,
-- so make sure to apply the Effects type to each. NOTE: Avoid prematurely applying effects
-- by applying them in synonyms. Only use them in function signatures where it is necessary.
component :: ∀ o item err eff m
  . MonadAff (Effects eff) m
  => Eq item
  => Show err
  => H.Component
      HH.HTML
      (Query o item err (Effects eff) m)
      (Input o item err (Effects eff) m)
      (Message o item)
      m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: const Nothing
    }
  where
    initialState
      :: Input o item err (Effects eff) m
      -> StateStore o item err (Effects eff) m
    initialState i = store i.render
      { items: i.items
      , selections: i.initialSelection
      , search: fromMaybe "" i.search
      , config: i.config
      }

    eval
      :: (Query o item err (Effects eff) m)
      ~> H.ParentDSL
          (StateStore o item err (Effects eff) m)
          (Query o item err (Effects eff) m)
          (ChildQuery o (Fuzzy item) (Effects eff))
          (ChildSlot)
          (Message o item)
          m
    eval = case _ of
      HandleSelect message a -> case message of
        Select.Emit query -> H.raise (Emit query) *> pure a

        Select.Selected (Fuzzy { original: item }) -> do
          (Tuple _ st) <- getState

          let selections = case st.selections of
                One     _  -> One  $ Just item
                Many    xs -> Many $ item : xs
                Limit n xs -> if length xs >= n then st.selections else Limit n $ item : xs

          H.modify $ seeks _ { selections = selections }
          _ <- if st.config.keepOpen
               then pure Nothing
               else H.query unit $ H.action $ Select.SetVisibility Select.Off

          H.raise $ SelectionsChanged ItemSelected item selections
          eval $ Synchronize a

        -- Perform a new search, fetching data if Async.
        Select.Searched text -> do
          (Tuple _ st) <- getState
          H.modify $ seeks _ { search = text }
          H.raise $ Searched text

          case st.config.syncMethod of
            Sync -> pure unit
            Async { fetchItems } -> do
              H.modify $ seeks $ _ { items = Loading }
              newItems <- H.liftAff $ fetchItems text
              H.modify $ seeks $ _ { items = newItems }

          eval $ Synchronize a

        Select.VisibilityChanged visibility -> do
          H.raise $ VisibilityChanged visibility
          pure a

      -- Remove a currently-selected item.
      Remove item a -> do
        (Tuple _ st) <- getState

        let selections = case st.selections of
              One     _  -> One Nothing
              Limit n xs -> Limit n $ filter ((/=) item) xs
              Many    xs -> Many    $ filter ((/=) item) xs

        H.modify $ seeks _ { selections = selections }
        H.raise $ SelectionsChanged ItemRemoved item selections
        eval $ Synchronize a

      -- Tell the parent what the current state of the Selections list is.
      GetSelections reply -> do
        (Tuple _ st) <- getState
        pure $ reply st.selections

      -- Update the state of Select to be in sync.
      Synchronize a -> do
        (Tuple _ st) <- getState

        _ <- case getNewItems st of
          Success items -> do
            H.query unit $ H.action $ Select.ReplaceItems items
          Failure err -> do
            H.liftAff $ logShow err
            _ <- H.query unit $ H.action $ Select.SetVisibility Select.Off
            H.query unit $ H.action $ Select.ReplaceItems []
          _ -> pure (pure unit)

        pure a

      ReplaceItems items a -> do
        H.modify $ seeks $ _ { items = items }
        eval $ Synchronize a

      ReplaceSelections selections a -> do
        H.modify $ seeks _ { selections = selections }
        eval $ Synchronize a

      Reset a -> do
        (Tuple _ st) <- getState

        let selections = case st.selections of
              One _ -> One Nothing
              Limit n _ -> Limit n []
              Many _ -> Many []
        H.modify $ seeks _ { selections = selections, items = NotAsked }
        eval $ Synchronize a

      Receive input a -> do
        H.modify $ updateStore input.render id
        pure a


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

-- Attempt to match new items against the user's search.
getNewItems :: ∀ item err eff. Eq item => State item err eff -> RemoteData err (Array (Fuzzy item))
getNewItems st = sort <<< applyF <<< applyI <<< fuzzyItems <$> removeSelections st.items st.selections
  where
    removeSelections :: RemoteData err (Array item) -> SelectionType item -> RemoteData err (Array item)
    removeSelections items selections= (\i -> difference i $ unpackSelections selections) <$> items

    matcher :: item -> Fuzzy item
    matcher = Fuzz.match true st.config.toStrMap st.search

    fuzzyItems :: Array item -> Array (Fuzzy item)
    fuzzyItems = map matcher

    applyI :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyI = applyInsertable matcher st.config.insertable st.search

    applyF :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyF = applyFilter st.config.filterType st.search


----------
-- External Helpers

unpackSelection :: ∀ item. SelectionType item -> Maybe item
unpackSelection (One x)      = x
unpackSelection (Limit _ xs) = head xs
unpackSelection (Many xs)    = head xs

unpackSelections :: ∀ item. SelectionType item -> Array item
unpackSelections (One Nothing)  = []
unpackSelections (One (Just i)) = [i]
unpackSelections (Limit _ xs)   = xs
unpackSelections (Many xs)      = xs
