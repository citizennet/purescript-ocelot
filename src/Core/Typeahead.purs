module CN.UI.Core.Typeahead where

import Prelude

import Network.RemoteData (RemoteData(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Network.HTTP.Affjax (AJAX)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Array (filter, (:), length)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.String (Pattern(..), contains, toLower)
import Data.Time.Duration (Milliseconds)
import Control.Comonad (extract)
import Control.Comonad.Store (Store, store, seeks)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Component.ChildPath as CP
import Select.Effects (FX)
import Select.Primitives.Container as Container
import Select.Primitives.Search as Search
import Select.Primitives.State (getState)

----------
-- Motivation

--  This module provides a pre-built typeahead component. It hides the necessary wiring of the Container and Search
--  primitives, and can be used 'out of the box' with a default configuration record. However, it is entirely up
--  to you whether you want to use that record.
--
--  You can also provide handler functions to insert into 'eval' for each important case: a new search from the
--  search field or a new item selection from the container. As always, you are responsible for providing the
--  render functions for each relevant primitive.
--
--  Your handler functions should provide your state management (they should decide how items can be selected, etc.),
--  and this hides the state from the parent unless they explicitly ask for it with `GetSelections`.
--
--  In use: provide render functions, provide configuration or handlers, provide items, and you're good to go.


----------
-- Item types

-- Items can nearly be anything you like, except that they must have an `Eq` instance (so they can be
-- filtered) and must have some way to compare to a string search. This may not correspond with the
-- default Show instance for the type, so I've provided a type class. It's likely this would be better
-- served in a Primitive module. Strings already have an instance of the class and require no further
-- effort.

class StringComparable a where
  toString :: a -> String

instance stringComparableString :: StringComparable String where
  toString s = s

----------
-- Component Types

-- State type is necessary so render function can be provided.
type State o item e = Store (TypeaheadState o item e) (TypeaheadHTML o item e)

-- Newtype because of the self-reference in config function.
newtype TypeaheadState o item e = State
  { items :: SyncMethod TypeaheadError (Array item)
  , selections :: SelectionType item
  , debounceTime :: Milliseconds
  , search :: String
  , config :: EvalConfig o item e
  }

type TypeaheadError = String

-- Indicates how the typeahead should fetch data. Async types store both their function
-- to fetch data and the result of running that function.
--
-- - `Sync`: The typeahead expects data provided by the parent.
-- - `Async`: The typeahead will fetch its own data on initialization (but not after)
-- - `ContinuousAsync`: The typeahead will fetch data every time a search is performed
data SyncMethod err a
  = Sync a
  | Async (forall e. Aff (ajax :: AJAX | e) (RemoteData err a)) (RemoteData err a)
  | ContinuousAsync (forall e. String -> Aff (ajax :: AJAX | e) (RemoteData err a)) (RemoteData err a)

-- Cannot make / derive a functor instance. See:
-- https://github.com/purescript/purescript/issues/3232
mapSyncMethod :: forall err a. (a -> a) -> SyncMethod err a -> SyncMethod err a
mapSyncMethod f (Sync x) = Sync $ f x
mapSyncMethod f (Async fetch x) = Async fetch $ f <$> x
mapSyncMethod f (ContinuousAsync fetch x) = ContinuousAsync fetch $ f <$> x

-- Could also provide 'Limit Int' for restricted lists
data SelectionType item
  = One (Maybe item)
  | Many (Array item)

derive instance functorSelectionType :: Functor SelectionType

data TypeaheadQuery o item e a
  = HandleContainer (Container.Message o item) a
  | HandleSearch (Search.Message o item) a
  | Remove item a
  | Selections (SelectionType item -> a)
  | Initialize a
  | TypeaheadReceiver (TypeaheadInput o item e) a

type TypeaheadInput o item e =
  { items :: SyncMethod TypeaheadError (Array item)
  , debounceTime :: Milliseconds
  , search :: Maybe String
  , initialSelection :: SelectionType item
  , render :: TypeaheadState o item e -> TypeaheadHTML o item e
  , config :: EvalConfig o item e
  }

data TypeaheadMessage o item
  = ItemSelected item
  | ItemRemoved item
  | NewSearch String
  | Emit (o Unit)

-- The idea: maintain an Either where you either provide
-- a configuration record, relying on default functionality
-- provided by the component, OR you can provide handlers
-- for the two important child messages (new search or
-- item selected)

type EvalConfig o item e =
  Either (HandlerRecord o item e) (ConfigRecord item)

-- A default config can help minimize work.
defaultConfig :: ∀ item. Eq item => StringComparable item => ConfigRecord item
defaultConfig =
  { filterType: CaseInsensitive
  , insertable: NotInsertable
  , keepOpen: true
  }

-- Some standard functionality is baked in to the component
-- and can be configured if the user wants a more 'out of the
-- box' experience.
type ConfigRecord item =
  { filterType  :: FilterType item
  , insertable  :: Insertable item
  , keepOpen    :: Boolean
  }

data FilterType item
  = NoFilter
  | Exact
  | CaseInsensitive
  | CustomMatch (String -> item -> Boolean)

-- The typeahead can either not insert un-matched values, or it can insert them so long
-- as there is a way to go from a string to your custom item type. If your data is just
-- a string, use `id`. If you want to tag inserted items so you can send them to the
-- DB on selection, make sure your Insertable function encodes this information.
data Insertable item
  = NotInsertable
  | Insertable (String -> item)

-- If you need something more custom, you can provide handlers for the most important
-- eval functions yourself. If you only need one, import the relevant function from
-- this module to the right field
-- (ex: import Typeahead as Typeahead; itemSelected: Typeahead.itemSelected)
type HandlerRecord o item e =
  { newSearch :: String -> TypeaheadDSL o item e Unit
  , itemSelected :: item -> TypeaheadDSL o item e Unit
  , itemRemoved :: item -> TypeaheadDSL o item e Unit
  }


----------
-- Convenience component types

type TypeaheadComponent o item e =
  H.Component
    HH.HTML
    (TypeaheadQuery o item e)
    (TypeaheadInput o item e)
    (TypeaheadMessage o item)
    (FX e)

type TypeaheadHTML o item e =
  H.ParentHTML
    (TypeaheadQuery o item e)
    (ChildQuery o item e)
    ChildSlot
    (FX e)

type TypeaheadDSL o item e =
  H.ParentDSL
    (State o item e)
    (TypeaheadQuery o item e)
    (ChildQuery o item e)
    ChildSlot
    (TypeaheadMessage o item)
    (FX e)


----------
-- Child types

type ContainerQuery o item =
  Container.ContainerQuery o item

type SearchQuery o item e =
  Search.SearchQuery o item e

type ChildQuery o item e =
  Coproduct2 (ContainerQuery o item) (SearchQuery o item e)

type ChildSlot =
  Either2 Slot Slot

data Slot = Slot PrimitiveSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data PrimitiveSlot
  = ContainerSlot
  | SearchSlot
derive instance eqPrimitiveSlot :: Eq PrimitiveSlot
derive instance ordPrimitiveSlot :: Ord PrimitiveSlot


----------
-- Component definition

-- You are expected to provide items in some Foldable / Functor instance
-- so our default functions can operate on them. The most common are likely to
-- be Array or Maybe.

component :: ∀ o item e
  . Eq item => StringComparable item => TypeaheadComponent o item e
component =
  H.lifecycleParentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input TypeaheadReceiver
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
    initialState :: TypeaheadInput o item e -> State o item e
    initialState i = store i.render $ State
        { items: i.items
        , selections: i.initialSelection
        , debounceTime: i.debounceTime
        , search: fromMaybe "" i.search
        , config: i.config
        }

    eval :: TypeaheadQuery o item e ~> TypeaheadDSL o item e
    eval = case _ of
      -- Handle messages from the container.
      HandleContainer message a -> case message of

        -- Evaluate an embedded parent query
        Container.Emit query -> H.raise (Emit query) *> pure a

        -- Handle a new item selection
        Container.ItemSelected item -> a <$ do
          (Tuple _ (State st)) <- getState
          case st.config of
             Left { itemSelected } -> itemSelected item
             Right config -> evalItemSelected config item

      -- Handle messages from the search.
      HandleSearch message a -> case message of

        -- Evaluate an embedded parent query
        Search.Emit query -> H.raise (Emit query) *> pure a

        -- Route a container query to its correct slot.
        Search.ContainerQuery query -> do
          _ <- H.query' CP.cp1 (Slot ContainerSlot) query
          pure a

        -- Handle a new search
        Search.NewSearch text -> a <$ do
          (Tuple _ (State st)) <- getState
          case st.config of
             Left { newSearch } -> newSearch text
             Right config -> evalNewSearch config text

      -- Handle a 'remove' event on the selections list.
      Remove item a -> a <$ do
        (Tuple _ (State st)) <- getState
        case st.config of
             Left { itemRemoved } -> itemRemoved item
             Right _ -> evalItemRemoved item

      -- Return the current selections to the parent.
      Selections reply -> do
        (Tuple _ (State st)) <- getState
        pure $ reply st.selections

      -- Specialty function specifically for Async type; fetches
      -- data using provided function.
      Initialize a -> do
        (Tuple _ (State st)) <- getState

        case st.items of
          Async fetch _ -> do
            items <- H.liftAff fetch
            H.modify
              $ seeks
              $ \(State st') -> State
              $ st' { items = Async fetch items }
            pure a

            -- In any other case, do not modify item data.
          _ -> pure a


      -- Overwrite the state with new input, handling the case in which the typeahead fetches its
      -- own data to avoid unnecessary requests.
      TypeaheadReceiver input a -> a <$ do
        H.put $ initialState input
        -- In the case of Async this will fetch the items again. We can also modify this to _not_
        -- overwrite the items field if the config type is Async or otherwise optimize performance.
        eval (Initialize a)



----------
-- Helper functions

-- Intended so the end user can mount these slots in their Typeahead render function wherever they would like
-- without managing the wiring. Allows them to maintain full control over rendering without having to manage
-- handlers.

searchSlot :: ∀ o item e. Search.SearchInput o item e -> TypeaheadHTML o item e
searchSlot i =
  HH.slot' CP.cp2 (Slot SearchSlot) Search.component i (HE.input HandleSearch)

containerSlot :: ∀ o item e. Container.ContainerInput o item -> TypeaheadHTML o item e
containerSlot i =
  HH.slot' CP.cp1 (Slot ContainerSlot) Container.component i (HE.input HandleContainer)


-- Given an item, the items, and the selections, move an item from the items list to
-- the selections list.
selectItem :: ∀ item
  . Eq item
 => StringComparable item
 => item
 -> SyncMethod TypeaheadError (Array item)
 -> SelectionType item
 -> Tuple (SyncMethod TypeaheadError (Array item)) (SelectionType item)
selectItem item items selections = case selections of
  One Nothing  -> Tuple (remove item items) (One $ Just item)
  One (Just i) -> Tuple (insert i $ remove item items) (One $ Just item)
  Many xs      -> Tuple (remove item items) (Many $ item : xs)
    where
      insert i is = mapSyncMethod (\i' -> i : i') is
      remove i is = mapSyncMethod (filter ((/=) i)) is

-- Given an item, the items, and the selections, move an item from the selections list to
-- the items list.
removeItem :: ∀ item
  . Eq item
 => StringComparable item
 => item
 -> SyncMethod TypeaheadError (Array item)
 -> SelectionType item
 -> Tuple (SyncMethod TypeaheadError (Array item)) (SelectionType item)
removeItem item items selections = case selections of
  One  _  -> Tuple (insert item items) (One Nothing)
  Many xs -> Tuple (insert item items) (Many $ filter ((/=) item) xs)
    where
      insert i is = mapSyncMethod (\i' -> i : i') is



----------
-- Helper eval functions

-- These functions feed from the configuration options and allow for a variety of
-- behaviors out of the box. However, if you need more fine-grained control over
-- state and behaviors, you can provide custom handlers.

-- Use a default matching function to determine what items match the user's search,
-- or use one provided by the user. If searches are insertable, allow the user to select
-- items they've searched, constructing a new item from the string using the user's function.
-- Finally, update the new items available in the container.

evalNewSearch :: ∀ o item e
  . Eq item
 => StringComparable item
 => ConfigRecord item
 -> String
 -> TypeaheadDSL o item e Unit
evalNewSearch { filterType, insertable } text = unit <$ do
  -- Update the search value so it can replace the text field
  H.modify $ seeks \(State st') -> State $ st' { search = text }

  -- Fetch the state in order to get current items
  (Tuple _ (State st)) <- getState

  -- In non-continuous, use existing items from the state.
  -- In continuous, use the provided function to fetch the items.
  -- WARN: ContinuousAsync assumes you just want the items returned -- it doesn't filter.
  -- This behavior can be changed to reflect usual filtering if needed; there exists
  -- a `NoFilter` option that will achieve the same behavior.
  fetchItemsResult <- case st.items of
    Sync items -> pure $ Sync $ (applyInsertable <<< applyFilter) items
    Async fetch items -> pure $ Async fetch $ (applyInsertable <<< applyFilter) <$> items
    ContinuousAsync fetch _ -> do
       -- Update items on state to loading
       H.modify
         $ seeks
         $ \(State st') -> State
         $ st' { items = ContinuousAsync fetch Loading }
       -- Fetch items
       items <- H.liftAff (fetch text)
       -- Update items on state with new results
       H.modify
         $ seeks
         $ \(State st') -> State
         $ st' { items = ContinuousAsync fetch items }
       -- Return filtered items wrapped in the correct type
       pure $ ContinuousAsync fetch $ (applyInsertable <<< applyFilter) <$> items

  _ <- case fetchItemsResult of
    -- If sync, send new items to the container.
    Sync items -> do
      _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems items
      pure unit

    -- If async, handle remote data cases.
    Async _ items -> updateContainer items
    ContinuousAsync _ items -> updateContainer items

  H.raise $ NewSearch text

  where
    -- Failure: Close and clear the container.
    updateContainer (Failure e) = do
      -- Close the container
      _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.Visibility Container.Off
      -- Clear the container
      _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems []
      H.liftAff $ log e
    -- Success: Update the items in the container.
    updateContainer (Success items) = do
      _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems items
      pure unit
    -- NotAsked or Loading: No updates for the container. Only
    -- possible in the ContinuousAsync case. Do nothing.
    updateContainer _ = pure unit

    -- Set up the filter
    applyFilter items =
      case filterType of
        NoFilter -> items
        Exact -> filter (\item -> contains (Pattern text) (toString item)) items
        CaseInsensitive ->
          filter (\item -> contains (Pattern $ toLower text) (toLower $ toString item)) items
        CustomMatch match -> filter (\item -> match text item) items

    -- If the typeahead is Insertable, then use the provided function in that type
    -- to construct a new item and add it to the item list. NOTE: If you want to be able
    -- to tell what items have been inserted, ensure your item type encodes this! Your
    -- mkItem function can tag items that were inserted.
    applyInsertable items = case insertable of
      NotInsertable -> items
      Insertable mkItem | length items > 0 -> items
                        | otherwise -> (mkItem text) : items


-- Standard selection: Take the item out of the available items list and put it in the
-- selected list. Reset the search, and close the container unless it's set to stay open.

-- WARN: This doesn't tell you if the new item selected was an inserted item that needs
-- to be written anywhere else! If you need this functionality, your item type must encode
-- it, like: (MyItem IsNew item) & mkItem item = MyItem IsNew item
evalItemSelected :: ∀ o item e
  . StringComparable item
 => Eq item
 => ConfigRecord item
 -> item
 -> TypeaheadDSL o item e Unit
evalItemSelected { keepOpen } item = unit <$ do
  (Tuple _ (State st)) <- getState
  let (Tuple newItems newSelections) = selectItem item st.items st.selections

  -- Send the new items to the container
  case newItems of
    Sync i -> updateContainer (Success i)
    Async _ i -> updateContainer i
    ContinuousAsync _ i -> updateContainer i

  H.modify $ seeks \(State st') -> State st'
    { selections = newSelections
    , items = newItems }

  -- Send an empty string to the search
  _ <- H.query' CP.cp2 (Slot SearchSlot)
        $ H.action
        $ Search.TextInput ""

  -- Unless you're meant to stay open on selection, close the menu.
  _ <- if keepOpen then pure Nothing else do
       H.query' CP.cp1 (Slot ContainerSlot)
         $ H.action
         $ Container.Visibility Container.Off

  H.raise $ ItemSelected item

  where
    -- Success: Update the items in the container.
    updateContainer (Success items) = do
      _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems items
      pure unit
    -- Failure, NotAsked, or Loading: No updates for the container.
    -- Shouldn't even be possible; no items will be rendered unless
    -- in a Success state.
    updateContainer _ = pure unit


-- Standard removal: When an item is removed, put it back into the available items list
-- and take it out of the selected list.
evalItemRemoved :: ∀ o item e
  . StringComparable item
 => Eq item
 => item
 -> TypeaheadDSL o item e Unit
evalItemRemoved item = unit <$ do
  (Tuple _ (State st)) <- getState
  let (Tuple newItems newSelections) = removeItem item st.items st.selections

  H.modify $ seeks \(State st') -> State $ st'
    { selections = newSelections
    , items = newItems }

  case newItems of
    Sync i -> updateContainer (Success i)
    Async _ i -> updateContainer i
    ContinuousAsync _ i -> updateContainer i

  H.raise $ ItemRemoved item

  where
    -- Success: Update the items in the container.
    updateContainer (Success items) = do
      _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems items
      pure unit
    -- Failure, NotAsked, or Loading: No updates for the container.
    -- Shouldn't even be possible; no items will be rendered unless
    -- in a Success state.
    updateContainer _ = pure unit
