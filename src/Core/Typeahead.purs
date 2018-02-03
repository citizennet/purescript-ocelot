module CN.UI.Core.Typeahead where

import Prelude

import Network.RemoteData (RemoteData(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (logShow)

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

import Select.Effects (Effects)
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
type State o source err item e = Store (TypeaheadState o source err item) (TypeaheadHTML o source err item e)

-- Newtype because of the self-reference in config function.
newtype TypeaheadState o source err item = State
  { items :: SyncMethod source err (Array item)
  , selections :: SelectionType item
  , debounceTime :: Milliseconds
  , search :: String
  , config :: ConfigRecord item
  }

-- Indicates how the typeahead should fetch data. Async types store both their function
-- to fetch data and the result of running that function.
--
-- - `Sync`: The typeahead expects data provided by the parent.
-- - `Async`: The typeahead will fetch its own data on initialization (but not after)
-- - `ContinuousAsync`: The typeahead will fetch data every time a search is performed
--
-- The `source` is a way to use message passing to tell the parent what data you need to fetch.
-- They are responsible for properly interpreting the request, performing it, and then using
-- the `FulfillRequest` query to send the response. Only applies to Async/ContinuousAsync.
--
-- For now, specialized to a string.
data SyncMethod source err a
  = Sync a
  | Async source (RemoteData err a)
  | ContinuousAsync String source (RemoteData err a)

derive instance functorSyncMethod :: Functor (SyncMethod source err)

-- Could also provide 'Limit Int' for restricted lists
data SelectionType item
  = One (Maybe item)
  | Many (Array item)

derive instance functorSelectionType :: Functor SelectionType

data TypeaheadQuery o source err item e a
  = HandleContainer (Container.Message o item) a
  | HandleSearch (Search.Message o item) a
  | Remove item a
  | Selections (SelectionType item -> a)
  | FulfillRequest (SyncMethod source err (Array item)) a
  | TypeaheadReceiver (TypeaheadInput o source err item e) a

type TypeaheadInput o source err item e =
  { items :: SyncMethod source err (Array item)
  , debounceTime :: Milliseconds
  , search :: Maybe String
  , initialSelection :: SelectionType item
  , render :: TypeaheadState o source err item -> TypeaheadHTML o source err item e
  , config :: ConfigRecord item
  }

data TypeaheadMessage o source err item
  = ItemSelected item
  | ItemRemoved item
  | NewSearch String
  | RequestData (SyncMethod source err (Array item))
  | Emit (o Unit)

-- The idea: maintain an Either where you either provide
-- a configuration record, relying on default functionality
-- provided by the component, OR you can provide handlers
-- for the two important child messages (new search or
-- item selected)

--  type EvalConfig o source err item e =
--    Either (HandlerRecord o source err item e) (ConfigRecord item)

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

--  type HandlerRecord o source err item e =
--    { newSearch :: String -> TypeaheadDSL o source err item e Unit
--    , itemSelected :: item -> TypeaheadDSL o source err item e Unit
--    , itemRemoved :: item -> TypeaheadDSL o source err item e Unit
--    }


----------
-- Convenience component types

type TypeaheadComponent o source err item e =
  H.Component
    HH.HTML
    (TypeaheadQuery o source err item e)
    (TypeaheadInput o source err item e)
    (TypeaheadMessage o source err item)
    (Aff e)

type TypeaheadHTML o source err item e =
  H.ParentHTML
    (TypeaheadQuery o source err item e)
    (ChildQuery o item e)
    ChildSlot
    (Aff e)

type TypeaheadDSL o source err item e =
  H.ParentDSL
    (State o source err item e)
    (TypeaheadQuery o source err item e)
    (ChildQuery o item e)
    ChildSlot
    (TypeaheadMessage o source err item)
    (Aff e)


----------
-- Child types

type ContainerQuery o item =
  Container.ContainerQuery o item

type SearchQuery o item e =
  Search.SearchQuery o item (Effects e)

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

component :: ∀ o source err item e
  . Show err
 => Eq item
 => StringComparable item
 => TypeaheadComponent o source err item e
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input TypeaheadReceiver
    }
  where
    initialState :: TypeaheadInput o source err item e -> State o source err item e
    initialState i = store i.render $ State
        { items: i.items
        , selections: i.initialSelection
        , debounceTime: i.debounceTime
        , search: fromMaybe "" i.search
        , config: i.config
        }

    eval :: TypeaheadQuery o source err item e ~> TypeaheadDSL o source err item e
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

      -- Listens to data returned by the parent.
      FulfillRequest method a -> do
        -- TODO: This should handle any filtering / insertable stuff!
        -- Otherwise it will always send the full result. In the case
        -- of continuous async we may need filtering applied.
        -- (Tuple _ (State st)) <- getState

        H.modify $ seeks $ \(State st) -> State st { items = method }

        let updateContainer (Failure e) = do
              -- Close the container
              _ <- H.query' CP.cp1 (Slot ContainerSlot)
                $ H.action
                $ Container.Visibility Container.Off
              -- Clear the container
              _ <- H.query' CP.cp1 (Slot ContainerSlot)
                $ H.action
                $ Container.ReplaceItems []
              H.liftAff $ logShow e

            updateContainer (Success items) = do
              _ <- H.query' CP.cp1 (Slot ContainerSlot)
                $ H.action
                $ Container.ReplaceItems items
              pure unit

            updateContainer _ = pure unit

        case method of
          Async _ items -> updateContainer items
          ContinuousAsync _ _ items -> updateContainer items
          _ -> pure unit
        pure a

      -- Overwrite the state with new input.
      -- WARN: This will mess things up when the parent re-renders. Only here temporarily.
      -- TODO: Only modify relevant parts of state after the init.
      TypeaheadReceiver input a -> a <$ do
        H.put $ initialState input


----------
-- Helper functions

-- Intended so the end user can mount these slots in their Typeahead render function wherever they would like
-- without managing the wiring. Allows them to maintain full control over rendering without having to manage
-- handlers.

searchSlot :: ∀ o source err item e. Search.SearchInput o item (Effects e) -> TypeaheadHTML o source err item e
searchSlot i =
  HH.slot' CP.cp2 (Slot SearchSlot) Search.component i (HE.input HandleSearch)

containerSlot :: ∀ o source err item e. Container.ContainerInput o item -> TypeaheadHTML o source err item e
containerSlot i =
  HH.slot' CP.cp1 (Slot ContainerSlot) Container.component i (HE.input HandleContainer)


-- Given an item, the items, and the selections, move an item from the items list to
-- the selections list.
selectItem :: ∀ source err item
  . Eq item
 => StringComparable item
 => item
 -> SyncMethod source err (Array item)
 -> SelectionType item
 -> Tuple (SyncMethod source err (Array item)) (SelectionType item)
selectItem item items selections = case selections of
  One Nothing  -> Tuple (remove item items) (One $ Just item)
  One (Just i) -> Tuple (insert i $ remove item items) (One $ Just item)
  Many xs      -> Tuple (remove item items) (Many $ item : xs)
    where
      insert i is = (\i' -> i : i') <$> is
      remove i is = (filter ((/=) i)) <$> is

-- Given an item, the items, and the selections, move an item from the selections list to
-- the items list.
removeItem :: ∀ source err item
  . Eq item
 => StringComparable item
 => item
 -> SyncMethod source err (Array item)
 -> SelectionType item
 -> Tuple (SyncMethod source err (Array item)) (SelectionType item)
removeItem item items selections = case selections of
  One  _  -> Tuple (insert item items) (One Nothing)
  Many xs -> Tuple (insert item items) (Many $ filter ((/=) item) xs)
    where
      insert i is = (\i' -> i : i') <$> is



----------
-- Helper eval functions

-- These functions feed from the configuration options and allow for a variety of
-- behaviors out of the box. However, if you need more fine-grained control over
-- state and behaviors, you can provide custom handlers.

-- Use a default matching function to determine what items match the user's search,
-- or use one provided by the user. If searches are insertable, allow the user to select
-- items they've searched, constructing a new item from the string using the user's function.
-- Finally, update the new items available in the container.

evalNewSearch :: ∀ o source err item e
  . Show err
 => Eq item
 => StringComparable item
 => ConfigRecord item
 -> String
 -> TypeaheadDSL o source err item e Unit
evalNewSearch { filterType, insertable } text = unit <$ do
  -- Update the search value so it can replace the text field
  H.modify $ seeks \(State st') -> State $ st' { search = text }

  -- Fetch the state in order to get current items
  (Tuple _ (State st)) <- getState

  -- In non-continuous, use existing items from the state.
  -- In continuous, ask for the data and immediately move on.
  newItems <- case st.items of
    Sync items -> pure $ Sync $ (applyInsertable <<< applyFilter) items
    Async src items -> pure $ Async src $ (applyInsertable <<< applyFilter) <$> items
    ContinuousAsync _ src _ -> do
       -- Update the value with the search
       let continuous = ContinuousAsync text src Loading
       -- Update items to loading
       H.modify $ seeks $ \(State st') -> State st' { items = continuous }
       -- Request data
       H.raise $ RequestData continuous
       -- Return the loading value
       pure continuous

  _ <- case newItems of
    -- If sync, send new items to the container.
    Sync items -> do
      _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems items
      pure unit

    -- If async, handle remote data cases.
    Async _ items -> updateContainer items
    ContinuousAsync _ _ items -> updateContainer items

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
      H.liftAff $ logShow e
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
evalItemSelected :: ∀ o source err item e
  . StringComparable item
 => Eq item
 => ConfigRecord item
 -> item
 -> TypeaheadDSL o source err item e Unit
evalItemSelected { keepOpen } item = unit <$ do
  (Tuple _ (State st)) <- getState
  let (Tuple newItems newSelections) = selectItem item st.items st.selections

  -- Send the new items to the container
  case newItems of
    Sync i -> updateContainer (Success i)
    Async _ i -> updateContainer i
    ContinuousAsync _ _ i -> updateContainer i

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
evalItemRemoved :: ∀ o source err item e
  . StringComparable item
 => Eq item
 => item
 -> TypeaheadDSL o source err item e Unit
evalItemRemoved item = unit <$ do
  (Tuple _ (State st)) <- getState
  let (Tuple newItems newSelections) = removeItem item st.items st.selections

  H.modify $ seeks \(State st') -> State $ st'
    { selections = newSelections
    , items = newItems }

  case newItems of
    Sync i -> updateContainer (Success i)
    Async _ i -> updateContainer i
    ContinuousAsync _ _ i -> updateContainer i

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
