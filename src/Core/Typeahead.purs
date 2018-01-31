module CN.UI.Core.Typeahead where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Data.Traversable (traverse_)
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
  { items :: Array item
  , selections :: SelectionType item
  , debounceTime :: Milliseconds
  , search :: String
  , config :: EvalConfig o item e
  }

-- Could also provide 'Limit Int' for restricted lists
data SelectionType item
  = One (Maybe item)
  | Many (Array item)

data TypeaheadQuery o item e a
  = HandleContainer (Container.Message o item) a
  | HandleSearch (Search.Message o item) a
  | Remove item a
  | Selections (SelectionType item -> a)
  | Initialize a
  | TypeaheadReceiver (TypeaheadInput o item e) a

type TypeaheadInput o item e =
  { items :: Array item
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
  { fetchType: Sync CaseInsensitive NotInsertable
  , keepOpen: true
  }

-- Some standard functionality is baked in to the component
-- and can be configured if the user wants a more 'out of the
-- box' experience.
type ConfigRecord item =
  { fetchType   :: FetchType item
  , keepOpen    :: Boolean
  }

-- Indicates how the typeahead should fetch data.
--
-- - `Sync`: The typeahead expects data provided by the parent.
-- - `Async`: The typeahead will fetch its own data on initialization (but not after)
-- - `ContinuousAsync`: The typeahead will fetch data every time a search is performed
data FetchType item
  = Sync (FilterType item) (Insertable item)
  | Async (FilterType item) (Insertable item) (forall e. Aff (ajax :: AJAX | e) (Array item))
  | ContinuousAsync (Insertable item) (forall e. String -> Aff (ajax :: AJAX | e) (Array item))

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

        case st.config of
          Right c -> case c.fetchType of
            Async _ _ fetch -> do
              items <- H.liftAff fetch
              H.liftAff $ traverse_ (log <<< toString) items
              H.modify
                $ seeks
                $ \(State st') -> State
                $ st' { items = items }
              pure a

            -- In any other case, do not modify item data.
            _ -> pure a
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
searchSlot i = HH.slot' CP.cp2 (Slot SearchSlot) Search.component i (HE.input HandleSearch)

containerSlot :: ∀ o item e. Container.ContainerInput o item -> TypeaheadHTML o item e
containerSlot i = HH.slot' CP.cp1 (Slot ContainerSlot) Container.component i (HE.input HandleContainer)


----------
-- Helper eval functions

-- These functions feed from the configuration options and allow for a variety of
-- behaviors out of the box. However, if you need more fine-grained control over
-- state and behaviors, you can provide custom handlers.

-- Use a default matching function to determine what items match the user's search,
-- or use one provided by the user. If searches are insertable, allow the user to select
-- items they've searched, constructing a new item from the string using the user's function.
-- Finally, update the new items available in the container.

evalNewSearch :: ∀ o item e. Eq item => StringComparable item
  => ConfigRecord item
  -> String
  -> TypeaheadDSL o item e Unit
evalNewSearch { fetchType } text = unit <$ do

  -- Update the search value
  H.modify $ seeks \(State st') -> State $ st' { search = text }

  -- If sync or async, use existing items from the state.
  -- If continuous async, use provided function to fetch the items (empty array if fails)
  newItems <- case fetchType of
    -- In any non-continuous, use the correct filter and do not fetch new items.
    Sync f i -> pure =<< applyInsertable i =<< applyFilter f
    Async f i _ -> pure =<< applyInsertable i =<< applyFilter f

    -- WARN: ContinuousAsync assumes you just want the items returned -- that you are filtering
    -- yourself in the function.
    ContinuousAsync i fetch -> H.liftAff (fetch text)

  -- Send the new items to the container
  _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems newItems

  H.raise $ NewSearch text

  where
    -- Set up the filter
    applyFilter matchType = getState >>= \(Tuple _ (State st)) -> pure $ do
      -- Note: The base filter matches on strings using the StringComparable type class
      case matchType of
        NoFilter ->
          st.items
        Exact ->
          filter (\item -> contains (Pattern text) (toString item)) st.items
        CaseInsensitive ->
          filter (\item -> contains (Pattern $ toLower text) (toLower $ toString item)) st.items
        CustomMatch match ->
          filter (\item -> match text item) st.items

    -- If the typeahead is Insertable, then use the provided function in that type
    -- to construct a new item and add it to the item list. NOTE: If you want to be able
    -- to tell what items have been inserted, ensure your item type encodes this! Your
    -- mkItem function can tag items that were inserted.
    applyInsertable insertable currentItems = case insertable of
      NotInsertable -> pure currentItems
      Insertable mkItem | length currentItems > 0 -> pure currentItems
                        | otherwise -> pure $ (mkItem text) : currentItems


-- Standard selection: Take the item out of the available items list and put it in the
-- selected list. Reset the search, and close the container unless it's set to stay open.

-- WARN: This doesn't tell you if the new item selected was an inserted item that needs
-- to be written anywhere else! If you need this functionality, your item type must encode
-- it, like: (MyItem IsNew item) & mkItem item = MyItem IsNew item
evalItemSelected :: ∀ o item e. Eq item => ConfigRecord item -> item -> TypeaheadDSL o item e Unit
evalItemSelected { keepOpen } item = unit <$ do
  (Tuple _ (State st)) <- getState

  let (Tuple newSelections newItems) = case st.selections of
        One Nothing  -> Tuple (One $ Just item) (filter ((/=) item) st.items)
        One (Just i) -> Tuple (One $ Just item) ((:) i $ filter ((/=) item) st.items)
        Many xs      -> Tuple (Many $ item : xs) (filter ((/=) item) st.items)

  H.modify $ seeks \(State st') -> State
    $ st' { selections = newSelections
          , items = newItems }

  -- Send the new items to the container
  _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems newItems

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


-- Standard removal: When an item is removed, put it back into the available items list
-- and take it out of the selected list.
evalItemRemoved :: ∀ o item e. Eq item => item -> TypeaheadDSL o item e Unit
evalItemRemoved item = unit <$ do
  (Tuple _ (State st)) <- getState

  let (Tuple newSelections newItems) = case st.selections of
        One _ -> Tuple (One Nothing) (item : st.items)
        Many xs -> Tuple (Many $ filter ((/=) item) xs) (item : st.items)

  H.modify $ seeks \(State st') -> State
    $ st' { selections = newSelections
          , items = newItems }

  -- Send the new items to the container
  _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems newItems

  H.raise $ ItemRemoved item
