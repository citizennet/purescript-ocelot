module CN.UI.Core.Typeahead where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Array (filter, (:))
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.String (Pattern(..), contains)
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
  | Emit (o Unit)

-- The idea: maintain an Either where you either provide
-- a configuration record, relying on default functionality
-- provided by the component, OR you can provide handlers
-- for the two important child messages (new search or
-- item selected)

type EvalConfig o item e =
  Either (HandlerRecord o item e) ConfigRecord

-- Some standard functionality is baked in to the component
-- and can be configured if the user wants a more 'out of the
-- box' experience. This is a sample.
type ConfigRecord =
  { insertable  :: Boolean        -- If no match, insert?
  , matchType   :: MatchType      -- Function to match
  , keepOpen    :: Boolean        -- Stay open on selection?
  , duplicates  :: Boolean        -- Allow duplicates?
  }

data MatchType
  = Exact
  | CaseInsensitive
  | Fuzzy

-- A default config can help minimize their efforts.
defaultConfig :: ConfigRecord
defaultConfig =
  { insertable: false
  , matchType: Fuzzy
  , keepOpen: true
  , duplicates: false
  }

-- Alternately, they can provide the full handlers for the
-- two most important queries, with full access to the state
-- and slot types, though this is certainly a more 'advanced'
-- case.

type HandlerRecord o item e =
  { newSearch :: String -> TypeaheadDSL o item e Unit
  , itemSelected :: item -> TypeaheadDSL o item e Unit
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
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input TypeaheadReceiver
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

				-- TODO:
				-- Handle a new item selection
				Container.ItemSelected item -> a <$ do
					H.raise $ ItemSelected item
					(Tuple _ (State st)) <- getState
					case st.config of
						 Left { itemSelected } -> itemSelected item
						 Right _ -> itemSelectedFn item

			-- Handle messages from the search.
			HandleSearch message a -> case message of

				-- Evaluate an embedded parent query
				Search.Emit query -> H.raise (Emit query) *> pure a

				-- Route a container query to its correct slot.
				Search.ContainerQuery query -> do
					_ <- H.query' CP.cp1 (Slot ContainerSlot) query
					pure a

				-- TODO
				-- Handle a new search
				Search.NewSearch text -> a <$ do
					(Tuple _ (State st)) <- getState
					case st.config of
						 Left { newSearch } -> newSearch text
						 Right _ -> newSearchFn text

			-- Handle a 'remove' event on the selections list.
			Remove item a -> a <$ do
				(Tuple _ (State st)) <- getState

				H.modify $ seeks \(State st') -> State
					$ st' { items = item : st.items
								, selections = st.selections }

				H.raise $ ItemRemoved item

			-- Return the current selections to the parent.
			Selections reply -> do
				(Tuple _ (State st)) <- getState
				pure $ reply st.selections

			-- Overwrite the state with new input.
			TypeaheadReceiver input a -> a <$ do
				H.put $ initialState input


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


-- Searching requires the ability to compare the item to a string. We supply a type
-- class for StringComparable; strings already have an instance of it and take no
-- further effort.
newSearchFn :: ∀ o item e. Eq item => StringComparable item => String -> TypeaheadDSL o item e Unit
newSearchFn text = do
  (Tuple _ (State st)) <- getState

  let matches = filter (\item -> contains (Pattern text) (toString item)) st.items

  -- Update the selections
  H.modify $ seeks \(State st') -> State $ st' { search = text }

  -- Send the new items to the container
  _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems matches

  pure unit


-- Manages a selection depending on what kind of select this is.
itemSelectedFn :: ∀ o item e. Eq item => item -> TypeaheadDSL o item e Unit
itemSelectedFn item = do
  (Tuple _ (State st)) <- getState

  let (Tuple newSelections newItems) = case st.selections of
        One Nothing  -> Tuple (One $ Just item) (filter ((/=) item) st.items)
        One (Just i) -> Tuple (One $ Just item) ((:) i $ filter ((/=) item) st.items)
        Many xs      -> Tuple (Many $ item : xs) (filter ((/=) item) st.items)

  H.modify $ seeks \(State st') -> State $ st' { selections = newSelections }

  -- Send the new items to the container
  _ <- H.query' CP.cp1 (Slot ContainerSlot)
        $ H.action
        $ Container.ReplaceItems newItems

  pure unit




