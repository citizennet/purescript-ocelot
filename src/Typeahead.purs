-- | The typeahead module intended for imports
module Ocelot.Typeahead
  ( Action
  , ChildSlots
  , Component
  , ComponentHTML
  , ComponentRender
  , ComponentM
  , CompositeAction
  , CompositeComponent
  , CompositeComponentHTML
  , CompositeComponentM
  , CompositeComponentRender
  , CompositeInput
  , CompositeQuery
  , CompositeState
  , DefaultAsyncTypeaheadInput
  , DefaultSyncTypeaheadInput
  , EmbeddedAction(..)
  , EmbeddedChildSlots
  , Input
  , Insertable(..)
  , Operations
  , Output(..)
  , Query(..)
  , SelectionCause(..)
  , Slot
  , Spec
  , State
  , StateRow
  , StateStore
  , asyncMulti
  , asyncMultiInput
  , asyncSingle
  , component
  , defFilterFuzzy
  , defRenderContainer
  , disabledClasses
  , getNewItems'
  , inputProps
  , isDisabled
  , linkClasses
  , multi
  , multiHighlightOnly
  , renderError
  , renderHeaderSearchDropdown
  , renderMulti
  , renderMultiInput
  , renderSearchDropdown
  , renderSingle
  , renderToolbarSearchDropdown
  , single
  , singleHighlightOnly
  , spinner
  , syncMulti
  , syncMultiInput
  , syncSingle
  ) where

import Prelude
import Control.Alternative as Control.Alternative
import Control.Comonad as Control.Comonad
import Control.Comonad.Store as Control.Comonad.Store
import DOM.HTML.Indexed as DOM.HTML.Indexed
import Data.Array ((!!), (:))
import Data.Array as Data.Array
import Data.Foldable as Data.Foldable
import Data.Fuzzy as Data.Fuzzy
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Newtype as Data.Newtype
import Data.Rational ((%))
import Data.Time.Duration as Data.Time.Duration
import Effect.Aff.Class as Effect.Aff.Class
import Foreign.Object as Foreign.Object
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Core as Halogen.HTML.Core
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Network.RemoteData as Network.RemoteData
import Ocelot.Block.Button as Ocelot.Block.Button
import Ocelot.Block.Conditional as Ocelot.Block.Conditional
import Ocelot.Block.Format as Ocelot.Block.Format
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.Block.Input as Ocelot.Block.Input
import Ocelot.Block.ItemContainer as Ocelot.Block.ItemContainer
import Ocelot.Block.Loading as Ocelot.Block.Loading
import Ocelot.Components.MultiInput.Component as Ocelot.Components.MultiInput.Component
import Ocelot.HTML.Properties ((<&>))
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Renderless.State as Renderless.State
import Select as Select
import Select.Setters as Select.Setters
import Type.Proxy (Proxy(..))
import Unsafe.Coerce as Unsafe.Coerce

--------
-- Types

data Action action (f :: Type -> Type) item (m :: Type -> Type)
  = PassingOutput (Output action f item)
  | ReceiveRender (Input action f item m)

type ChildSlots action f item =
  ( select :: Select.Slot (Query f item) EmbeddedChildSlots (Output action f item) Unit
  )

type Component action f item m
  = Halogen.Component (Query f item) (Input action f item m) (Output action f item) m

type ComponentHTML action f item m
  = Halogen.ComponentHTML (Action action f item m) (ChildSlots action f item) m

type ComponentRender action f item m
  = State f item m -> ComponentHTML action f item m

type ComponentM action f item m a
  = Halogen.HalogenM (StateStore action f item m) (Action action f item m) (ChildSlots action f item) (Output action f item) m a

type CompositeAction action f item m
  = Select.Action (EmbeddedAction action f item m)

type CompositeComponent action f item m
  = Halogen.Component (CompositeQuery f item) (CompositeInput f item m) (Output action f item) m

type CompositeComponentHTML action f item m
  = Halogen.ComponentHTML (CompositeAction action f item m) EmbeddedChildSlots m

type CompositeComponentM action f item m a
  = Halogen.HalogenM (CompositeState f item m) (CompositeAction action f item m) EmbeddedChildSlots (Output action f item) m a

type CompositeComponentRender action f item m
  = (CompositeState f item m) -> CompositeComponentHTML action f item m

type CompositeInput f item m
  = Select.Input (StateRow f item m)

type CompositeQuery f item
  = Select.Query (Query f item) EmbeddedChildSlots

type CompositeState f item m
  = Select.State (StateRow f item m)

type DefaultAsyncTypeaheadInput item m
  = { itemToObject :: item -> Foreign.Object.Object String
    , renderFuzzy :: Data.Fuzzy.Fuzzy item -> Halogen.HTML.PlainHTML
    , async :: String -> m (Network.RemoteData.RemoteData String (Array item))
    }

type DefaultSyncTypeaheadInput item
  = { itemToObject :: item -> Foreign.Object.Object String
    , renderFuzzy :: Data.Fuzzy.Fuzzy item -> Halogen.HTML.PlainHTML
    }

data EmbeddedAction action (f :: Type -> Type) item (m :: Type -> Type)
  = HandleMultiInput Ocelot.Components.MultiInput.Component.Output
  | Initialize
  | Raise action
  | Remove item
  | RemoveAll

type EmbeddedChildSlots
  = ( multiInput :: Ocelot.Components.MultiInput.Component.Slot Unit
    )

type Input action f item m
  = { async :: Maybe (String -> m (Network.RemoteData.RemoteData String (Array item)))
    , debounceTime :: Maybe Data.Time.Duration.Milliseconds
    , disabled :: Boolean
    , insertable :: Insertable item
    , itemToObject :: item -> Foreign.Object.Object String
    , items :: Network.RemoteData.RemoteData String (Array item)
    , keepOpen :: Boolean
    , render :: CompositeComponentRender action f item m
    }

data Insertable item
  = NotInsertable
  | Insertable (String -> item)

type Operations f item
  = { runSelect :: item -> f item -> f item
    , runRemove :: item -> f item -> f item
    , runFilterFuzzy :: Array (Data.Fuzzy.Fuzzy item) -> Array (Data.Fuzzy.Fuzzy item)
    , runFilterItems :: Array item -> f item -> Array item
    }

data Output action (f :: Type -> Type) item
  = Searched String
  | Selected item
  | SelectionChanged SelectionCause (f item)
  | Emit action

data Query f item a
  = GetSelected (f item -> a)
  | ReplaceSelected (f item) a
  | ReplaceSelectedBy (Array item -> f item) a
  | ReplaceItems (Network.RemoteData.RemoteData String (Array item)) a
  | Reset a
  | SetDisabled Boolean a

data SelectionCause
  = RemovalQuery
  | ReplacementQuery
  | ResetQuery
  | SelectionMessage

derive instance eqSelectionCause :: Eq SelectionCause

type Slot action f item id
  = Halogen.Slot (Query f item) (Output action f item) id

type Spec action f item m
  = Select.Spec (StateRow f item m) (Query f item) (EmbeddedAction action f item m) EmbeddedChildSlots (CompositeInput f item m) (Output action f item) m

type State f item m
  = Record (StateRow f item m)

type StateRow f item m
  = ( items :: Network.RemoteData.RemoteData String (Array item) -- NOTE pst.items, Parent(Typeahead)
    , insertable :: Insertable item
    , keepOpen :: Boolean
    , itemToObject :: item -> Foreign.Object.Object String
    , async :: Maybe (String -> m (Network.RemoteData.RemoteData String (Array item)))
    , disabled :: Boolean
    , ops :: Operations f item
    , config :: { debounceTime :: Maybe Data.Time.Duration.Milliseconds }
    , selected :: f item
    , fuzzyItems :: Array (Data.Fuzzy.Fuzzy item) -- NOTE cst.items, Child(Select)
    )

type StateStore action f item m
  = Control.Comonad.Store.Store (State f item m) (ComponentHTML action f item m)

-------------
-- Components

component
  :: forall action f item m
  . Control.Alternative.Plus f
  => Data.Foldable.Foldable f
  => Eq item
  => Effect.Aff.Class.MonadAff m
  => Operations f item
  -> Component action f item m
component ops = Halogen.mkComponent
  { initialState: initialState ops
  , render: Control.Comonad.extract
  , eval: Halogen.mkEval Halogen.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

single
  :: forall action item m
  . Eq item
  => Effect.Aff.Class.MonadAff m
  => Component action Maybe item m
single = component
  { runSelect: const <<< Just
  , runRemove: const (const Nothing)
  , runFilterFuzzy: defFilterFuzzy
  , runFilterItems: \items -> Data.Maybe.maybe items (\i -> Data.Array.filter (_ /= i) items)
  }

-- | Typeahead that doesn't filter or search results, only highlights matches.
-- |
-- | This is useful when using an endpoint that already filters and sorts the results.
singleHighlightOnly ::
  forall action item m.
  Eq item =>
  Effect.Aff.Class.MonadAff m =>
  Component action Maybe item m
singleHighlightOnly = component
  { runSelect: const <<< Just
  , runRemove: const (const Nothing)
  , runFilterFuzzy: identity
  , runFilterItems: \items -> Data.Maybe.maybe items (\item -> Data.Array.filter (_ /= item) items)
  }


multi
  :: forall action item m
  . Eq item
  => Effect.Aff.Class.MonadAff m
  => Component action Array item m
multi = component
  { runSelect: (:)
  , runRemove: Data.Array.filter <<< (/=)
  , runFilterFuzzy: defFilterFuzzy
  , runFilterItems: Data.Array.difference
  }

-- | Typeahead that doesn't filter or search results, only highlights matches.
-- |
-- | This is useful when using an endpoint that already filters and sorts the results.
multiHighlightOnly ::
  forall action item m.
  Eq item =>
  Effect.Aff.Class.MonadAff m =>
  Component action Array item m
multiHighlightOnly = component
  { runSelect: (:)
  , runRemove: Data.Array.filter <<< (/=)
  , runFilterFuzzy: identity
  , runFilterItems: Data.Array.difference
  }

asyncSingle
  :: ∀ action item m
   . Eq item
  => Effect.Aff.Class.MonadAff m
  => DefaultAsyncTypeaheadInput item m
  -> Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action Maybe item m))
  -> Input action Maybe item m
asyncSingle { async, itemToObject, renderFuzzy } props =
  { async: Just async
  , debounceTime: Just $ Data.Time.Duration.Milliseconds 300.0
  , disabled: isDisabled props
  , insertable: NotInsertable
  , itemToObject
  , items: Network.RemoteData.NotAsked
  , keepOpen: false
  , render: renderSingle
      props
      (renderFuzzy <<< Data.Fuzzy.match false itemToObject "")
      (defRenderContainer renderFuzzy)
  }

asyncMulti
  :: ∀ action item m
   . Eq item
  => Effect.Aff.Class.MonadAff m
  => DefaultAsyncTypeaheadInput item m
  -> Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action Array item m))
  -> Input action Array item m
asyncMulti { async, itemToObject, renderFuzzy } props =
  { async: Just async
  , debounceTime: Just $ Data.Time.Duration.Milliseconds 300.0
  , disabled: isDisabled props
  , insertable: NotInsertable
  , itemToObject
  , items: Network.RemoteData.NotAsked
  , keepOpen: true
  , render: renderMulti
      props
      (renderFuzzy <<< Data.Fuzzy.match false itemToObject "")
      (defRenderContainer renderFuzzy)
  }

asyncMultiInput
  :: ∀ action item m
  . Eq item
  => Effect.Aff.Class.MonadAff m
  => DefaultAsyncTypeaheadInput item m
  -> Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action Array item m))
  -> Ocelot.Components.MultiInput.Component.Input
  -> Input action Array item m
asyncMultiInput { async, itemToObject, renderFuzzy } props input =
  { async: Just async
  , debounceTime: Just $ Data.Time.Duration.Milliseconds 300.0
  , disabled: isDisabled props
  , insertable: NotInsertable
  , itemToObject
  , items: Network.RemoteData.NotAsked
  , keepOpen: false
  , render: renderMultiInput
      input
      (defRenderContainer renderFuzzy)
  }

syncSingle
  :: ∀ action item m
   . Eq item
  => Effect.Aff.Class.MonadAff m
  => DefaultSyncTypeaheadInput item
  -> Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action Maybe item m))
  -> Input action Maybe item m
syncSingle { itemToObject, renderFuzzy } props =
  { async: Nothing
  , debounceTime: Nothing
  , disabled: isDisabled props
  , insertable: NotInsertable
  , itemToObject
  , items: Network.RemoteData.NotAsked
  , keepOpen: false
  , render: renderSingle
      props
      (renderFuzzy <<< Data.Fuzzy.match false itemToObject "")
      (defRenderContainer renderFuzzy)
  }

syncMultiInput
  :: ∀ action item m
  . Eq item
  => Effect.Aff.Class.MonadAff m
  => DefaultSyncTypeaheadInput item
  -> Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action Array item m))
  -> Ocelot.Components.MultiInput.Component.Input
  -> Input action Array item m
syncMultiInput { itemToObject, renderFuzzy } props input =
  { async: Nothing
  , debounceTime: Nothing
  , disabled: isDisabled props
  , insertable: NotInsertable
  , itemToObject
  , items: Network.RemoteData.NotAsked
  , keepOpen: false
  , render: renderMultiInput
      input
      (defRenderContainer renderFuzzy)
  }


syncMulti
  :: ∀ action item m
   . Eq item
  => Effect.Aff.Class.MonadAff m
  => DefaultSyncTypeaheadInput item
  -> Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action Array item m))
  -> Input action Array item m
syncMulti { itemToObject, renderFuzzy } props =
  { async: Nothing
  , debounceTime: Nothing
  , disabled: isDisabled props
  , insertable: NotInsertable
  , itemToObject
  , items: Network.RemoteData.NotAsked
  , keepOpen: true
  , render: renderMulti
      props
      (renderFuzzy <<< Data.Fuzzy.match false itemToObject "")
      (defRenderContainer renderFuzzy)
  }

---------
-- Values

_multiInput = Proxy :: Proxy "multiInput"

_select = Proxy :: Proxy "select"

applyInsertable
  :: forall item
  . (item -> Data.Fuzzy.Fuzzy item)
  -> Insertable item
  -> String
  -> Array (Data.Fuzzy.Fuzzy item)
  -> Array (Data.Fuzzy.Fuzzy item)
applyInsertable _ _ "" items = items
applyInsertable match insertable text items = case insertable of
  NotInsertable -> items
  Insertable mkItem | Data.Array.length (Data.Array.filter isExactMatch items) > 0 -> items
                    | otherwise -> (match $ mkItem text) : items
  where
    isExactMatch (Data.Fuzzy.Fuzzy { distance }) = distance == Data.Fuzzy.Distance 0 0 0 0 0 0

defFilterFuzzy ::
  forall item.
  Eq item =>
  Array (Data.Fuzzy.Fuzzy item) ->
  Array (Data.Fuzzy.Fuzzy item)
defFilterFuzzy =
  Data.Array.sort
    <<< Data.Array.filter (\(Data.Fuzzy.Fuzzy { ratio }) -> ratio > (2 % 3))

defRenderContainer
  :: ∀ action f item m
   . (Data.Fuzzy.Fuzzy item -> Halogen.HTML.PlainHTML)
  -> CompositeComponentRender action f item m
defRenderContainer renderFuzzy st =
  Ocelot.Block.ItemContainer.itemContainer st.highlightedIndex (renderFuzzy <$> st.fuzzyItems) []

disabledClasses :: Array Halogen.HTML.ClassName
disabledClasses = Halogen.HTML.ClassName <$>
  [ "bg-grey-95"
  , "text-grey-70"
  , "sibling:bg-grey-95"
  , "sibling:text-grey-50"
  , "border-t-2"
  , "border-b-2"
  , "font-light"
  , "focus:no-outline"
  , "py-2"
  , "border-l-2"
  , "w-full"
  , "px-3"
  ]

embeddedHandleAction
  :: forall action f item m
  . Eq item
  => Control.Alternative.Plus f
  => Effect.Aff.Class.MonadAff m
  => EmbeddedAction action f item m
  -> CompositeComponentM action f item m Unit
embeddedHandleAction = case _ of
  HandleMultiInput output -> embeddedHandleMultiInput output
  Initialize -> do
    synchronize
  Remove item -> embeddedRemove item
  RemoveAll -> do
    st <- Halogen.modify \st ->
      st { selected = Control.Alternative.empty :: f item
         , visibility = Select.Off
         }
    Halogen.raise $ SelectionChanged RemovalQuery st.selected
    synchronize
  Raise action -> do
    Halogen.raise $ Emit action

embeddedHandleMessage
  :: forall action f item m
   . Eq item
  => Effect.Aff.Class.MonadAff m
  => Select.Event
  -> CompositeComponentM action f item m Unit
embeddedHandleMessage = case _ of
  Select.Selected idx -> do
    { fuzzyItems } <- Halogen.get
    case fuzzyItems !! idx of
      Nothing -> pure unit
      Just (Data.Fuzzy.Fuzzy { original: item }) -> do
        st <- Halogen.modify \st -> st { selected = st.ops.runSelect item st.selected }
        when (not st.keepOpen) do
          Halogen.modify_ _ { visibility = Select.Off }
        case Data.Array.head (Foreign.Object.values (st.itemToObject item )) of
          Nothing -> pure unit
          Just text -> do
            void $ Halogen.tell _multiInput unit
              $ Ocelot.Components.MultiInput.Component.SelectItem text
        Halogen.raise $ SelectionChanged SelectionMessage st.selected
        Halogen.raise $ Selected item
        synchronize
  -- Perform a new search, fetching data if Async.
  Select.Searched text -> do
    Halogen.modify_ _ { search = text }
    async <- Halogen.gets _.async
    case async of
      Nothing -> pure unit
      Just fetchItems -> do
        Halogen.modify_ _ { items = Network.RemoteData.Loading }
        synchronize
        newItems <- Halogen.lift $ fetchItems text
        Halogen.modify_ _ { items = newItems }
    Halogen.raise $ Searched text
    synchronize
  _ -> pure unit

embeddedHandleMultiInput
  :: forall action f item m
  . Control.Alternative.Plus f
  => Eq item
  => Effect.Aff.Class.MonadAff m
  => Ocelot.Components.MultiInput.Component.Output
  -> CompositeComponentM action f item m Unit
embeddedHandleMultiInput = case _ of
  Ocelot.Components.MultiInput.Component.ItemsUpdated _ -> pure unit
  Ocelot.Components.MultiInput.Component.ItemRemoved text -> do
    state <- Halogen.get
    case
      do
        items <- Network.RemoteData.toMaybe state.items
        Data.Array.find
          (Data.Array.elem text <<< Foreign.Object.values <<< state.itemToObject)
          items
      of
      Nothing -> pure unit
      Just item -> embeddedRemove item
  Ocelot.Components.MultiInput.Component.On htmlEvents -> case htmlEvents of
    Ocelot.Components.MultiInput.Component.Blur ->
      Select.handleAction embeddedHandleAction embeddedHandleMessage
        $ Select.SetVisibility Select.Off
    Ocelot.Components.MultiInput.Component.Focus ->
      Select.handleAction embeddedHandleAction embeddedHandleMessage
        $ Select.SetVisibility Select.On
    Ocelot.Components.MultiInput.Component.KeyDown keyboardEvent ->
      Select.handleAction embeddedHandleAction embeddedHandleMessage
        $ Select.Key keyboardEvent
    Ocelot.Components.MultiInput.Component.MouseDown mouseEvent ->
      Select.handleAction embeddedHandleAction embeddedHandleMessage
        $ Select.ToggleClick mouseEvent
    Ocelot.Components.MultiInput.Component.ValueInput text ->
      Select.handleAction embeddedHandleAction embeddedHandleMessage
        $ Select.Search text

embeddedHandleQuery
  :: forall action f item m a
  . Control.Alternative.Plus f
  => Data.Foldable.Foldable f
  => Eq item
  => Effect.Aff.Class.MonadAff m
  => Query f item a
  -> CompositeComponentM action f item m (Maybe a)
embeddedHandleQuery = case _ of
  GetSelected reply -> do
    { selected } <- Halogen.get
    pure $ Just $ reply selected
  ReplaceSelected selected a -> Just a <$ do
    replaceSelected selected
  ReplaceSelectedBy f a -> Just a <$ do
    { items } <- Halogen.get
    case items of
      Network.RemoteData.Success items' -> replaceSelected (f items')
      _ -> pure unit
  ReplaceItems items a -> Just a <$ do
    Halogen.modify_ _ { items = items }
    synchronize
  Reset a -> Just a <$ do
    st <-
      Halogen.modify
        _
          { selected = Control.Alternative.empty :: f item
          , items = Network.RemoteData.NotAsked
          }
    Halogen.raise $ SelectionChanged ResetQuery st.selected
    synchronize
  SetDisabled disabled a -> Just a <$ do
    Halogen.modify_ _ { disabled = disabled }

embeddedInitialize :: forall action f item m. Maybe (EmbeddedAction action f item m)
embeddedInitialize = Just Initialize

-- NOTE configure Select
embeddedInput :: forall f item m. State f item m -> CompositeInput f item m
embeddedInput { items, selected, insertable, keepOpen, itemToObject, ops, async, fuzzyItems, config: { debounceTime }, disabled } =
  { async
  , config: { debounceTime } -- NOTE overhead
  , debounceTime
  , disabled
  , fuzzyItems
  , getItemCount: Data.Array.length <<< _.fuzzyItems
  , inputType: Select.Text
  , insertable
  , itemToObject
  , items
  , keepOpen
  , ops
  , search: Nothing
  , selected
  }

embeddedRemove
  :: forall action f item m
  . Eq item
  => Effect.Aff.Class.MonadAff m
  => item
  -> CompositeComponentM action f item m Unit
embeddedRemove item = do
  st <- Halogen.modify \st -> st { selected = st.ops.runRemove item st.selected }
  Halogen.raise $ SelectionChanged RemovalQuery st.selected
  synchronize

getNewItems ::
  forall f item m.
  Effect.Aff.Class.MonadAff m =>
  Eq item =>
  CompositeState f item m ->
  Network.RemoteData.RemoteData String (Array (Data.Fuzzy.Fuzzy item))
getNewItems st = st.items <#> \items ->
  getNewItems'
    { insertable: st.insertable
    , itemToObject: st.itemToObject
    , runFilterFuzzy: st.ops.runFilterFuzzy
    , runFilterItems: st.ops.runFilterItems
    , search: st.search
    , selected: st.selected
    }
    items

getNewItems' ::
  forall f item state.
  { insertable :: Insertable item
  , itemToObject :: item -> Foreign.Object.Object String
  , runFilterFuzzy :: Array (Data.Fuzzy.Fuzzy item) -> Array (Data.Fuzzy.Fuzzy item)
  , runFilterItems :: Array item -> f item -> Array item
  , search :: String
  , selected :: f item
  | state
  } ->
  Array item ->
  Array (Data.Fuzzy.Fuzzy item)
getNewItems' st =
  st.runFilterFuzzy
    <<< applyInsert
    <<< fuzzyItems
    <<< flip st.runFilterItems st.selected
  where
    matcher :: item -> Data.Fuzzy.Fuzzy item
    matcher = Data.Fuzzy.match true st.itemToObject st.search

    fuzzyItems :: Array item -> Array (Data.Fuzzy.Fuzzy item)
    fuzzyItems = map matcher

    applyInsert :: Array (Data.Fuzzy.Fuzzy item) -> Array (Data.Fuzzy.Fuzzy item)
    applyInsert = applyInsertable matcher st.insertable st.search

-- NOTE re-raise output messages from the embedded component
-- NOTE update Dropdown render function if it relies on external state
handleAction
  :: forall action f item m
  . Control.Alternative.Plus f
  => Data.Foldable.Foldable f
  => Eq item
  => Effect.Aff.Class.MonadAff m
  => Action action f item m
  -> ComponentM action f item m Unit
handleAction = case _ of
  PassingOutput output ->
    Halogen.raise output
  ReceiveRender { render } -> do
    Renderless.State.modifyStore_ (renderAdapter render) identity

-- NOTE passing query to the embedded component
handleQuery :: forall action f item m a. Query f item a -> ComponentM action f item m (Maybe a)
handleQuery = case _ of
  GetSelected reply -> do
    response <- Halogen.request _select unit (Select.Query <<< GetSelected)
    pure $ reply <$> response
  ReplaceSelected selected a -> Just a <$ do
    Halogen.tell _select unit (Select.Query <<< ReplaceSelected selected)
  ReplaceSelectedBy f a -> Just a <$ do
    Halogen.tell _select unit (Select.Query <<< ReplaceSelectedBy f)
  ReplaceItems items a -> Just a <$ do
    Halogen.tell _select unit (Select.Query <<< ReplaceItems items)
  Reset a -> Just a <$ do
    Halogen.tell _select unit (Select.Query <<< Reset)
  SetDisabled disabled a -> Just a <$ do
    Halogen.tell _select unit (Select.Query <<< SetDisabled disabled)

initialState
  :: forall action f item m
  . Control.Alternative.Plus f
  => Data.Foldable.Foldable f
  => Eq item
  => Effect.Aff.Class.MonadAff m
  => Operations f item
  -> Input action f item m
  -> StateStore action f item m
initialState ops
  { items, insertable, keepOpen, itemToObject, async, debounceTime, render, disabled }
  = Control.Comonad.Store.store (renderAdapter render)
      { async
      , config: {debounceTime}
      , disabled
      , fuzzyItems: []
      , insertable
      , itemToObject
      , items
      , keepOpen
      , ops
      , selected: Control.Alternative.empty :: f item
      }

inputProps
  :: ∀ action f item m
   . Boolean
  -> Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action f item m))
  -> Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action f item m))
inputProps disabled iprops = if disabled
  then iprops'
  else Select.Setters.setInputProps iprops'
  where
    iprops' =
      [ Halogen.HTML.Properties.disabled disabled
      , Halogen.HTML.Properties.autocomplete false
      , Ocelot.HTML.Properties.css "focus:next:text-blue-88"
      ]
      <&> iprops

isDisabled :: ∀ i. Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLinput i) -> Boolean
isDisabled = Data.Array.foldr f false
  where
    f (Halogen.HTML.Properties.IProp (Halogen.HTML.Core.Property "disabled" disabled))
      | coercePropValue disabled == true = (||) true
    f _ = (||) false

    coercePropValue :: Halogen.HTML.Core.PropValue -> Boolean
    coercePropValue = Unsafe.Coerce.unsafeCoerce

linkClasses :: Boolean -> Array Halogen.HTML.ClassName
linkClasses = if _
  then Halogen.HTML.ClassName <$> [ "text-grey-70", "no-underline", "font-medium" ]
  else Ocelot.Block.Format.linkClasses

renderAdapter
  :: forall action f item m
  . Control.Alternative.Plus f
  => Data.Foldable.Foldable f
  => Eq item
  => Effect.Aff.Class.MonadAff m
  => CompositeComponentRender action f item m
  -> ComponentRender action f item m
renderAdapter render state =
  Halogen.HTML.slot _select unit (Select.component identity $ spec render)
    (embeddedInput state)
    PassingOutput

renderError :: ∀ p i. Boolean -> Halogen.HTML.HTML p i
renderError error =
  Ocelot.Block.Conditional.conditional error
    [ Ocelot.HTML.Properties.css "flex items-center mt-1" ]
    [ Ocelot.Block.Icon.error
      [ Ocelot.HTML.Properties.css "text-2xl text-yellow" ]
    , Halogen.HTML.p
      [ Ocelot.HTML.Properties.css "ml-3 text-grey-50 font-light" ]
      [ Halogen.HTML.text "Some data could not be retrieved here." ]
    ]

renderHeaderSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> String
  -> (item -> Halogen.HTML.PlainHTML)
  -> (Data.Fuzzy.Fuzzy item -> Halogen.HTML.PlainHTML)
  -> CompositeComponentRender action Maybe item m
renderHeaderSearchDropdown defaultLabel resetLabel renderItem renderFuzzy st =
  renderSearchDropdown resetLabel label renderFuzzy st
  where
    label = Halogen.HTML.span
      [ Ocelot.HTML.Properties.css "text-white text-3xl font-thin cursor-pointer whitespace-no-wrap" ]
      [ Data.Maybe.maybe (Halogen.HTML.text defaultLabel) (Halogen.HTML.fromPlainHTML <<< renderItem) st.selected
      , Ocelot.Block.Icon.collapse [ Ocelot.HTML.Properties.css "ml-3 text-xl text-grey-50 align-middle" ]
      ]

renderMulti
  :: ∀ action item m
  . Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action Array item m))
  -> (item -> Halogen.HTML.PlainHTML)
  -> CompositeComponentRender action Array item m
  -> CompositeComponentRender action Array item m
renderMulti iprops renderItem renderContainer st =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "relative" ]
    [ if (not disabled && not Data.Array.null st.selected)
        then
          Halogen.HTML.a
            [ Ocelot.HTML.Properties.css "absolute -mt-7 pin-r underline text-grey-70 cursor-pointer"
            , Halogen.HTML.Events.onClick \_ -> Select.Action $ RemoveAll
            ]
            [ Halogen.HTML.text "Remove All" ]
        else
          Halogen.HTML.text ""
    , Ocelot.Block.ItemContainer.selectionContainer $ st.selected <#>
        if disabled
          then
            Halogen.HTML.fromPlainHTML <<< renderItem
          else
            \selected ->
              Ocelot.Block.ItemContainer.selectionGroup
                renderItem
                []
                [ Halogen.HTML.Events.onClick \_ -> Select.Action $ Remove selected ]
                selected
    , Ocelot.Block.Input.inputGroup_
      [ Ocelot.Block.Input.inputCenter $ inputProps disabled iprops
      , Ocelot.Block.Input.addonLeft_
        [ Ocelot.Block.Icon.search_ ]
      , Ocelot.Block.Input.addonCenter
        [ Ocelot.HTML.Properties.css $ if Network.RemoteData.isLoading st.items then "" else "offscreen" ]
        [ spinner ]
      , Ocelot.Block.Input.borderRight
        [ Halogen.HTML.Properties.classes $ linkClasses disabled ]
        [ Halogen.HTML.text "Browse" ]
      ]
    , Ocelot.Block.Conditional.conditional (st.visibility == Select.On)
        [ Ocelot.HTML.Properties.css "relative block" ]
        [ renderContainer st ]
    , renderError $ Network.RemoteData.isFailure st.items
    ]
  where
  disabled = st.disabled

renderMultiInput
  :: ∀ action item m
  . Effect.Aff.Class.MonadAff m
  => Ocelot.Components.MultiInput.Component.Input
  -> CompositeComponentRender action Array item m
  -> CompositeComponentRender action Array item m
renderMultiInput input renderContainer st =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "relative" ]
    [ Halogen.HTML.slot _multiInput unit
        Ocelot.Components.MultiInput.Component.component
        input
        (Select.Action <<< HandleMultiInput)
    , Ocelot.Block.Conditional.conditional (st.visibility == Select.On)
        [ Ocelot.HTML.Properties.css "relative block" ]
        [ renderContainer st ]
    , renderError $ Network.RemoteData.isFailure st.items
    ]

renderSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> Halogen.HTML.PlainHTML
  -> (Data.Fuzzy.Fuzzy item -> Halogen.HTML.PlainHTML)
  -> CompositeComponentRender action Maybe item m
renderSearchDropdown resetLabel label renderFuzzy st =
  Halogen.HTML.label
    [ Ocelot.HTML.Properties.css "relative" ]
    [ Halogen.HTML.fromPlainHTML label
    , Halogen.HTML.div
      [ Halogen.HTML.Properties.classes
        $ Halogen.HTML.ClassName "min-w-80" :
          if st.visibility == Select.Off
            then [ Halogen.HTML.ClassName "offscreen" ]
            else []
      ]
      [ Ocelot.Block.ItemContainer.dropdownContainer
        [ renderInput, renderReset ]
        renderFuzzy
        ((==) st.selected <<< Just <<< _.original <<< Data.Newtype.unwrap)
        st.fuzzyItems
        st.highlightedIndex
      ]
    ]
  where
  renderInput =
    Halogen.HTML.div
      [ Ocelot.HTML.Properties.css "m-4 border-b-2 border-blue-88 pb-2 flex" ]
      [ Ocelot.Block.Icon.search [ Ocelot.HTML.Properties.css "mr-4 text-xl text-grey-70" ]
      , Halogen.HTML.input
        $ inputProps false [ Ocelot.HTML.Properties.css "no-outline w-full", Halogen.HTML.Properties.placeholder "Search" ]
      ]

  renderReset =
    Ocelot.Block.ItemContainer.dropdownItem
      Halogen.HTML.div
      [ Halogen.HTML.Events.onClick \_ -> Select.Action $ RemoveAll
      ]
      [ Halogen.HTML.text resetLabel ]
      ( Data.Maybe.isNothing st.selected )
      false

renderSingle
  :: ∀ action item m
  . Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLinput (CompositeAction action Maybe item m))
  -> (item -> Halogen.HTML.PlainHTML)
  -> CompositeComponentRender action Maybe item m
  -> CompositeComponentRender action Maybe item m
renderSingle iprops renderItem renderContainer st =
  Halogen.HTML.div_
    [ Ocelot.Block.Input.inputGroup' Halogen.HTML.div
      [ Ocelot.HTML.Properties.css $ if showSelected then "" else "offscreen" ]
      [ if disabled
          then
            Data.Maybe.maybe (Halogen.HTML.text "")
              ( \selected -> Halogen.HTML.div
                [ Halogen.HTML.Properties.classes disabledClasses ]
                [ Halogen.HTML.fromPlainHTML $ renderItem selected ]
              )
            st.selected
          else
            Data.Maybe.maybe (Halogen.HTML.text "")
            ( \selected -> Halogen.HTML.div
              [ Halogen.HTML.Properties.classes Ocelot.Block.Input.mainLeftClasses ]
              [ Ocelot.Block.ItemContainer.selectionGroup renderItem
                [ Halogen.HTML.Events.onClick Select.ToggleClick ]
                [ Halogen.HTML.Events.onClick \_ -> Select.Action $ Remove selected ]
                selected
              ])
            st.selected
      , Ocelot.Block.Input.borderRight
        [ Halogen.HTML.Properties.classes $ linkClasses disabled
        , Halogen.HTML.Events.onClick Select.ToggleClick
        ]
        [ Halogen.HTML.text "Change" ]
      ]
    , Ocelot.Block.Input.inputGroup
      [ Ocelot.HTML.Properties.css $ if showSelected then "offscreen" else "" ]
      [ Ocelot.Block.Input.inputCenter $ inputProps disabled iprops
      , Ocelot.Block.Input.addonLeft_
        [ Ocelot.Block.Icon.search_ ]
      , Ocelot.Block.Input.addonCenter
        [ Ocelot.HTML.Properties.css $ if Network.RemoteData.isLoading st.items then "" else "offscreen" ]
        [ spinner ]
      , Ocelot.Block.Input.borderRight
        [ Halogen.HTML.Properties.classes $ linkClasses disabled ]
        [ Halogen.HTML.text "Browse" ]
      ]
    , Ocelot.Block.Conditional.conditional (st.visibility == Select.On)
        [ Ocelot.HTML.Properties.css "relative block" ]
        [ renderContainer st ]
    , renderError $ Network.RemoteData.isFailure st.items
    ]
  where
  disabled = st.disabled
  showSelected = Data.Maybe.isJust st.selected && st.visibility == Select.Off

renderToolbarSearchDropdown
  :: ∀ action item m
   . Eq item
  => String
  -> String
  -> (item -> Halogen.HTML.PlainHTML)
  -> (Data.Fuzzy.Fuzzy item -> Halogen.HTML.PlainHTML)
  -> CompositeComponentRender action Maybe item m
renderToolbarSearchDropdown defaultLabel resetLabel renderItem renderFuzzy st =
  renderSearchDropdown resetLabel label renderFuzzy st
  where
    label = Ocelot.Block.ItemContainer.dropdownButton
      Halogen.HTML.span
      [ Halogen.HTML.Properties.classes
        $ Halogen.HTML.ClassName "whitespace-no-wrap"
        : Ocelot.Block.Button.buttonMainClasses
        <> Ocelot.Block.Button.buttonClearClasses
      ]
      [ Data.Maybe.maybe (Halogen.HTML.text defaultLabel) (Halogen.HTML.fromPlainHTML <<< renderItem) st.selected ]

replaceSelected
  :: forall action f item m
  . Data.Foldable.Foldable f
  => Eq item
  => Effect.Aff.Class.MonadAff m
  => f item
  -> CompositeComponentM action f item m Unit
replaceSelected selected = do
  st <- Halogen.modify _ { selected = selected }
  Halogen.tell _multiInput unit
    $ Ocelot.Components.MultiInput.Component.SetItems
        ( Data.Array.mapMaybe (Data.Array.head <<< Foreign.Object.values <<< st.itemToObject)
            <<< Data.Array.fromFoldable
            $ selected
        )
  Halogen.raise $ SelectionChanged ReplacementQuery st.selected
  synchronize

spec
  :: forall action f item m
  . Control.Alternative.Plus f
  => Data.Foldable.Foldable f
  => Eq item
  => Effect.Aff.Class.MonadAff m
  => CompositeComponentRender action f item m
  -> Spec action f item m
spec embeddedRender =
  Select.defaultSpec
  { render = embeddedRender
  , handleAction = embeddedHandleAction
  , handleQuery = embeddedHandleQuery
  , handleEvent = embeddedHandleMessage
  , initialize = embeddedInitialize
  }

spinner :: ∀ p i. Halogen.HTML.HTML p i
spinner = Ocelot.Block.Loading.spinner [ Ocelot.HTML.Properties.css "w-6 text-blue-88" ]

synchronize
  :: forall action f item m
  . Eq item
  => Effect.Aff.Class.MonadAff m
  => CompositeComponentM action f item m Unit
synchronize = do
  st <- Halogen.get
  case getNewItems st of
    Network.RemoteData.Success items -> do
      Halogen.modify_ _ { fuzzyItems = items }
    Network.RemoteData.Failure _ -> do
      Halogen.modify_
        _ { visibility = Select.Off
          , fuzzyItems = []
          }
    Network.RemoteData.NotAsked -> do
      Halogen.modify_
        _ { visibility = Select.Off
          , fuzzyItems = []
          }
    Network.RemoteData.Loading -> do
      Halogen.modify_ _ { fuzzyItems = [] }
