module Ocelot.Typeahead.Base where

import Prelude
import Control.Alternative (class Plus, empty)
import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Array ((!!), (:))
import Data.Array as Array
import Data.Fuzzy (Fuzzy(..))
import Data.Fuzzy as Fuzz
import Data.Maybe (Maybe(..), maybe)
import Data.Rational ((%))
import Data.Time.Duration (Milliseconds)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))
import Renderless.State (modifyStore_)
import Select as S
import Type.Data.Symbol (SProxy(..))


-------------
-- Components

single
  :: forall action item m
  . Eq item
  => MonadAff m
  => Component action Maybe item m
single = component
  { runSelect: const <<< Just
  , runRemove: const (const Nothing)
  , runFilter: \items -> maybe items (\i -> Array.filter (_ /= i) items)
  }

multi
  :: forall action item m
  . Eq item
  => MonadAff m
  => Component action Array item m
multi = component
  { runSelect: (:)
  , runRemove: Array.filter <<< (/=)
  , runFilter: Array.difference
  }

--------
-- Types

type Slot action f item id = H.Slot (Query f item) (Output action f item) id

type Component action f item m = H.Component HH.HTML (Query f item) (Input action f item m) (Output action f item) m
type ComponentHTML action f item m = H.ComponentHTML (Action action f item m) (ChildSlots action f item) m
type ComponentRender action f item m = State f item m -> ComponentHTML action f item m
type ComponentM action f item m a = H.HalogenM (StateStore action f item m) (Action action f item m) (ChildSlots action f item) (Output action f item) m a

type StateRow f item m =
  ( items :: RemoteData String (Array item) -- NOTE pst.items, Parent(Typeahead)
  , insertable :: Insertable item
  , keepOpen :: Boolean
  , itemToObject :: item -> Object String
  , async :: Maybe (String -> m (RemoteData String (Array item)))
  , disabled :: Boolean

  , ops :: Operations f item
  , config :: { debounceTime :: Maybe Milliseconds }
  , selected :: f item
  , fuzzyItems :: Array (Fuzzy item) -- NOTE cst.items, Child(Select)
  )

type State f item m = Record (StateRow f item m)

type StateStore action f item m = Store (State f item m) (ComponentHTML action f item m)

type Input action f item m =
  { items :: RemoteData String (Array item)
  , insertable :: Insertable item
  , keepOpen :: Boolean
  , itemToObject :: item -> Object String
  , async :: Maybe (String -> m (RemoteData String (Array item)))
  , disabled :: Boolean

  , debounceTime :: Maybe Milliseconds
  , render :: CompositeComponentRender action f item m
  }

data Action action (f :: Type -> Type) item (m :: Type -> Type)
  = PassingOutput (Output action f item)
  | ReceiveRender (Input action f item m)

data EmbeddedAction action (f :: Type -> Type) item (m :: Type -> Type)
  = Initialize
  | Remove item
  | RemoveAll
  | Raise action

data Query f item a
  = GetSelected (f item -> a)
  | ReplaceSelected (f item) a
  | ReplaceSelectedBy (Array item -> f item) a
  | ReplaceItems (RemoteData String (Array item)) a
  | Reset a
  | SetDisabled Boolean a

data Output action (f :: Type -> Type) item
  = Searched String
  | Selected item
  | SelectionChanged SelectionCause (f item)
  | Emit action

type ChildSlots action f item =
  ( select :: S.Slot (Query f item) EmbeddedChildSlots (Output action f item) Unit
  )
_select = SProxy :: SProxy "select"

type CompositeState f item m = S.State (StateRow f item m)
type CompositeAction action f item m = S.Action (EmbeddedAction action f item m)
type CompositeQuery f item = S.Query (Query f item) EmbeddedChildSlots
type CompositeInput f item m = S.Input (StateRow f item m)
type EmbeddedChildSlots = () -- NOTE no extension

type Spec action f item m = S.Spec (StateRow f item m) (Query f item) (EmbeddedAction action f item m) EmbeddedChildSlots (CompositeInput f item m) (Output action f item) m
type CompositeComponent action f item m = H.Component HH.HTML (CompositeQuery f item) (CompositeInput f item m) (Output action f item) m
type CompositeComponentHTML action f item m = H.ComponentHTML (CompositeAction action f item m) EmbeddedChildSlots m
type CompositeComponentRender action f item m = (CompositeState f item m) -> CompositeComponentHTML action f item m
type CompositeComponentM action f item m a = H.HalogenM (CompositeState f item m) (CompositeAction action f item m) EmbeddedChildSlots (Output action f item) m a

-------
-- Data

data SelectionCause
  = RemovalQuery
  | ReplacementQuery
  | ResetQuery
  | SelectionMessage
derive instance eqSelectionCause :: Eq SelectionCause

type Operations f item =
  { runSelect  :: item -> f item -> f item
  , runRemove  :: item -> f item -> f item
  , runFilter  :: Array item -> f item -> Array item
  }

data Insertable item
  = NotInsertable
  | Insertable (String -> item)

------------
-- Container

component
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => Operations f item
  -> Component action f item m
component ops = H.mkComponent
  { initialState: initialState ops
  , render: extract
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

initialState
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => Operations f item
  -> Input action f item m
  -> StateStore action f item m
initialState ops
  { items, insertable, keepOpen, itemToObject, async, debounceTime, render, disabled }
  = store (renderAdapter render)
      { items
      , insertable
      , keepOpen
      , itemToObject
      , async
      , disabled

      , ops
      , config: {debounceTime}
      , selected: empty :: f item
      , fuzzyItems: []
      }

renderAdapter
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => CompositeComponentRender action f item m
  -> ComponentRender action f item m
renderAdapter render state =
  HH.slot _select unit (S.component identity $ spec render)
    (embeddedInput state)
    (Just <<< PassingOutput)

spec
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => CompositeComponentRender action f item m
  -> Spec action f item m
spec embeddedRender =
  S.defaultSpec
  { render = embeddedRender
  , handleAction = embeddedHandleAction
  , handleQuery = embeddedHandleQuery
  , handleEvent = embeddedHandleMessage
  , initialize = embeddedInitialize
  }

-- NOTE configure Select
embeddedInput :: forall f item m. State f item m -> CompositeInput f item m
embeddedInput { items, selected, insertable, keepOpen, itemToObject, ops, async, fuzzyItems, config: { debounceTime }, disabled } =
  { inputType: S.Text
  , search: Nothing
  , debounceTime
  , getItemCount: Array.length <<< _.fuzzyItems

  , items
  , selected
  , insertable
  , keepOpen
  , itemToObject
  , ops
  , async
  , fuzzyItems
  , disabled

  , config: { debounceTime } -- NOTE overhead
  }

-- NOTE re-raise output messages from the embedded component
-- NOTE update Dropdown render function if it relies on external state
handleAction
  :: forall action f item m
  . Plus f
  => Eq item
  => MonadAff m
  => Action action f item m
  -> ComponentM action f item m Unit
handleAction = case _ of
  PassingOutput output ->
    H.raise output
  ReceiveRender { render } -> do
    modifyStore_ (renderAdapter render) identity

-- NOTE passing query to the embedded component
handleQuery :: forall action f item m a. Query f item a -> ComponentM action f item m (Maybe a)
handleQuery = case _ of
  GetSelected reply -> do
    response <- H.query _select unit (S.Query $ H.request GetSelected)
    pure $ reply <$> response
  ReplaceSelected selected a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ ReplaceSelected selected)
  ReplaceSelectedBy f a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ ReplaceSelectedBy f)
  ReplaceItems items a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ ReplaceItems items)
  Reset a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ Reset)
  SetDisabled disabled a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ SetDisabled disabled)

------------------
-- Embedded > Util

getNewItems
  :: forall f item m
  . MonadAff m
  => Eq item
  => CompositeState f item m
  -> RemoteData String (Array (Fuzzy item))
getNewItems st =
  Array.sort
  <<< applyF
  <<< applyI
  <<< fuzzyItems
  <$> (map (flip st.ops.runFilter st.selected) st.items)
  where
    matcher :: item -> Fuzzy item
    matcher = Fuzz.match true st.itemToObject st.search

    fuzzyItems :: Array item -> Array (Fuzzy item)
    fuzzyItems = map matcher

    applyI :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyI = applyInsertable matcher st.insertable st.search

    applyF :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyF = Array.filter (\(Fuzzy { ratio }) -> ratio > (2 % 3))

applyInsertable
  :: forall item
  . (item -> Fuzzy item)
  -> Insertable item
  -> String
  -> Array (Fuzzy item)
  -> Array (Fuzzy item)
applyInsertable _ _ "" items = items
applyInsertable match insertable text items = case insertable of
  NotInsertable -> items
  Insertable mkItem | Array.length (Array.filter isExactMatch items) > 0 -> items
                    | otherwise -> (match $ mkItem text) : items
  where
    isExactMatch (Fuzzy { distance }) = distance == Fuzz.Distance 0 0 0 0 0 0

synchronize
  :: forall action f item m
  . Eq item
  => MonadAff m
  => CompositeComponentM action f item m Unit
synchronize = do
  st <- H.get
  case getNewItems st of
    Success items -> do
      H.modify_ _ { fuzzyItems = items }
    Failure err -> do
      H.modify_
        _ { visibility = S.Off
          , fuzzyItems = []
          }
    NotAsked -> do
      H.modify_
        _ { visibility = S.Off
          , fuzzyItems = []
          }
    Loading -> do
      H.modify_ _ { fuzzyItems = [] }

replaceSelected
  :: forall action f item m
  . Eq item
  => MonadAff m
  => f item
  -> CompositeComponentM action f item m Unit
replaceSelected selected = do
  st <- H.modify _ { selected = selected }
  H.raise $ SelectionChanged ReplacementQuery st.selected
  synchronize

--------------------------
-- Embedded > handleAction

embeddedHandleAction
  :: forall action f item m
  . Eq item
  => Plus f
  => MonadAff m
  => EmbeddedAction action f item m
  -> CompositeComponentM action f item m Unit
embeddedHandleAction = case _ of
  Initialize -> do
    synchronize
  Remove item -> do
    st <- H.modify \st -> st { selected = st.ops.runRemove item st.selected }
    H.raise $ SelectionChanged RemovalQuery st.selected
    synchronize
  RemoveAll -> do
    st <- H.modify \st ->
      st { selected = empty :: f item
         , visibility = S.Off
         }
    H.raise $ SelectionChanged RemovalQuery st.selected
    synchronize
  Raise action -> do
    H.raise $ Emit action

-------------------------
-- Embedded > handleQuery

embeddedHandleQuery
  :: forall action f item m a
  . Plus f
  => Eq item
  => MonadAff m
  => Query f item a
  -> CompositeComponentM action f item m (Maybe a)
embeddedHandleQuery = case _ of
  GetSelected reply -> do
    { selected } <- H.get
    pure $ Just $ reply selected

  ReplaceSelected selected a -> Just a <$ do
    replaceSelected selected

  ReplaceSelectedBy f a -> Just a <$ do
    { items } <- H.get
    case items of
      Success items' -> replaceSelected (f items')
      _ -> pure unit

  ReplaceItems items a -> Just a <$ do
    H.modify_ _ { items = items }
    synchronize

  Reset a -> Just a <$ do
    st <- H.modify _ { selected = empty :: f item, items = NotAsked }
    H.raise $ SelectionChanged ResetQuery st.selected
    synchronize

  SetDisabled disabled a -> Just a <$ do
    H.modify_ _ { disabled = disabled }

---------------------------
-- Embedded > handleMessage

embeddedHandleMessage
  :: forall action f item m
   . Eq item
  => MonadAff m
  => S.Event
  -> CompositeComponentM action f item m Unit
embeddedHandleMessage = case _ of
  S.Selected idx -> do
    { fuzzyItems } <- H.get
    case fuzzyItems !! idx of
      Nothing -> pure unit
      Just (Fuzzy { original: item }) -> do
        st <- H.modify \st -> st { selected = st.ops.runSelect item st.selected }
        when (not st.keepOpen) do
          H.modify_ _ { visibility = S.Off }
        H.raise $ SelectionChanged SelectionMessage st.selected
        H.raise $ Selected item
        synchronize

  -- Perform a new search, fetching data if Async.
  S.Searched text -> do
    H.modify_ _ { search = text }

    async <- H.gets _.async
    case async of
      Nothing -> pure unit
      Just fetchItems -> do
        H.modify_ _ { items = Loading }
        synchronize
        newItems <- H.lift $ fetchItems text
        H.modify_ _ { items = newItems }

    H.raise $ Searched text
    synchronize

  _ -> pure unit

------------------------
-- Embedded > initialize

embeddedInitialize :: forall action f item m. Maybe (EmbeddedAction action f item m)
embeddedInitialize = Just Initialize
