module Ocelot.Components.Dropdown.Component where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Array ((!!))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Renderless.State (modifyStore_)
import Select as S
import Type.Data.Symbol (SProxy(..))


type Slot item id = H.Slot (Query item) (Output item) id

type Component item m = H.Component HH.HTML (Query item) (Input item m) (Output item) m
type ComponentHTML item m = H.ComponentHTML (Action item m) (ChildSlots item) m
type ComponentRender item m = State item -> ComponentHTML item m
type ComponentM item m a = H.HalogenM (StateStore item m) (Action item m) (ChildSlots item) (Output item) m a

type StateRow item =
  ( selectedItem :: Maybe item
  , items :: Array item
  )

type State item = Record (StateRow item)

type StateStore item m = Store (State item) (ComponentHTML item m)

type Input item m =
  { selectedItem :: Maybe item
  , items :: Array item

  , render :: CompositeComponentRender item m
  }

data Action item m
  = PassingOutput (Output item)
  | ReceiveRender (Input item m)

type EmbeddedAction = Void

data Query item a
  = SetItems (Array item) a
  | SetSelection (Maybe item) a

data Output item
  = Selected item
  | VisibilityChanged S.Visibility

type ChildSlots item =
  ( select :: S.Slot (Query item) EmbeddedChildSlots (Output item) Unit
  )
_select = SProxy :: SProxy "select"


type CompositeState item = S.State (StateRow item)
type CompositeAction = S.Action EmbeddedAction
type CompositeQuery item = S.Query (Query item) EmbeddedChildSlots
type CompositeInput item = S.Input (StateRow item)
type EmbeddedChildSlots = () -- NOTE no extension

type Spec item m = S.Spec (StateRow item) (Query item) EmbeddedAction EmbeddedChildSlots (CompositeInput item) (Output item) m
type CompositeComponent item m = H.Component HH.HTML (CompositeQuery item) (CompositeInput item) (Output item) m
type CompositeComponentHTML m = H.ComponentHTML CompositeAction EmbeddedChildSlots m
type CompositeComponentRender item m = (CompositeState item) -> CompositeComponentHTML m
type CompositeComponentM item m a = H.HalogenM (CompositeState item) CompositeAction EmbeddedChildSlots (Output item) m a


------------
-- Container

component
  :: forall item m
  . MonadAff m
  => Component item m
component = H.mkComponent
  { initialState
  , render: extract
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

initialState :: forall item m. MonadAff m => Input item m -> StateStore item m
initialState { render, selectedItem, items } =
  store (renderAdapter render) { selectedItem, items }

renderAdapter
  :: forall item m
  . MonadAff m
  => CompositeComponentRender item m
  -> ComponentRender item m
renderAdapter render state =
  HH.slot _select unit (S.component identity $ spec render)
    (embeddedInput state)
    (Just <<< PassingOutput)

spec
  :: forall item m
  . MonadAff m
  => CompositeComponentRender item m
  -> Spec item m
spec embeddedRender =
  S.defaultSpec
  { render = embeddedRender
  , handleQuery = embeddedHandleQuery
  , handleEvent = embeddedHandleMessage
  }

-- NOTE configure Select
embeddedInput :: forall item. State item -> CompositeInput item
embeddedInput { selectedItem, items } =
  { inputType: S.Toggle
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: Array.length <<< _.items

  , selectedItem
  , items
  }

-- NOTE re-raise output messages from the embedded component
-- NOTE update Dropdown render function if it relies on external state
handleAction :: forall item m. MonadAff m => Action item m -> ComponentM item m Unit
handleAction = case _ of
  PassingOutput output ->
    H.raise output

  ReceiveRender { render } -> do
    modifyStore_ (renderAdapter render) identity

-- NOTE passing query to the embedded component
handleQuery :: forall item m a. Query item a -> ComponentM item m (Maybe a)
handleQuery = case _ of
  SetItems items a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ SetItems items)

  SetSelection item a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ SetSelection item)

-------------------------
-- Embedded > handleQuery

embeddedHandleQuery
  :: forall item m a. MonadAff m => Query item a -> CompositeComponentM item m (Maybe a)
embeddedHandleQuery = case _ of
  SetItems items a -> Just a <$ do
    H.modify_ _ { items = items }

  SetSelection item a -> Just a <$ do
    H.modify_ _ { selectedItem = item }

---------------------------
-- Embedded > handleMessage

embeddedHandleMessage
  :: forall item m. MonadAff m => S.Event -> CompositeComponentM item m Unit
embeddedHandleMessage = case _ of
  S.Selected idx -> do
    items <- H.gets _.items
    case items !! idx of
      Nothing -> pure unit
      Just item -> do
        H.modify_
          _ { visibility = S.Off
            , selectedItem = Just item
            }
        H.raise $ Selected item

  S.VisibilityChanged vis -> H.raise (VisibilityChanged vis)

  _ -> pure unit
