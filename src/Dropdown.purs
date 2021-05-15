module Ocelot.Dropdown 
  ( Action
  , ButtonBlock
  , ChildSlots
  , Component
  , ComponentHTML
  , ComponentM
  , ComponentRender
  , CompositeAction
  , CompositeComponent
  , CompositeComponentHTML
  , CompositeComponentM
  , CompositeComponentRender
  , CompositeInput
  , CompositeQuery
  , CompositeState
  , EmbeddedAction(..)
  , EmbeddedChildSlots
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , Spec
  , State
  , StateRow
  , StateStore
  , component
  , defDropdown
  ) where

import Prelude
import Control.Comonad as Control.Comonad
import Control.Comonad.Store as Control.Comonad.Store
import DOM.HTML.Indexed as DOM.HTML.Indexed
import Data.Array ((!!))
import Data.Array as Data.Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Effect.Aff.Class as Effect.Aff.Class
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.ItemContainer as Ocelot.Block.ItemContainer
import Ocelot.HTML.Properties ((<&>))
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Renderless.State as Renderless.State
import Select as Select
import Select.Setters as Select.Setters
import Type.Proxy (Proxy(..))

--------
-- Types

data Action item m
  = PassingOutput (Output item)
  | ReceiveRender (Input item m)

type ButtonBlock p i
  = Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLbutton i)
  -> Array (Halogen.HTML.HTML p i)
  -> Halogen.HTML.HTML p i

type ChildSlots item 
  = ( select :: Select.Slot (Query item) EmbeddedChildSlots (Output item) Unit
    )

type Component item m 
  = Halogen.Component (Query item) (Input item m) (Output item) m

type ComponentHTML item m 
  = Halogen.ComponentHTML (Action item m) (ChildSlots item) m

type ComponentM item m a 
  = Halogen.HalogenM (StateStore item m) (Action item m) (ChildSlots item) (Output item) m a

type ComponentRender item m 
  = State item -> ComponentHTML item m

type CompositeAction
  = Select.Action EmbeddedAction

type CompositeComponent item m 
  = Halogen.Component (CompositeQuery item) (CompositeInput item) (Output item) m

type CompositeComponentHTML m 
  = Halogen.ComponentHTML CompositeAction EmbeddedChildSlots m

type CompositeComponentM item m a 
  = Halogen.HalogenM (CompositeState item) CompositeAction EmbeddedChildSlots (Output item) m a

type CompositeComponentRender item m 
  = (CompositeState item) -> CompositeComponentHTML m

type CompositeInput item 
  = Select.Input (StateRow item)

type CompositeQuery item 
  = Select.Query (Query item) EmbeddedChildSlots

type CompositeState item 
  = Select.State (StateRow item)

type EmbeddedAction = Void

type EmbeddedChildSlots = () -- NOTE no extension

type Input item m =
  { disabled :: Boolean
  , items :: Array item
  , render :: CompositeComponentRender item m
  , selectedItem :: Maybe item
  }

data Output item
  = Selected item
  | VisibilityChanged Select.Visibility

data Query item a
  = SetDisabled Boolean a
  | SetItems (Array item) a
  | SetSelection (Maybe item) a

type Slot item id = Halogen.Slot (Query item) (Output item) id

type Spec item m 
  = Select.Spec (StateRow item) (Query item) EmbeddedAction EmbeddedChildSlots (CompositeInput item) (Output item) m

type State item = Record (StateRow item)

type StateRow item =
  ( disabled :: Boolean
  , items :: Array item
  , selectedItem :: Maybe item
  )

type StateStore item m = Control.Comonad.Store.Store (State item) (ComponentHTML item m)

------------
-- Component

component
  :: forall item m
  . Effect.Aff.Class.MonadAff m
  => Component item m
component = Halogen.mkComponent
  { initialState
  , render: Control.Comonad.extract
  , eval: Halogen.mkEval Halogen.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

---------
-- Values

_select = Proxy :: Proxy "select"

defDropdown
  :: ∀ item m
  . Eq item
  => (∀ p i. ButtonBlock p i)
  -> Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLbutton CompositeAction)
  -> (item -> String)
  -> String
  -> CompositeComponentRender item m
defDropdown button props toString label st =
  Halogen.HTML.div [ Ocelot.HTML.Properties.css "relative" ] [ toggle, menu ]
  where
    toggle =
      Ocelot.Block.ItemContainer.dropdownButton
        button
        (toggleProps st.disabled props)
        [ Halogen.HTML.text $ Data.Maybe.maybe label toString st.selectedItem ]

    menu = Halogen.HTML.div
      [ Halogen.HTML.Properties.classes containerClasses ]
      [ Ocelot.Block.ItemContainer.dropdownContainer
        []
        (Halogen.HTML.text <<< toString)
        ((==) st.selectedItem <<< Just)
        st.items
        st.highlightedIndex
      ]

    containerClasses = case st.visibility of
      Select.Off -> [ Halogen.HTML.ClassName "invisible" ]
      Select.On -> []

-- Embedded > handleMessage
embeddedHandleMessage
  :: forall item m. Effect.Aff.Class.MonadAff m => Select.Event -> CompositeComponentM item m Unit
embeddedHandleMessage = case _ of
  Select.Selected idx -> do
    { items } <- Halogen.get
    case items !! idx of
      Nothing -> pure unit
      Just item -> do
        Halogen.modify_
          _ { visibility = Select.Off
            , selectedItem = Just item
            }
        Halogen.raise $ Selected item
  Select.VisibilityChanged vis -> Halogen.raise (VisibilityChanged vis)
  _ -> pure unit

-- Embedded > handleQuery
embeddedHandleQuery
  :: forall item m a. Effect.Aff.Class.MonadAff m => Query item a -> CompositeComponentM item m (Maybe a)
embeddedHandleQuery = case _ of
  SetDisabled disabled a -> Just a <$ do
    Halogen.modify_ _ { disabled = disabled }
  SetItems items a -> Just a <$ do
    Halogen.modify_ _ { items = items }
  SetSelection item a -> Just a <$ do
    Halogen.modify_ _ { selectedItem = item }

-- NOTE configure Select
embeddedInput :: forall item. State item -> CompositeInput item
embeddedInput { disabled, items, selectedItem } =
  { debounceTime: Nothing
  , disabled
  , getItemCount: Data.Array.length <<< _.items
  , inputType: Select.Toggle
  , items
  , search: Nothing
  , selectedItem
  }

-- NOTE re-raise output messages from the embedded component
-- NOTE update Dropdown render function if it relies on external state
handleAction :: forall item m. Effect.Aff.Class.MonadAff m => Action item m -> ComponentM item m Unit
handleAction = case _ of
  PassingOutput output ->
    Halogen.raise output
  ReceiveRender { render } -> do
    Renderless.State.modifyStore_ (renderAdapter render) identity

-- NOTE passing query to the embedded component
handleQuery :: forall item m a. Query item a -> ComponentM item m (Maybe a)
handleQuery = case _ of
  SetDisabled disabled a -> Just a <$ do
    Halogen.tell _select unit (Select.Query <<< SetDisabled disabled)
  SetItems items a -> Just a <$ do
    Halogen.tell _select unit (Select.Query <<< SetItems items)
  SetSelection item a -> Just a <$ do
    Halogen.tell _select unit (Select.Query <<< SetSelection item)

initialState :: forall item m. Effect.Aff.Class.MonadAff m => Input item m -> StateStore item m
initialState { disabled, items, render, selectedItem } =
  Control.Comonad.Store.store (renderAdapter render) { disabled, items, selectedItem }

renderAdapter
  :: forall item m
  . Effect.Aff.Class.MonadAff m
  => CompositeComponentRender item m
  -> ComponentRender item m
renderAdapter render state =
  Halogen.HTML.slot _select unit (Select.component identity $ spec render)
    (embeddedInput state)
    (Just <<< PassingOutput)

spec
  :: forall item m
  . Effect.Aff.Class.MonadAff m
  => CompositeComponentRender item m
  -> Spec item m
spec embeddedRender =
  Select.defaultSpec
  { render = embeddedRender
  , handleQuery = embeddedHandleQuery
  , handleEvent = embeddedHandleMessage
  }

toggleProps
  :: Boolean
  -> Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLbutton CompositeAction)
  -> Array (Halogen.HTML.Properties.IProp DOM.HTML.Indexed.HTMLbutton CompositeAction)
toggleProps disabled iprops = if disabled
  then iprops'
  else Select.Setters.setToggleProps iprops'
  where
  iprops' = 
    [ Halogen.HTML.Properties.disabled disabled
    ] 
    <&> iprops