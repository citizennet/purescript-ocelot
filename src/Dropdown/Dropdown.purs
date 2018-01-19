module CN.UI.Dropdown where

import Prelude

import Data.Array ((:), difference, mapWithIndex)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Select.Dispatch (ContainerQuery(..), ContainerState, Dispatch(..), emit, getContainerProps, getItemProps, getToggleProps)
import Select.Effects (FX)
import Select.Primitive.Container as C

----------
-- Item types

type DropdownItem = String

data SelectedStatus
  = Selected
  | NotSelected

data SelectableStatus
  = Selectable
  | NotSelectable


----------
-- Component types

-- Component state definition
type State item e =
  { items :: Array item
  , selections :: Array item
  , itemHTML :: item -> Array (H.HTML Void (ChildQuery item e))
  }

-- Component query definition
data Query item e a
  = HandleContainer (C.Message item (Query item e) e) a
  | Receiver (DropdownInput item e) a

-- Component top-level definition
type DropdownComponent item e
  = H.Component HH.HTML (Query item e) (DropdownInput item e) DropdownMessage (FX e)

-- Component input and message types
type DropdownInput item e =
  { items :: Array item
  , itemHTML :: item -> Array (H.HTML Void (ChildQuery item e))
  }

type DropdownMessage = Void

-- Component child types
type ChildQuery item e = Dispatch item (Query item e) e
type ChildSlot = Unit

-- Return type of render function; must use Dispatch as child type (or coproduct)
type DropdownHTML item e =
  H.ParentHTML (Query item e) (ChildQuery item e) ChildSlot (FX e)

-- Return type of eval function
type DropdownDSL item e =
  H.ParentDSL (State item e) (Query item e) (ChildQuery item e) ChildSlot DropdownMessage (FX e)



----------
-- Component definition
component :: ∀ item e. Eq item => DropdownComponent item e
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Receiver
    }
  where
    initialState :: DropdownInput item e -> State item e
    initialState input = { items: input.items, itemHTML: input.itemHTML, selections: [] }

    render :: State item e -> DropdownHTML item e
    render st =
      HH.div_
        [ HH.slot
            unit
            C.component
            { items: st.items, render: renderContainer st.itemHTML }
            ( HE.input HandleContainer )
        ]

    eval :: (Query item e) ~> (DropdownDSL item e)
    eval = case _ of

      Receiver input a -> a <$ do
        H.put $ initialState input

      HandleContainer m a -> case m of
        C.Emit q -> emit eval q a

        C.ItemSelected item -> do
          H.modify \st -> st { selections = item : st.selections }

          st <- H.get
          _ <- H.query unit
            $ H.action
            $ Container
            $ ContainerReceiver
            $ { render: renderContainer st.itemHTML, items: difference st.items st.selections }

          pure a


----------
-- Render helpers

-- Render the dropdown
renderContainer :: ∀ item e. (item -> Array (H.HTML Void (ChildQuery item e))) -> (ContainerState item) -> H.HTML Void (ChildQuery item e)
renderContainer itemHTML st =
  HH.div_
  $ if not st.open
    then [ renderToggle ]
    else [ renderToggle, renderItems $ renderItem `mapWithIndex` st.items ]

  where

    -- The clickable region that opens the dropdown
    renderToggle :: H.HTML Void (ChildQuery item e)
    renderToggle =
      HH.span
      ( getToggleProps [] )
      [ HH.text "Toggle" ]

    -- The individual items to render
    renderItems :: Array (H.HTML Void (ChildQuery item e)) -> H.HTML Void (ChildQuery item e)
    renderItems html =
      HH.div
      ( getContainerProps [] )
      [ HH.ul_ html ]

    -- One particular item to render
    renderItem :: Int -> item -> H.HTML Void (ChildQuery item e)
    renderItem index item =
      HH.li ( getItemProps index [] ) $ itemHTML item
