module CN.UI.Dropdown where

import Prelude
import Select.Dispatch

import Control.Monad.Aff.Console (log)
import Data.Array ((:), difference, mapWithIndex)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
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
type State =
  { items :: Array DropdownItem
  , selections :: Array DropdownItem }

-- Component query definition
data Query e a
  = HandleContainer (C.Message String (Query e) e) a

-- Component top-level definition
type DropdownComponent e
  = H.Component HH.HTML (Query e) DropdownInput DropdownMessage (FX e)

-- Component input and message types
type DropdownInput = Unit
type DropdownMessage = Void

-- Component child types
type ChildQuery e = Dispatch DropdownItem (Query e) e
type ChildSlot = Unit

-- Return type of render function; must use Dispatch as child type (or coproduct)
type DropdownHTML e =
  H.ParentHTML (Query e) (ChildQuery e) DropdownInput (FX e)

-- Return type of eval function
type DropdownDSL e =
  H.ParentDSL State (Query e) (ChildQuery e) ChildSlot DropdownMessage (FX e)



----------
-- Component definition
component :: ∀ e. DropdownComponent e
component =
  H.parentComponent
    { initialState: const { items: [], selections: [] }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> DropdownHTML e
    render st =
      HH.div_
      [ HH.slot
          unit
          C.component
          { items: [], render: renderContainer }
          ( HE.input HandleContainer )
      ]

    eval :: (Query e) ~> (DropdownDSL e)
    eval = case _ of

      HandleContainer m a -> case m of
        C.Emit q -> emit eval q a

        C.ItemSelected item -> do
           H.liftAff $ log ("Selected: " <> item)
           H.modify \st -> st { selections = item : st.selections }

           st <- H.get
           _ <- H.query unit
                  $ H.action
                  $ Container
                  $ ContainerReceiver
                  $ { render: renderContainer, items: difference st.items st.selections }

           pure a


----------
-- Render helpers

-- Render the dropdown
renderContainer :: ∀ e. (ContainerState String) -> H.HTML Void (ChildQuery e)
renderContainer st =
  HH.div_
  $ if not st.open
    then [ renderToggle ]
    else [ renderToggle, renderItems $ renderItem `mapWithIndex` st.items ]

  where

    -- The clickable region that opens the dropdown
    renderToggle :: H.HTML Void (ChildQuery e)
    renderToggle =
      HH.span
      ( getToggleProps [] )
      [ HH.text "Toggle" ]

    -- The individual items to render
    renderItems :: Array (H.HTML Void (ChildQuery e)) -> H.HTML Void (ChildQuery e)
    renderItems html =
      HH.div
      ( getContainerProps [] )
      [ HH.ul_ html ]

    -- One particular item to render
    renderItem :: Int -> DropdownItem -> H.HTML Void (ChildQuery e)
    renderItem index item =
      HH.li ( getItemProps index [] ) [ HH.text item ]
