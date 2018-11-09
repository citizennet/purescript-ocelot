module Ocelot.Component.Dropdown.Render where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.ItemContainer as IC
import Ocelot.Component.Dropdown as DD
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Setters (setToggleProps)

type ButtonFn p i
   = Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i

defDropdown
  :: ∀ o item
   . Eq item
  => (∀ p i. ButtonFn p i)
  -> Array (H.IProp HTMLbutton (Select.Query o item))
  -> (item -> String)
  -> String
  -> DD.State item
  -> Select.State item
  -> H.ComponentHTML (Select.Query o item)
defDropdown button props toString label pst cst =
  HH.div [ css "relative" ] [ toggle, menu ]

  where
    toggle = IC.dropdownButton
      button (setToggleProps props) [ HH.text $ maybe label toString pst.selectedItem ]

    menu = HH.div
      [ HP.classes containerClasses ]
      [ IC.dropdownContainer
        [] (HH.text <<< toString) ((==) pst.selectedItem <<< Just) cst.items cst.highlightedIndex
      ]

    containerClasses = case cst.visibility of
      Select.Off -> [ HH.ClassName "invisible" ]
      Select.On -> []

render
  :: ∀ o item m
   . MonadAff m
  => (DD.State item -> Select.State item -> H.ComponentHTML (Select.Query o item))
  -> DD.State item
  -> H.ParentHTML (DD.Query o item m) (DD.ChildQuery o item) DD.ChildSlot m
render renderDropdown state =
  HH.slot unit Select.component selectInput (HE.input DD.HandleSelect)

  where
    selectInput :: Select.Input o item
    selectInput =
      { debounceTime: Nothing
      , initialSearch: Nothing
      , inputType: Select.Toggle
      , items: state.items
      , render: renderDropdown state
      }
