module Ocelot.Components.Dropdown.Render where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Data.Maybe (Maybe(..), maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.ItemContainer as IC
import Ocelot.HTML.Properties (css)
import Select as S
import Select.Setters as SS

import Ocelot.Components.Dropdown.Component (CompositeAction, CompositeComponentRender)

--------------------
-- Embedded > render

type ButtonBlock p i
  = Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i

defDropdown
  :: ∀ item m
  . Eq item
  => (∀ p i. ButtonBlock p i)
  -> Array (HP.IProp HTMLbutton CompositeAction)
  -> (item -> String)
  -> String
  -> CompositeComponentRender item m
defDropdown button props toString label st =
  HH.div [ css "relative" ] [ toggle, menu ]

  where
    toggle =
      IC.dropdownButton
        button
        (SS.setToggleProps props)
        [ HH.text $ maybe label toString st.selectedItem ]

    menu = HH.div
      [ HP.classes containerClasses ]
      [ IC.dropdownContainer
        []
        (HH.text <<< toString)
        ((==) st.selectedItem <<< Just)
        st.items
        st.highlightedIndex
      ]

    containerClasses = case st.visibility of
      S.Off -> [ HH.ClassName "invisible" ]
      S.On -> []
