module Ocelot.Component.Dropdown.Render where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
import Ocelot.Component.Dropdown as DD
import Ocelot.HTML.Properties ((<&>))
import Select as Select
import Select.Utils.Setters (setContainerProps, setItemProps, setToggleProps)

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
defDropdown button props toString label state selectState =
  HH.div [ HP.class_ $ HH.ClassName "relative" ] [ toggle, menu ]

  where
    toggle =
      button
        ( setToggleProps
          [ HP.class_ $ HH.ClassName "font-medium flex items-center" ] <&> props )
        [ HH.text $ maybe label toString state.selectedItem
        , HH.div
          [ HP.class_ $ HH.ClassName "ml-3 text-xs" ]
          [ Icon.caratDown_ ]
        ]

    menu =
      HH.ul
        ( setContainerProps [ HP.classes containerClasses ] )
        ( mapWithIndex renderItem selectState.items )

    containerClasses = HH.ClassName <$> case selectState.visibility of
      Select.Off -> [ "invisible" ] <> containerClasses'
      Select.On -> containerClasses'

    containerClasses' =
      [ "bg-white"
      , "text-black-20"
      , "border"
      , "cursor-pointer"
      , "list-reset"
      , "py-2"
      , "rounded"
      , "shadow"
      , "absolute"
      , "pin-t"
      , "z-60"
      ]

    renderItem idx item =
      HH.li
        itemProps
        [ Icon.selected
          [ HP.classes $ HH.ClassName <$> ([ "mr-2", "text-green" ] <> checkmarkClass) ]
        , HH.text (toString item)
        ]
      where

        itemProps =
          setItemProps idx [ HP.classes itemClasses ]

        itemClasses =
          HH.ClassName <$> (itemClasses' <> highlightClass <> selectedClass)

        itemClasses' =
          [ "px-4"
          , "py-2"
          ]

        highlightClass
          | Just idx == selectState.highlightedIndex = [ "bg-grey-97" ]
          | otherwise = []

        selectedClass
          | Just item == state.selectedItem = [ "font-medium" ]
          | otherwise = []

        checkmarkClass
          | Just item == state.selectedItem = []
          | otherwise = [ "invisible" ]

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
