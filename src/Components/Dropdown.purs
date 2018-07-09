module Ocelot.Components.Dropdown where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Icon as Icon
import Select as Select
import Select.Utils.Setters (setContainerProps, setItemProps, setToggleProps)

data Query item a
  = HandleSelect (Select.Message (Query item) item) a
  | Receive (Input item) a
  | SetItems (Array item) a

type State item =
  { selectedItem :: Maybe item
  , items :: Array item
  , label :: String
  , toString :: item -> String
  , disabled :: Boolean
  }

type Input item = State item

data Message item = ItemSelected item

type ChildSlot = Unit

type ChildQuery item = Select.Query (Query item) item

type ButtonFn p i
   = Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i

dropdown
  :: ∀ item m
   . MonadAff m
  => Eq item
  => H.Component HH.HTML (Query item) (Input item) (Message item) m
dropdown = component Button.button

dropdownDark
  :: ∀ item m
   . MonadAff m
  => Eq item
  => H.Component HH.HTML (Query item) (Input item) (Message item) m
dropdownDark = component Button.buttonDark

component
  :: ∀ m item
   . MonadAff m
  => Eq item
  => (∀ p i. ButtonFn p i)
  -> H.Component HH.HTML (Query item) (Input item) (Message item) m
component button =
  H.parentComponent
    { initialState: identity
    , render
    , eval
    , receiver: HE.input Receive
    }

  where
    eval
      :: Query item
      ~> H.ParentDSL
          (State item)
          (Query item)
          (ChildQuery item)
          (ChildSlot)
          (Message item)
          m
    eval = case _ of
      HandleSelect message a -> case message of
        Select.Selected item -> do
          _ <- H.query unit $ Select.setVisibility Select.Off
          H.modify_ _ { selectedItem = Just item }
          H.raise $ ItemSelected item
          pure a
        _ -> pure a
      Receive input a -> do
        H.put input
        pure a
      SetItems items a -> do
        H.modify_ _ { items = items }
        pure a

    render
      :: State item
      -> H.ParentHTML (Query item) (ChildQuery item) ChildSlot m
    render state =
      HH.slot unit Select.component selectInput (HE.input HandleSelect)

      where
        selectInput :: Select.Input (Query item) item
        selectInput =
          { debounceTime: Nothing
          , initialSearch: Nothing
          , inputType: Select.Toggle
          , items: state.items
          , render: renderDropdown
          }

          where
            renderDropdown selectState =
              HH.div [ HP.class_ $ HH.ClassName "relative" ] [ toggle, menu ]

              where
                toggle =
                  button
                    ( setToggleProps
                      [ HP.disabled state.disabled
                      , HP.class_ $ HH.ClassName "font-medium flex items-center" ] )
                    [ HH.text $ maybe state.label state.toString state.selectedItem
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
                  HH.li itemProps [HH.text (state.toString item)]

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
