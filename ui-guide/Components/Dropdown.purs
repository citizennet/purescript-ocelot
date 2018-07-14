module UIGuide.Components.Dropdown where

import Prelude

import Data.Array (mapWithIndex)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Format (caption_) as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Blocks.Choice as Choice
import Ocelot.Components.Dropdown as Dropdown
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Utils.Setters as SelectSetters
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a
  = HandleDropdown (Dropdown.Message String) a
  | HandleChoice (Select.Message Query Platform) a

type Input = Unit

type Message = Void

type ChildSlot = Either2 Unit Unit

type ChildQuery = Coproduct2 (Dropdown.Query String) (Select.Query Query Platform)

data Platform
  = Facebook
  | Twitter

component
  :: ∀ m
   . MonadAff m
  => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }

  where
    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery ChildSlot Message m
    eval = case _ of
      HandleDropdown message a -> case message of
        Dropdown.ItemSelected x -> do
          H.liftEffect (log x)
          H.modify_ identity
          pure a

      HandleChoice message a -> case message of
        Select.Selected x -> a <$ do
          H.liftEffect $ case x of
            Facebook -> log "Facebook"
            Twitter -> log "Twitter"
          H.query' CP.cp2 unit ( Select.setVisibility Select.Off )

        _ -> pure a

    render
      :: State
      -> H.ParentHTML Query ChildQuery ChildSlot m
    render state =
      HH.div_
        [ Documentation.block_
          { header: "Dropdown"
          , subheader: "A dropdown list of selectable items."
          }
          [ Backdrop.backdrop_
            [ Backdrop.content_
              [ HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Standard" ]
                , HH.slot'
                  CP.cp1
                  unit
                  Dropdown.dropdown
                  { selectedItem: Nothing
                  , items
                  , label: "Pick one"
                  , toString: identity
                  , disabled: false
                  }
                  ( HE.input HandleDropdown )
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.slot'
                  CP.cp1
                  unit
                  Dropdown.dropdown
                  { selectedItem: Just "Kilchoman Blue Label"
                  , items
                  , label: "Pick one"
                  , toString: identity
                  , disabled: true
                  }
                  ( HE.input HandleDropdown )
                ]
              ]
            ]
          , Backdrop.backdropWhite_
            [ Backdrop.content_
              [ HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Standard" ]
                , HH.slot'
                  CP.cp1
                  unit
                  Dropdown.dropdown
                  { selectedItem: Nothing
                  , items
                  , label: "Pick one"
                  , toString: identity
                  , disabled: false
                  }
                  ( HE.input HandleDropdown )
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.slot'
                  CP.cp1
                  unit
                  Dropdown.dropdown
                  { selectedItem: Just "Kilchoman Blue Label"
                  , items
                  , label: "Pick one"
                  , toString: identity
                  , disabled: true
                  }
                  ( HE.input HandleDropdown )
                ]
              ]
            ]
          , Backdrop.backdropDark_
            [ Backdrop.content_
              [ HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Standard" ]
                , HH.slot'
                  CP.cp1
                  unit
                  Dropdown.dropdownDark
                  { selectedItem: Nothing
                  , items
                  , label: "Pick one"
                  , toString: identity
                  , disabled: false
                  }
                  ( HE.input HandleDropdown )
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.slot'
                  CP.cp1
                  unit
                  Dropdown.dropdownDark
                  { selectedItem: Just "Kilchoman Blue Label"
                  , items
                  , label: "Pick one"
                  , toString: identity
                  , disabled: true
                  }
                  ( HE.input HandleDropdown )
                ]
              ]
            ]
          ]
        , Documentation.block_
          { header: "Choice"
          , subheader: "A specialized dropdown for making selections."
          }
          [ Backdrop.backdrop
            [ css "h-40 flex items-center justify-center" ]
            [ HH.slot'
                CP.cp2
                unit
                Select.component
                selectInput
                ( HE.input HandleChoice )
            ]
          ]
        ]

      where
        items :: Array String
        items =
          [ "Lagavulin 16"
          , "Kilchoman Blue Label"
          , "Laphroaig"
          , "Ardbeg"
          ]

        selectInput :: Select.Input Query Platform
        selectInput =
          { debounceTime: Nothing
          , initialSearch: Nothing
          , inputType: Select.Toggle
          , items: [ Facebook, Twitter ]
          , render: renderPlatformChoice
          }

        renderPlatformChoice state' =
          HH.div
            [ css "flex items-center flex-col" ]
            [ menu
            , Button.buttonPrimary
              ( SelectSetters.setToggleProps [] )
              [ HH.text "Create Campaign Group" ]
            ]
            where
              visibilityClasses = case state'.visibility of
                Select.On -> css ""
                Select.Off -> css "hidden"

              menu =
                Choice.choice
                  ( SelectSetters.setContainerProps [ visibilityClasses ] )
                  [ Choice.header_
                    [ HH.span
                      [ css "font-medium text-grey-50" ]
                      [ HH.text "Advertise on..." ]
                    ]
                  , Choice.body_ $
                      mapWithIndex
                        ( \index item ->
                            Choice.option
                              ( SelectSetters.setItemProps
                                  index
                                  [ if Just index == state'.highlightedIndex
                                      then HP.classes Choice.highlightedOptionClasses
                                      else HP.classes []
                                  ]
                              )
                              ( renderPlatform item )
                        )
                        state'.items
                  ]

              renderPlatform = case _ of
                Facebook ->
                  [ HH.div_
                    [ Icon.facebook
                      [ css "text-fb-blue text-4xl" ]
                    ]
                  , HH.div_
                    [ HH.p
                      [ css "text-black-20 font-light" ]
                      [ HH.text "Facebook" ]
                    ]
                  ]
                Twitter ->
                  [ HH.div_
                    [ Icon.twitter
                      [ css "text-tw-blue text-4xl" ] ]
                  , HH.div_
                    [ HH.p
                      [ css "text-black-20 font-light" ]
                      [ HH.text "Twitter" ]
                    ]
                  ]

