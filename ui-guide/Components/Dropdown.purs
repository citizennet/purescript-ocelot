module UIGuide.Component.Dropdown where

import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Choice as Choice
import Ocelot.Block.Format (caption_) as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Components.Dropdown.Component as DD
import Ocelot.Components.Dropdown.Render as DR
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Setters as SelectSetters
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query

data Action
  = HandleDropdown (DD.Output String)
  | HandleChoice Select.Event

type Input = Unit

type Message = Void

type ChildSlot =
  ( dropdown :: DD.Slot String Unit
  , select :: Select.Slot (Const Query) () Select.Event Unit
  )
_dropdown = SProxy :: SProxy "dropdown"
_select = SProxy :: SProxy "select"

data Platform
  = Facebook
  | Twitter

component
  :: ∀ m
   . MonadAff m
  => H.Component HH.HTML (Const Query) Input Message m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where
    handleAction :: Action -> H.HalogenM State Action ChildSlot Message m Unit
    handleAction = case _ of
      HandleDropdown message -> case message of
        DD.Selected x -> do
          H.liftEffect (log x)
          H.modify_ identity

        _ -> pure unit

      HandleChoice message -> case message of
        Select.Selected x -> do
          H.liftEffect $ case x of
            0 -> log "Facebook"
            1 -> log "Twitter"
            _ -> pure unit
          pure unit

        _ -> pure unit

    render
      :: State
      -> H.ComponentHTML Action ChildSlot m
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
                , HH.slot
                    _dropdown
                    unit
                    DD.component
                    { selectedItem: Nothing
                    , items
                    , render: renderDropdown Button.button
                    }
                    ( Just <<< HandleDropdown )
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Just "Kilchoman Blue Label"
                  , items
                  , render: renderDisabledDropdown Button.button
                  }
                  ( Just <<< HandleDropdown )
                ]
              ]
            ]
          , Backdrop.backdropWhite_
            [ Backdrop.content_
              [ HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Primary" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Nothing
                  , items
                  , render: renderDropdown Button.buttonPrimary
                  }
                  ( Just <<< HandleDropdown )
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Just "Kilchoman Blue Label"
                  , items
                  , render: renderDisabledDropdown Button.buttonPrimary
                  }
                  ( Just <<< HandleDropdown )
                ]
              ]
            ]
          , Backdrop.backdropDark_
            [ Backdrop.content_
              [ HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Dark" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Nothing
                  , items
                  , render: renderDropdown Button.buttonDark
                  }
                  ( Just <<< HandleDropdown )
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.slot
                  _dropdown
                  unit
                  DD.component
                  { selectedItem: Just "Kilchoman Blue Label"
                  , items
                  , render: renderDisabledDropdown Button.buttonDark
                  }
                  ( Just <<< HandleDropdown )
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
            [ HH.slot
                _select
                unit
                (Select.component identity choiceSpec )
                selectInput
                ( Just <<< HandleChoice )
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

        renderDropdown
          :: (∀ p i. DR.ButtonBlock p i)
          -> DD.CompositeState String
          -> H.ComponentHTML DD.CompositeAction DD.EmbeddedChildSlots m
        renderDropdown btnFn = DR.defDropdown btnFn [ ] identity "Pick One"

        renderDisabledDropdown
          :: (∀ p i. DR.ButtonBlock p i)
          -> DD.CompositeState String
          -> H.ComponentHTML DD.CompositeAction () m
        renderDisabledDropdown btnFn =
          DR.defDropdown btnFn [ HP.disabled true ] identity "Pick One"

        selectInput :: Select.Input (DD.StateRow Platform)
        selectInput =
          { debounceTime: Nothing
          , search: Nothing
          , inputType: Select.Toggle
          , getItemCount: Array.length <<< _.items
          , items : [ Facebook, Twitter ]
          , selectedItem: Nothing
          }

        choiceSpec = Select.defaultSpec
          { render = renderPlatformChoice, handleEvent = H.raise }

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

