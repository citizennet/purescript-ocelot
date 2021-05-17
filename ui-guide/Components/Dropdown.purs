module UIGuide.Component.Dropdown where

import Prelude
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Choice as Choice
import Ocelot.Block.Format (caption_) as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Dropdown as Ocelot.Dropdown
import Ocelot.HTML.Properties (css)
import Select as Select
import Select.Setters as SelectSetters
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State 
  = { disabled :: Boolean
    }

data Query

data Action
  = HandleDropdown (Ocelot.Dropdown.Output String)
  | HandleChoice Select.Event
  | ToggleDisabled Boolean

data DropdownSlot
  = StandardStatic
  | StandardDynamic
  | PrimaryStatic
  | PrimaryDynamic
  | DarkStatic
  | DarkDynamic

derive instance eqDropdownSlot :: Eq DropdownSlot

derive instance ordDropdownSlot :: Ord DropdownSlot

type Input = Unit

type Message = Void

type ChildSlot =
  ( dropdown :: Ocelot.Dropdown.Slot String DropdownSlot
  , select :: Select.Slot (Const Query) () Select.Event Unit
  )

_dropdown = Proxy :: Proxy "dropdown"
_select = Proxy :: Proxy "select"

data Platform
  = Facebook
  | Twitter

component
  :: ∀ m
   . MonadAff m
  => H.Component (Const Query) Input Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where
    handleAction :: Action -> H.HalogenM State Action ChildSlot Message m Unit
    handleAction = case _ of
      HandleDropdown message -> case message of
        Ocelot.Dropdown.Selected x -> do
          H.liftEffect (log x)
          H.modify_ identity
        _ -> pure unit
      HandleChoice message -> case message of
        Select.Selected x -> do
          H.liftEffect $ case x of
            0 -> log "Facebook"
            1 -> log "Twitter"
            _ -> pure unit
        _ -> pure unit
      ToggleDisabled old -> do
        let disabled = not old
        void $ H.tell _dropdown StandardDynamic (Ocelot.Dropdown.SetDisabled disabled)
        void $ H.tell _dropdown PrimaryDynamic (Ocelot.Dropdown.SetDisabled disabled)
        void $ H.tell _dropdown DarkDynamic (Ocelot.Dropdown.SetDisabled disabled)
        H.modify_ \state -> state { disabled = disabled }

    initialState :: Input -> State
    initialState _ = { disabled: true }

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
                    StandardStatic
                    Ocelot.Dropdown.component
                    { disabled: false
                    , selectedItem: Nothing
                    , items
                    , render: renderDropdown Button.button
                    }
                    HandleDropdown
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.div
                    [ css "flex" ]
                    [ HH.slot
                        _dropdown
                        StandardDynamic
                        Ocelot.Dropdown.component
                        { disabled: state.disabled
                        , selectedItem: Just "Kilchoman Blue Label"
                        , items
                        , render: renderDropdown Button.button
                        }
                        HandleDropdown
                    , Button.button
                        [ Halogen.HTML.Events.onClick \_ -> ToggleDisabled state.disabled
                        , css "ml-2"
                        ]
                        [ HH.text "Toggle" ]
                    ]
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
                  PrimaryStatic
                  Ocelot.Dropdown.component
                  { disabled: false
                  , selectedItem: Nothing
                  , items
                  , render: renderDropdown Button.buttonPrimary
                  }
                  HandleDropdown
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.div
                    [ css "flex" ]
                    [ HH.slot
                        _dropdown
                        PrimaryDynamic
                        Ocelot.Dropdown.component
                        { disabled: state.disabled
                        , selectedItem: Just "Kilchoman Blue Label"
                        , items
                        , render: renderDropdown Button.buttonPrimary
                        }
                        HandleDropdown
                    , Button.buttonPrimary
                        [ Halogen.HTML.Events.onClick \_ -> ToggleDisabled state.disabled
                        , css "ml-2"
                        ]
                        [ HH.text "Toggle" ]
                    ]
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
                  DarkStatic
                  Ocelot.Dropdown.component
                  { disabled: false
                  , selectedItem: Nothing
                  , items
                  , render: renderDropdown Button.buttonDark
                  }
                  HandleDropdown
                ]
              , HH.div
                [ css "mb-6" ]
                [ Format.caption_
                  [ HH.text "Disabled & Hydrated" ]
                , HH.div
                    [ css "flex" ]
                    [ HH.slot
                        _dropdown
                        DarkDynamic
                        Ocelot.Dropdown.component
                        { disabled: state.disabled
                        , selectedItem: Just "Kilchoman Blue Label"
                        , items
                        , render: renderDropdown Button.buttonDark
                        }
                        HandleDropdown
                    , Button.buttonDark
                        [ Halogen.HTML.Events.onClick \_ -> ToggleDisabled state.disabled
                        , css "ml-2"
                        ]
                        [ HH.text "Toggle" ]
                    ]
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
                HandleChoice
            ]
          ]
        ]

      where
        disableToggleText :: String
        disableToggleText
          | state.disabled = "Enable"
          | otherwise = "Disable"

        items :: Array String
        items =
          [ "Lagavulin 16"
          , "Kilchoman Blue Label"
          , "Laphroaig"
          , "Ardbeg"
          ]

        renderDropdown
          :: (∀ p i. Ocelot.Dropdown.ButtonBlock p i)
          -> Ocelot.Dropdown.CompositeState String
          -> H.ComponentHTML Ocelot.Dropdown.CompositeAction Ocelot.Dropdown.EmbeddedChildSlots m
        renderDropdown btnFn = 
          Ocelot.Dropdown.defDropdown btnFn [] identity "Pick One"

        selectInput :: Select.Input (Ocelot.Dropdown.StateRow Platform)
        selectInput =
          { debounceTime: Nothing
          , disabled: false
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

