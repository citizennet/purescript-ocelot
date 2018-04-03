module UIGuide.Components.FormControl where

import Prelude

import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormControl as FormControl
import Ocelot.Block.FormHeader as FormHeader
import Ocelot.Block.FormPanel as FormPanel
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.Radio as Radio
import Ocelot.Block.Range as Range
import Ocelot.Block.Toggle as Toggle
import Ocelot.Block.Type as Type
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log, CONSOLE)
import DOM.Event.Types (MouseEvent)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State =
  { formPanelIsOpen :: Boolean }

data Query a
  = NoOp a
  | HandleFormHeaderClick MouseEvent a
  | ToggleFormPanel MouseEvent a

type Input = Unit

type Message = Void

type Effects eff = ( console :: CONSOLE | eff )

component :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (Effects eff))
component =
  H.component
    { initialState: const { formPanelIsOpen: false }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    eval :: Query ~> H.ComponentDSL State Query Message (Aff (Effects eff))
    eval = case _ of
      NoOp a -> do
        pure a

      HandleFormHeaderClick _ a -> do
        H.liftAff (log "submit form")
        pure a

      ToggleFormPanel _ a -> do
        state <- H.get
        H.modify (_ { formPanelIsOpen = not state.formPanelIsOpen })
        pure a

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
      [ Documentation.documentation_
          { header: "Form Header"
          , subheader: "The header used on forms"
          }
          [ Backdrop.backdrop_
            [ HH.h3
              [ HP.classes Type.captionClasses ]
              [ HH.text "Form Header" ]
            , FormHeader.formHeader
              { name: [ HH.text "Campaign Group" ]
              , title: [ HH.text "New" ]
              , brand: Nothing
              , buttons:
                [ Button.buttonClear
                  [ HP.class_ $ HH.ClassName "mr-2" ]
                  [ HH.text "Cancel" ]
                , Button.buttonPrimary
                  [ HE.onClick $ HE.input HandleFormHeaderClick ]
                  [ HH.text "Create" ]
                ]
              }
            ]
          ]
      , Documentation.documentation_
          { header: "Input"
          , subheader: "Inputing very important text"
          }
          [ Backdrop.backdrop_
              [ HH.h3
                [ HP.classes Type.captionClasses ]
                [ HH.text "Standalone Text Input" ]
              , Input.input
                [ HP.placeholder "davelovesdesignpatterns@gmail.com" ]
              ]
          , Backdrop.backdrop_
              [ HH.h3
                [ HP.classes Type.captionClasses ]
                [ HH.text "Text Input with Form Control" ]
              , FormControl.formControl_
                { label: "Email"
                , helpText: Just "Dave will spam your email with gang of four patterns"
                , valid: Nothing
                , inputId: "email"
                }
              [ Input.input
                [ HP.placeholder "davelovesdesignpatterns@gmail.com"
                , HP.id_ "email" 
                ]
              ]
            ]
          ]
      , Documentation.documentation_
          { header: "Toggle"
          , subheader: "Enable or disable something" }
          [ Backdrop.backdrop_
              [ FormControl.formControl_
                { label: "Dave's OO Emails"
                , helpText: Just "Once enabled, you can never unsubscribe."
                , valid: Nothing
                , inputId: "toggle"
                }
                [ Toggle.toggle [ HP.id_ "toggle" ] ]
              ]
          ]
      , Documentation.documentation_
          { header: "Radio"
          , subheader: "Select one option"
          }
          [ Backdrop.backdrop_
              [ Card.card_
                [ Type.contentHeading_
                  [ Icon.tip
                    [ HP.class_ $ HH.ClassName "text-yellow text-2xl" ]
                  , HH.text "Accessibility Note"
                  ]
                , HH.p_
                  [ HH.text "Make sure to use "
                  , HH.code_ [ HH.text "FormControl.fieldset" ]
                  , HH.text " instead of "
                  , HH.code_ [ HH.text "FormControl.formControl" ]
                  , HH.text " with groups of radios and checkboxes."
                  ]
                ]
              , FormControl.fieldset_
                  { label: "Platform"
                  , inputId: "radio"
                  , helpText: Just "Where do you want your ad to appear?"
                  , valid: Nothing
                  }
                  [ HH.div_
                    [ Radio.radio
                        { label: "Facebook" }
                        [ HP.name "platform" ]
                    , Radio.radio
                        { label: "Instagram" }
                        [ HP.name "platform" ]
                    , Radio.radio
                        { label: "Twitter" }
                        [ HP.name "platform" ]
                    ]
                  ]
              ]
          ]
      , Documentation.documentation_
          { header: "Range"
          , subheader: "Select a numeric value between a min and max" }
          [ Backdrop.backdrop_
              [ HH.h3
                [ HP.classes Type.captionClasses ]
                [ HH.text "Standalone Range Input" ]
              , Range.range
                [ HP.id_ "range_"
                , HP.min 0.0
                , HP.max 100.0
                ]
              ]
          , Backdrop.backdrop_
              [ HH.h3
                [ HP.classes Type.captionClasses ]
                [ HH.text "Range Input with Form Control" ]
              , FormControl.formControl_
                { label: "Dave's OO Emails"
                , helpText: Just "How many do you want?"
                , valid: Nothing
                , inputId: "range"
                }
                [ Range.range
                  [ HP.id_ "range"
                  , HP.min 0.0
                  , HP.max 100.0
                  ]
                ]
              ]
          ]
      , Documentation.documentation_
          { header: "Form Panel"
          , subheader: "Collapse + Expand Form Controls"
          }
          [ Backdrop.backdrop_
            [ FormPanel.formPanel
                { isOpen: state.formPanelIsOpen
                , renderToggle:
                  ( \isOpen ->
                      if isOpen
                        then HH.text "Hide Advanced Options"
                        else HH.text "Show Advanced Options"
                  )
                }
                [ HE.onClick (HE.input ToggleFormPanel) ]
                [ FormControl.formControl_
                  { label: "Email"
                  , helpText: Just "Dave will spam your email with gang of four patterns"
                  , valid: Nothing
                  , inputId: "email'"
                  }
                  [ Input.input
                    [ HP.placeholder "davelovesdesignpatterns@gmail.com"
                    , HP.id_ "email'"
                    ]
                  ]
                ]
            ]
          ]
      ]
