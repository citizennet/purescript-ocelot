module UIGuide.Components.FormControl where

import Prelude

import Ocelot.Block.FormControl as FormControl
import Ocelot.Block.FormHeader as FormHeader
import Ocelot.Block.FormPanel as FormPanel
import Ocelot.Block.Input as Input
import Ocelot.Block.Radio as Radio
import Ocelot.Block.Range as Range
import Ocelot.Block.Toggle as Toggle
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log, CONSOLE)
import DOM.Event.Types (MouseEvent)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UIGuide.Block.Component as Component
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
      [ Documentation.documentation
          { header: "Form Header"
          , subheader: "The header used on forms"
          }
          [ Component.component
              { title: "Form Header" }
              [ FormHeader.formHeader_
                  { name: "Campaign Group"
                  , onClick: HE.input HandleFormHeaderClick
                  , title: "New"
                  , brand: Nothing
                  }
              ]
          ]
      , Documentation.documentation
          { header: "Input"
          , subheader: "Inputing very important text"
          }
          [ Component.component
              { title: "Input" }
              [ Input.input
                  [ HP.placeholder "davelovesdesignpatterns@gmail.com" ]
              ]
          , Component.component
              { title: "Input with Form Control" }
              [ FormControl.formControl
                { label: "Email"
                , helpText: Just "Dave will spam your email with gang of four patterns"
                , valid: Nothing
                , inputId: "email"
                }
                ( Input.input [ HP.placeholder "davelovesdesignpatterns@gmail.com", HP.id_ "email" ] )
              ]
          ]
      , Documentation.documentation
          { header: "Toggle"
          , subheader: "Enable or disable something" }
          [ Component.component
              { title: "Toggle" }
              [ Toggle.toggle [] ]
          , Component.component
              { title: "Toggle with Form Control" }
              [ FormControl.formControl
                { label: "Dave's OO Emails"
                , helpText: Just "Once enabled, you can never unsubscribe."
                , valid: Nothing
                , inputId: "toggle"
                }
                ( Toggle.toggle [ HP.id_ "toggle" ] )
              ]
          ]
      , Documentation.documentation
          { header: "Radio"
          , subheader: "Select one option"
          }
          [ Component.component
              { title: "Radio" }
              [ HH.div_
                  [ Radio.radio
                      { label: "Apples" }
                      [ HP.name "fruit" ]
                  , Radio.radio
                      { label: "Bananas" }
                      [ HP.name "fruit" ]
                  , Radio.radio
                      { label: "Oranges" }
                      [ HP.name "fruit" ]
                  ]
              ]
          , Component.component
              { title: "Radio with Form Control" }
              [ FormControl.formControl
                  { label: "Platform"
                  , inputId: "radio"
                  , helpText: Just "Where do you want your ad to appear?"
                  , valid: Nothing
                  }
                  ( HH.div_
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
                  )
              ]
          ]
      , Documentation.documentation
          { header: "Range"
          , subheader: "Select a numeric value between a min and max" }
          [ Component.component
              { title: "Range" }
              [ Range.range
                [ HP.id_ "range_"
                , HP.min 0.0
                , HP.max 100.0
                ]
              ]
          , Component.component
              { title: "Range with Form Control" }
              [ FormControl.formControl
                { label: "Dave's OO Emails"
                , helpText: Just "How many do you want?"
                , valid: Nothing
                , inputId: "range"
                }
                ( Range.range
                    [ HP.id_ "range"
                    , HP.min 0.0
                    , HP.max 100.0
                    ]
                )
              ]
          ]
      , Documentation.documentation
          { header: "Form Panel"
          , subheader: "Collapse + Expand Form Controls"
          }
          [ Component.component
            { title: "Form Panel" }
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
                [ FormControl.formControl
                    { label: "Email"
                    , helpText: Just "Dave will spam your email with gang of four patterns"
                    , valid: Nothing
                    , inputId: "email'"
                    }
                    ( Input.input [ HP.placeholder "davelovesdesignpatterns@gmail.com", HP.id_ "email'" ] )
                ]
            ]
          ]
      ]
