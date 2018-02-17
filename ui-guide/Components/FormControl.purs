module UIGuide.Components.FormControl where

import Prelude

import CN.UI.Block.FormControl as FormControl
import CN.UI.Block.FormHeader as FormHeader
import CN.UI.Block.Input as Input
import CN.UI.Block.Radio as Radio
import CN.UI.Block.Toggle as Toggle
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log, CONSOLE)
import DOM.Event.Types (MouseEvent)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UIGuide.Block.Component as Component
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a
  = NoOp a
  | HandleFormHeaderClick MouseEvent a

type Input = Unit

type Message = Void

type Effects eff = ( console :: CONSOLE | eff )

component :: ∀ eff. H.Component HH.HTML Query Input Message (Aff (Effects eff))
component =
  H.component
    { initialState: const unit
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

    render :: State -> H.ComponentHTML Query
    render _ =
      HH.div_
      [ Documentation.documentation
          { header: "Form Header"
          , subheader: "The header used on forms"
          }
          [ Component.component
              { title: "Form Header" }
              [ FormHeader.formHeader
                  { name: "Campaign Group"
                  , onClick: HE.input HandleFormHeaderClick
                  , title: "New"
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
                }
                ( Input.input [ HP.placeholder "davelovesdesignpatterns@gmail.com" ] )
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
                }
                ( Toggle.toggle [] )
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
      ]
