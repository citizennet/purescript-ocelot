module UIGuide.Components.FormControl where

import Prelude

import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Radio as Radio
import Ocelot.Block.Format as Format
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log, CONSOLE)
import DOM.Event.Types (MouseEvent)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
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
      let css :: ∀ p i. String -> H.IProp ( "class" :: String | p ) i
          css = HP.class_ <<< HH.ClassName
          content = Backdrop.content [ css "flex" ]
          accessibilityCallout =
            Documentation.callout_
              [ Backdrop.backdropWhite
                [ css "flex-col" ]
                [ Format.subHeading_
                  [ Icon.tip [ css "text-yellow pr-2" ]
                  , HH.text "Accessibility Note"
                  ]
                , HH.p_
                  [ HH.text "Make sure to use "
                  , HH.code_ [ HH.text "FormField.fieldset" ]
                  , HH.text " instead of "
                  , HH.code_ [ HH.text "FormField.field" ]
                  , HH.text " with groups of radios and checkboxes."
                  ]
                ]
              ]
        in
      HH.div_
        [ Documentation.customBlock_
          { header: "Checkboxes"
          , subheader: "Select one or more options."
          }
          [ accessibilityCallout
          , Documentation.callout_
            [ Backdrop.backdrop_
              [ content
                [ HH.div
                  [ css "flex-1" ]
                  [ HH.h3
                    [ HP.classes Format.captionClasses ]
                    [ HH.text "Vertical List" ]
                  , FormField.fieldset_
                    { label: "Platform"
                    , inputId: "checkbox-vertical"
                    , helpText: Just "Where do you want your ad to appear?"
                    , error: Nothing
                    }
                    [ HH.div_
                      [ Checkbox.checkbox_
                        [ HP.name "platform"
                        , HP.checked true
                        ]
                        [ HH.text "Facebook" ]
                      , Checkbox.checkbox_
                        [ HP.name "platform" ]
                        [ HH.text "Instagram" ]
                      , Checkbox.checkbox_
                        [ HP.name "platform" ]
                        [ HH.text "Twitter" ]
                      ]
                    ]
                  ]
                , HH.div
                  [ css "flex-1" ]
                  [ HH.h3
                    [ HP.classes Format.captionClasses ]
                    [ HH.text "Horizontal List" ]
                  , FormField.fieldset_
                    { label: "Platform"
                    , inputId: "checkbox-horizontal"
                    , helpText: Just "Where do you want your ad to appear?"
                    , error: Nothing
                    }
                    [ HH.div
                      [ css "flex" ]
                      [ Checkbox.checkbox
                        [ css "pr-6" ]
                        [ HP.name "platform"
                        , HP.checked true
                        ]
                        [ HH.text "Facebook" ]
                      , Checkbox.checkbox
                        [ css "pr-6" ]
                        [ HP.name "platform" ]
                        [ HH.text "Instagram" ]
                      , Checkbox.checkbox
                        [ css "pr-6" ]
                        [ HP.name "platform" ]
                        [ HH.text "Twitter" ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          , Documentation.callout_
            [ Backdrop.backdrop_
              [ content
                [ HH.div
                  [ css "flex-1" ]
                  [ HH.h3
                    [ HP.classes Format.captionClasses ]
                    [ HH.text "Disabled Vertical List" ]
                  , FormField.fieldset_
                    { label: "Platform"
                    , inputId: "checkbox-vertical-disabled"
                    , helpText: Just "Where do you want your ad to appear?"
                    , error: Nothing
                    }
                    [ HH.div_
                      [ Checkbox.checkbox_
                        [ HP.name "platform-disabled"
                        , HP.checked true
                        , HP.disabled true
                        ]
                        [ HH.text "Facebook" ]
                      , Checkbox.checkbox_
                        [ HP.name "platform-disabled"
                        , HP.disabled true
                        ]
                        [ HH.text "Instagram" ]
                      , Checkbox.checkbox_
                        [ HP.name "platform-disabled"
                        , HP.disabled true
                        ]
                        [ HH.text "Twitter" ]
                      ]
                    ]
                  ]
                , HH.div
                  [ css "flex-1" ]
                  [ HH.h3
                    [ HP.classes Format.captionClasses ]
                    [ HH.text "Disabled Horizontal List" ]
                  , FormField.fieldset_
                    { label: "Platform"
                    , inputId: "checkbox-horizontal-disabled"
                    , helpText: Just "Where do you want your ad to appear?"
                    , error: Nothing
                    }
                    [ HH.div
                      [ css "flex" ]
                      [ Checkbox.checkbox
                        [ css "pr-6" ]
                        [ HP.name "platform-disabled"
                        , HP.checked true
                        , HP.disabled true
                        ]
                        [ HH.text "Facebook" ]
                      , Checkbox.checkbox
                        [ css "pr-6" ]
                        [ HP.name "platform-disabled"
                        , HP.disabled true
                        ]
                        [ HH.text "Instagram" ]
                      , Checkbox.checkbox
                        [ css "pr-6" ]
                        [ HP.name "platform-disabled"
                        , HP.disabled true
                        ]
                        [ HH.text "Twitter" ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        , Documentation.customBlock_
          { header: "Radios"
          , subheader: "Select one option."
          }
          [ accessibilityCallout
          , Documentation.callout_
            [ Backdrop.backdrop_
              [ content
                [ HH.div
                  [ css "flex-1" ]
                  [ HH.h3
                    [ HP.classes Format.captionClasses ]
                    [ HH.text "Vertical List" ]
                  , FormField.fieldset_
                    { label: "Optimization Goal"
                    , inputId: "radio-vertical"
                    , helpText: Just "What do you want to optimize for?"
                    , error: Nothing
                    }
                    [ HH.div_
                      [ Radio.radio_
                        [ HP.name "goal"
                        , HP.checked true
                        ]
                        [ HH.text "Page Likes" ]
                      , Radio.radio_
                        [ HP.name "goal" ]
                        [ HH.text "Impressions" ]
                      , Radio.radio_
                        [ HP.name "goal" ]
                        [ HH.text "Page Engagement" ]
                      ]
                    ]
                  ]
                , HH.div
                  [ css "flex-1" ]
                  [ HH.h3
                    [ HP.classes Format.captionClasses ]
                    [ HH.text "Horizontal List" ]
                  , FormField.fieldset_
                    { label: "Previews"
                    , inputId: "radio-horizontal"
                    , helpText: Just "What kind of preview do you want to see?"
                    , error: Nothing
                    }
                    [ HH.div
                      [ css "flex" ]
                      [ Radio.radio
                        [ css "pr-6" ]
                        [ HP.name "preview"
                        , HP.checked true
                        ]
                        [ HH.text "Desktop" ]
                      , Radio.radio
                        [ css "pr-6" ]
                        [ HP.name "preview" ]
                        [ HH.text "Story" ]
                      , Radio.radio
                        [ css "pr-6" ]
                        [ HP.name "preview" ]
                        [ HH.text "Mobile" ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          , Documentation.callout_
            [ Backdrop.backdrop_
              [ content
                [ HH.div
                  [ css "flex-1" ]
                  [ HH.h3
                    [ HP.classes Format.captionClasses ]
                    [ HH.text "Disabled Vertical List" ]
                  , FormField.fieldset_
                    { label: "Optimization Goal"
                    , inputId: "radio-vertical-disabled"
                    , helpText: Just "What do you want to optimize for?"
                    , error: Nothing
                    }
                    [ HH.div_
                      [ Radio.radio_
                        [ HP.name "goal-disabled"
                        , HP.checked true
                        , HP.disabled true
                        ]
                        [ HH.text "Page Likes" ]
                      , Radio.radio_
                        [ HP.name "goal-disabled"
                        , HP.disabled true
                        ]
                        [ HH.text "Impressions" ]
                      , Radio.radio_
                        [ HP.name "goal-disabled"
                        , HP.disabled true
                        ]
                        [ HH.text "Page Engagement" ]
                      ]
                    ]
                  ]
                , HH.div
                  [ css "flex-1" ]
                  [ HH.h3
                    [ HP.classes Format.captionClasses ]
                    [ HH.text "Horizontal List" ]
                  , FormField.fieldset_
                    { label: "Disabled Previews"
                    , inputId: "radio-horizontal-disabled"
                    , helpText: Just "What kind of preview do you want to see?"
                    , error: Nothing
                    }
                    [ HH.div
                      [ css "flex" ]
                      [ Radio.radio
                        [ css "pr-6" ]
                        [ HP.name "preview-disabled"
                        , HP.checked true
                        , HP.disabled true
                        ]
                        [ HH.text "Desktop" ]
                      , Radio.radio
                        [ css "pr-6" ]
                        [ HP.name "preview-disabled"
                        , HP.disabled true
                        ]
                        [ HH.text "Story" ]
                      , Radio.radio
                        [ css "pr-6" ]
                        [ HP.name "preview-disabled"
                        , HP.disabled true
                        ]
                        [ HH.text "Mobile" ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        -- TODO
        -- , Documentation.block_
          -- { header: "Ranges"
          -- , subheader: "Select a numeric value between a min and max" }
          -- [ Backdrop.backdrop_
            -- [ HH.h3
              -- [ HP.classes Format.captionClasses ]
              -- [ HH.text "Standalone Range Input" ]
            -- , Range.range
              -- [ HP.id_ "range_"
              -- , HP.min 0.0
              -- , HP.max 100.0
              -- ]
            -- ]
          -- , Backdrop.backdrop_
            -- [ HH.h3
              -- [ HP.classes Format.captionClasses ]
              -- [ HH.text "Range Input with Form Control" ]
            -- , FormField.field_
              -- { label: "Dave's OO Emails"
              -- , helpText: Just "How many do you want?"
              -- , error: Nothing
              -- , inputId: "range"
              -- }
              -- [ Range.range
                -- [ HP.id_ "range"
                -- , HP.min 0.0
                -- , HP.max 100.0
                -- ]
              -- ]
            -- ]
          -- ]
        ]
