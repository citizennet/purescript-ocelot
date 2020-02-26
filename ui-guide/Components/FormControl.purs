module UIGuide.Component.FormControl where

import Prelude

import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Radio as Radio
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import Web.UIEvent.MouseEvent (MouseEvent)

type State =
  { formPanelIsOpen :: Boolean }

data Query a
data Action
  = HandleFormHeaderClick MouseEvent
  | ToggleFormPanel MouseEvent

type Input = Unit

type Message = Void

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
    { initialState: const { formPanelIsOpen: false }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
    handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
    handleAction = case _ of
      HandleFormHeaderClick _ -> do
        H.liftEffect (log "submit form")

      ToggleFormPanel _ -> do
        state <- H.get
        H.modify_ (_ { formPanelIsOpen = not state.formPanelIsOpen })

    render :: State -> H.ComponentHTML Action () Aff
    render state =
      let content = Backdrop.content [ css "flex" ]
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
                    { label: HH.text "Platform"
                    , inputId: "checkbox-vertical"
                    , helpText: [ HH.text "Where do you want your ad to appear?" ]
                    , error: []
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
                    { label: HH.text "Platform"
                    , inputId: "checkbox-horizontal"
                    , helpText: [ HH.text "Where do you want your ad to appear?" ]
                    , error: []
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
                    { label: HH.text "Platform"
                    , inputId: "checkbox-vertical-disabled"
                    , helpText: [ HH.text "Where do you want your ad to appear?" ]
                    , error: []
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
                    { label: HH.text "Platform"
                    , inputId: "checkbox-horizontal-disabled"
                    , helpText: [ HH.text "Where do you want your ad to appear?" ]
                    , error: []
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
                    { label: HH.text "Optimization Goal"
                    , inputId: "radio-vertical"
                    , helpText: [ HH.text "What do you want to optimize for?" ]
                    , error: []
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
                    { label: HH.text "Previews"
                    , inputId: "radio-horizontal"
                    , helpText: [ HH.text "What kind of preview do you want to see?" ]
                    , error: []
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
                    { label: HH.text "Optimization Goal"
                    , inputId: "radio-vertical-disabled"
                    , helpText: [ HH.text "What do you want to optimize for?" ]
                    , error: []
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
                    { label: HH.text "Disabled Previews"
                    , inputId: "radio-horizontal-disabled"
                    , helpText: [ HH.text "What kind of preview do you want to see?" ]
                    , error: []
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
              -- { label: HH.text "Dave's OO Emails"
              -- , helpText: [ HH.text "How many do you want?" ]
              -- , error: []
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
