module UIGuide.Component.FormControl where

import Prelude

import Data.Array as Data.Array
import Data.Int as Data.Int
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Propreties
import Halogen.Svg.Attributes as Halogen.Svg.Attributes
import Ocelot.Block.Card as Card
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Format as Ocelot.Block.Format
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Radio as Radio
import Ocelot.HTML.Properties (css)
import Ocelot.Slider as Ocelot.Slider
import Ocelot.Slider.Render as Ocelot.Slider.Render
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import Web.UIEvent.MouseEvent (MouseEvent)

type Component m = Halogen.Component Query Input Output m

type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m

type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a


type State =
  { formPanelIsOpen :: Boolean }

data Query (a :: Type)
data Action
  = HandleFormHeaderClick MouseEvent
  | HandleSlider Ocelot.Slider.Output
  | HandleThumbCount Int String
  | Initialize
  | ToggleFormPanel MouseEvent

type Input = Unit

type Output = Void

type ChildSlots =
  ( slider :: Ocelot.Slider.Slot String )

_slider = Proxy :: Proxy "slider"

component ::
  forall m.
  MonadAff m =>
  Component m
component =
  Halogen.mkComponent
    { initialState: const { formPanelIsOpen: false }
    , render
    , eval:
      Halogen.mkEval
        Halogen.defaultEval
          { handleAction = handleAction
          , initialize = Just Initialize
          }
    }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  HandleFormHeaderClick _ -> do
    Halogen.liftEffect (log "submit form")

  HandleSlider output -> case output of
    Ocelot.Slider.ValueChanged xs -> do
      Halogen.liftEffect (log ("slider: " <> show xs))

  HandleThumbCount n slotKey -> do
    void $ Halogen.tell _slider slotKey
      $ Ocelot.Slider.SetThumbCount n

  Initialize -> do
    void <<< Halogen.tell _slider "disabled"
      $ Ocelot.Slider.ReplaceThumbs [ { percent: 30.0 }, { percent: 70.0 } ]

  ToggleFormPanel _ -> do
    state <- Halogen.get
    Halogen.modify_ (_ { formPanelIsOpen = not state.formPanelIsOpen })

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML m
render _ =
  let content = Backdrop.content [ css "flex" ]
      accessibilityCallout =
        Documentation.callout_
          [ Backdrop.backdropWhite
            [ css "flex-col" ]
            [ Format.subHeading_
              [ Icon.tip [ css "text-yellow pr-2" ]
              , Halogen.HTML.text "Accessibility Note"
              ]
            , Halogen.HTML.p_
              [ Halogen.HTML.text "Make sure to use "
              , Halogen.HTML.code_ [ Halogen.HTML.text "FormField.fieldset" ]
              , Halogen.HTML.text " instead of "
              , Halogen.HTML.code_ [ Halogen.HTML.text "FormField.field" ]
              , Halogen.HTML.text " with groups of radios and checkboxes."
              ]
            ]
          ]
    in
  Halogen.HTML.div_
    [ Documentation.customBlock_
      { header: "Sliders"
      , subheader: "Select one or more values in percentage."
      }
      [ Backdrop.backdrop_
        [ Backdrop.content_
          [ Card.card
            [ css "flex-1" ]
            [ Halogen.HTML.h3
                [ Halogen.HTML.Propreties.classes Ocelot.Block.Format.captionClasses ]
                [ Halogen.HTML.text "Discrete" ]
            , Halogen.HTML.div
              [ css "flex" ]
              [ Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-discrete"
                , Halogen.HTML.Propreties.checked true
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 1 "discrete"
                ]
                [ Halogen.HTML.text "1" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-discrete"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 2 "discrete"
                ]
                [ Halogen.HTML.text "2" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-discrete"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 3 "discrete"
                ]
                [ Halogen.HTML.text "3" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-discrete"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 4 "discrete"
                ]
                [ Halogen.HTML.text "4" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-discrete"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 5 "discrete"
                ]
                [ Halogen.HTML.text "5" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-discrete"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 6 "discrete"
                ]
                [ Halogen.HTML.text "6" ]
              ]
            , Halogen.HTML.slot _slider "discrete"
              Ocelot.Slider.component
              { axis: Just axisData
              , disabled: false
              , layout: config
              , marks: Just marksData
              , minDistance: Just { percent: 9.9 }
              , renderIntervals: Data.Array.foldMap renderInterval
              }
              HandleSlider
            ]
          , Card.card
            [ css "flex-1" ]
            [ Halogen.HTML.h3
              [ Halogen.HTML.Propreties.classes Ocelot.Block.Format.captionClasses ]
              [ Halogen.HTML.text "Continuous" ]
            , Halogen.HTML.div
              [ css "flex" ]
              [ Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-continuous"
                , Halogen.HTML.Propreties.checked true
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 1 "continuous"
                ]
                [ Halogen.HTML.text "1" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-continuous"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 2 "continuous"
                ]
                [ Halogen.HTML.text "2" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-continuous"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 3 "continuous"
                ]
                [ Halogen.HTML.text "3" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-continuous"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 4 "continuous"
                ]
                [ Halogen.HTML.text "4" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-continuous"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 5 "continuous"
                ]
                [ Halogen.HTML.text "5" ]
              , Radio.radio
                [ css "pr-6" ]
                [ Halogen.HTML.Propreties.name "slider-continuous"
                , Halogen.HTML.Events.onClick \_ -> HandleThumbCount 6 "continuous"
                ]
                [ Halogen.HTML.text "6" ]
              ]
            , Halogen.HTML.slot _slider "continuous"
              Ocelot.Slider.component
              { axis: Just axisData
              , disabled: false
              , layout: config
              , marks: Nothing
              , minDistance: Just { percent: 10.0 }
              , renderIntervals: Data.Array.foldMap renderInterval
              }
              HandleSlider
            ]
          , Card.card
            [ css "flex-1" ]
            [ Halogen.HTML.h3
              [ Halogen.HTML.Propreties.classes Ocelot.Block.Format.captionClasses ]
              [ Halogen.HTML.text "Disabled" ]
            , Halogen.HTML.slot _slider "disabled"
              Ocelot.Slider.component
              { axis: Just axisData
              , disabled: true
              , layout: config
              , marks: Nothing
              , minDistance: Just { percent: 10.0 }
              , renderIntervals: Data.Array.foldMap renderInterval
              }
              HandleSlider
            ]
          ]
        ]
      ]
    , Documentation.customBlock_
      { header: "Checkboxes"
      , subheader: "Select one or more options."
      }
      [ accessibilityCallout
      , Documentation.callout_
        [ Backdrop.backdrop_
          [ content
            [ Halogen.HTML.div
              [ css "flex-1" ]
              [ Halogen.HTML.h3
                [ Halogen.HTML.Propreties.classes Format.captionClasses ]
                [ Halogen.HTML.text "Vertical List" ]
              , FormField.fieldset_
                { label: Halogen.HTML.text "Platform"
                , inputId: "checkbox-vertical"
                , helpText: [ Halogen.HTML.text "Where do you want your ad to appear?" ]
                , error: []
                }
                [ Halogen.HTML.div_
                  [ Checkbox.checkbox_
                    [ Halogen.HTML.Propreties.name "platform"
                    , Halogen.HTML.Propreties.checked true
                    ]
                    [ Halogen.HTML.text "Facebook" ]
                  , Checkbox.checkbox_
                    [ Halogen.HTML.Propreties.name "platform" ]
                    [ Halogen.HTML.text "Instagram" ]
                  , Checkbox.checkbox_
                    [ Halogen.HTML.Propreties.name "platform" ]
                    [ Halogen.HTML.text "Twitter" ]
                  ]
                ]
              ]
            , Halogen.HTML.div
              [ css "flex-1" ]
              [ Halogen.HTML.h3
                [ Halogen.HTML.Propreties.classes Format.captionClasses ]
                [ Halogen.HTML.text "Horizontal List" ]
              , FormField.fieldset_
                { label: Halogen.HTML.text "Platform"
                , inputId: "checkbox-horizontal"
                , helpText: [ Halogen.HTML.text "Where do you want your ad to appear?" ]
                , error: []
                }
                [ Halogen.HTML.div
                  [ css "flex" ]
                  [ Checkbox.checkbox
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "platform"
                    , Halogen.HTML.Propreties.checked true
                    ]
                    [ Halogen.HTML.text "Facebook" ]
                  , Checkbox.checkbox
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "platform" ]
                    [ Halogen.HTML.text "Instagram" ]
                  , Checkbox.checkbox
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "platform" ]
                    [ Halogen.HTML.text "Twitter" ]
                  ]
                ]
              ]
            ]
          ]
        ]
      , Documentation.callout_
        [ Backdrop.backdrop_
          [ content
            [ Halogen.HTML.div
              [ css "flex-1" ]
              [ Halogen.HTML.h3
                [ Halogen.HTML.Propreties.classes Format.captionClasses ]
                [ Halogen.HTML.text "Disabled Vertical List" ]
              , FormField.fieldset_
                { label: Halogen.HTML.text "Platform"
                , inputId: "checkbox-vertical-disabled"
                , helpText: [ Halogen.HTML.text "Where do you want your ad to appear?" ]
                , error: []
                }
                [ Halogen.HTML.div_
                  [ Checkbox.checkbox_
                    [ Halogen.HTML.Propreties.name "platform-disabled"
                    , Halogen.HTML.Propreties.checked true
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Facebook" ]
                  , Checkbox.checkbox_
                    [ Halogen.HTML.Propreties.name "platform-disabled"
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Instagram" ]
                  , Checkbox.checkbox_
                    [ Halogen.HTML.Propreties.name "platform-disabled"
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Twitter" ]
                  ]
                ]
              ]
            , Halogen.HTML.div
              [ css "flex-1" ]
              [ Halogen.HTML.h3
                [ Halogen.HTML.Propreties.classes Format.captionClasses ]
                [ Halogen.HTML.text "Disabled Horizontal List" ]
              , FormField.fieldset_
                { label: Halogen.HTML.text "Platform"
                , inputId: "checkbox-horizontal-disabled"
                , helpText: [ Halogen.HTML.text "Where do you want your ad to appear?" ]
                , error: []
                }
                [ Halogen.HTML.div
                  [ css "flex" ]
                  [ Checkbox.checkbox
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "platform-disabled"
                    , Halogen.HTML.Propreties.checked true
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Facebook" ]
                  , Checkbox.checkbox
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "platform-disabled"
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Instagram" ]
                  , Checkbox.checkbox
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "platform-disabled"
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Twitter" ]
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
            [ Halogen.HTML.div
              [ css "flex-1" ]
              [ Halogen.HTML.h3
                [ Halogen.HTML.Propreties.classes Format.captionClasses ]
                [ Halogen.HTML.text "Vertical List" ]
              , FormField.fieldset_
                { label: Halogen.HTML.text "Optimization Goal"
                , inputId: "radio-vertical"
                , helpText: [ Halogen.HTML.text "What do you want to optimize for?" ]
                , error: []
                }
                [ Halogen.HTML.div_
                  [ Radio.radio_
                    [ Halogen.HTML.Propreties.name "goal"
                    , Halogen.HTML.Propreties.checked true
                    ]
                    [ Halogen.HTML.text "Page Likes" ]
                  , Radio.radio_
                    [ Halogen.HTML.Propreties.name "goal" ]
                    [ Halogen.HTML.text "Impressions" ]
                  , Radio.radio_
                    [ Halogen.HTML.Propreties.name "goal" ]
                    [ Halogen.HTML.text "Page Engagement" ]
                  ]
                ]
              ]
            , Halogen.HTML.div
              [ css "flex-1" ]
              [ Halogen.HTML.h3
                [ Halogen.HTML.Propreties.classes Format.captionClasses ]
                [ Halogen.HTML.text "Horizontal List" ]
              , FormField.fieldset_
                { label: Halogen.HTML.text "Previews"
                , inputId: "radio-horizontal"
                , helpText: [ Halogen.HTML.text "What kind of preview do you want to see?" ]
                , error: []
                }
                [ Halogen.HTML.div
                  [ css "flex" ]
                  [ Radio.radio
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "preview"
                    , Halogen.HTML.Propreties.checked true
                    ]
                    [ Halogen.HTML.text "Desktop" ]
                  , Radio.radio
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "preview" ]
                    [ Halogen.HTML.text "Story" ]
                  , Radio.radio
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "preview" ]
                    [ Halogen.HTML.text "Mobile" ]
                  ]
                ]
              ]
            ]
          ]
        ]
      , Documentation.callout_
        [ Backdrop.backdrop_
          [ content
            [ Halogen.HTML.div
              [ css "flex-1" ]
              [ Halogen.HTML.h3
                [ Halogen.HTML.Propreties.classes Format.captionClasses ]
                [ Halogen.HTML.text "Disabled Vertical List" ]
              , FormField.fieldset_
                { label: Halogen.HTML.text "Optimization Goal"
                , inputId: "radio-vertical-disabled"
                , helpText: [ Halogen.HTML.text "What do you want to optimize for?" ]
                , error: []
                }
                [ Halogen.HTML.div_
                  [ Radio.radio_
                    [ Halogen.HTML.Propreties.name "goal-disabled"
                    , Halogen.HTML.Propreties.checked true
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Page Likes" ]
                  , Radio.radio_
                    [ Halogen.HTML.Propreties.name "goal-disabled"
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Impressions" ]
                  , Radio.radio_
                    [ Halogen.HTML.Propreties.name "goal-disabled"
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Page Engagement" ]
                  ]
                ]
              ]
            , Halogen.HTML.div
              [ css "flex-1" ]
              [ Halogen.HTML.h3
                [ Halogen.HTML.Propreties.classes Format.captionClasses ]
                [ Halogen.HTML.text "Horizontal List" ]
              , FormField.fieldset_
                { label: Halogen.HTML.text "Disabled Previews"
                , inputId: "radio-horizontal-disabled"
                , helpText: [ Halogen.HTML.text "What kind of preview do you want to see?" ]
                , error: []
                }
                [ Halogen.HTML.div
                  [ css "flex" ]
                  [ Radio.radio
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "preview-disabled"
                    , Halogen.HTML.Propreties.checked true
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Desktop" ]
                  , Radio.radio
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "preview-disabled"
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Story" ]
                  , Radio.radio
                    [ css "pr-6" ]
                    [ Halogen.HTML.Propreties.name "preview-disabled"
                    , Halogen.HTML.Propreties.disabled true
                    ]
                    [ Halogen.HTML.text "Mobile" ]
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
        -- [ Halogen.HTML.h3
          -- [ Halogen.HTML.Propreties.classes Format.captionClasses ]
          -- [ Halogen.HTML.text "Standalone Range Input" ]
        -- , Range.range
          -- [ Halogen.HTML.Propreties.id_ "range_"
          -- , Halogen.HTML.Propreties.min 0.0
          -- , Halogen.HTML.Propreties.max 100.0
          -- ]
        -- ]
      -- , Backdrop.backdrop_
        -- [ Halogen.HTML.h3
          -- [ Halogen.HTML.Propreties.classes Format.captionClasses ]
          -- [ Halogen.HTML.text "Range Input with Form Control" ]
        -- , FormField.field_
          -- { label: Halogen.HTML.text "Dave's OO Emails"
          -- , helpText: [ Halogen.HTML.text "How many do you want?" ]
          -- , error: []
          -- , inputId: "range"
          -- }
          -- [ Range.range
            -- [ Halogen.HTML.Propreties.id_ "range"
            -- , Halogen.HTML.Propreties.min 0.0
            -- , Halogen.HTML.Propreties.max 100.0
            -- ]
          -- ]
        -- ]
      -- ]
    ]


axisData :: Array { label :: String, percent :: Number }
axisData = toLabel <$> Data.Array.range 0 10
  where
  toLabel :: Int -> { label :: String, percent :: Number }
  toLabel index =
    { label: show index <> "%"
    , percent: (Data.Int.toNumber index) * 10.0
    }

config :: Ocelot.Slider.Render.Config
config =
  { axisHeight: 20.0
  , betweenThumbAndAxis: 20.0
  , betweenTopAndThumb: 20.0
  , frameWidth: { px: 400.0 }
  , margin: 5.0
  , trackWidth: 400.0
  , trackRadius: 2.5
  , thumbRadius: 10.0
  }

marksData :: Array { percent :: Number }
marksData = toMark <$> Data.Array.range 0 10
  where
  toMark :: Int -> { percent :: Number }
  toMark index = { percent: (Data.Int.toNumber index) * 10.0 }

renderInterval :: Ocelot.Slider.Interval -> Array Halogen.HTML.PlainHTML
renderInterval = case _ of
  Ocelot.Slider.StartToThumb _ -> []
  Ocelot.Slider.BetweenThumbs { left, right } ->
    [ Ocelot.Slider.Render.interval config
        { start: left, end: right }
        [ Halogen.Svg.Attributes.fill (pure (Halogen.Svg.Attributes.RGB 126 135 148)) ]
    ]
  Ocelot.Slider.ThumbToEnd _ -> []
