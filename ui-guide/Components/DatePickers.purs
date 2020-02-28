module UIGuide.Component.DatePickers where

import Prelude

import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Components.DatePicker.Component as DatePicker
import Ocelot.Components.DateTimePicker.Component as DateTimePicker
import Ocelot.Components.TimePicker.Component as TimePicker
import Ocelot.Data.DateTime (unsafeMkDate, unsafeMkTime)
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation


----------
-- Component Types

type State = Unit

data Query a
type Action = Unit

----------
-- Child paths

type ChildSlot =
  ( datePicker :: DatePicker.Slot Int
  , timePicker :: TimePicker.Slot Int
  , dtp :: DateTimePicker.Slot Int
  )

_datePicker = SProxy :: SProxy "datePicker"
_timePicker = SProxy :: SProxy "timePicker"
_dtp = SProxy :: SProxy "dtp"

----------
-- Component definition

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML Query Unit Void m
component =
  H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
    render
      :: State
      -> H.ComponentHTML Action ChildSlot m
    render _ = cnDocumentationBlocks

----------
-- HTML

content :: forall t1 t2. Array (HH.HTML t2 t1) -> HH.HTML t2 t1
content = Backdrop.content [ css "flex" ]

cnDocumentationBlocks :: ∀ m
  . MonadAff m
 => H.ComponentHTML Action ChildSlot m
cnDocumentationBlocks =
  HH.div_
    [ Documentation.block_
      { header: "Date Pickers"
      , subheader: "It's a date picker. Deal with it."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Standard" ]
            , FormField.fieldMid_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start date." ]
              , error: []
              , inputId: "start-date"
              }
              [ HH.slot _datePicker 0 DatePicker.component
                { targetDate: Nothing
                , selection: Nothing
                , disabled: false
                }
                (const $ Just unit)
              ]
            , Format.caption_ [ HH.text "Standard Disabled" ]
            , FormField.fieldMid_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start date." ]
              , error: []
              , inputId: "start-date-disabled"
              }
              [ HH.slot _datePicker 2 DatePicker.component
                { targetDate: Nothing
                , selection: Nothing
                , disabled: true
                }
                (const Nothing)
              ]
            ]
          ]
        , content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Hydrated" ]
            , FormField.fieldMid_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end date." ]
              , error: []
              , inputId: "end-date"
              }
              [ HH.slot _datePicker 1 DatePicker.component
                { targetDate: Nothing
                , selection: Just $ unsafeMkDate 2019 1 1
                , disabled: false
                }
                (const Nothing)
              ]
            , Format.caption_ [ HH.text "Hydrated Disabled" ]
            , FormField.fieldMid_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end date." ]
              , error: []
              , inputId: "end-date-disabled"
              }
              [ HH.slot _datePicker 3 DatePicker.component
                { targetDate: Nothing
                , selection: Just $ unsafeMkDate 2019 1 1
                , disabled: true
                }
                (const Nothing)
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "Time Pickers"
      , subheader: "It's a time picker. Deal with it."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Standard" ]
            , FormField.fieldMid_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start time." ]
              , error: []
              , inputId: "start-time"
              }
              [ HH.slot _timePicker 0 TimePicker.component
                { selection: Nothing
                , disabled: false
                }
                (const Nothing)
              ]
            , Format.caption_ [ HH.text "Standard Disabled" ]
            , FormField.fieldMid_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start time." ]
              , error: []
              , inputId: "start-time-disabled"
              }
              [ HH.slot _timePicker 2 TimePicker.component
                { selection: Nothing
                , disabled: true
                }
                (const Nothing)
              ]
            ]
          ]
        , content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Hydrated" ]
            , FormField.fieldMid_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end time." ]
              , error: []
              , inputId: "end-time"
              }
              [ HH.slot _timePicker 1 TimePicker.component
                { selection: Just $ unsafeMkTime 12 0 0 0
                , disabled: false
                }
                (const Nothing)
              ]
            , Format.caption_ [ HH.text "Hydrated Disabled" ]
            , FormField.fieldMid_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end time." ]
              , error: []
              , inputId: "end-time-disabled"
              }
              [ HH.slot _timePicker 3 TimePicker.component
                { selection: Just $ unsafeMkTime 12 0 0 0
                , disabled: true
                }
                (const Nothing)
              ]
            ]
          ]
        ]
      ]
    , Documentation.block_
      { header: "DateTime Pickers"
      , subheader: "We've combined them. Deal with it."
      }
      [ Backdrop.backdrop_
        [ content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Standard" ]
            , FormField.field_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start date and time." ]
              , error: []
              , inputId: "start"
              }
              [ HH.slot _dtp 0 DateTimePicker.component
                { targetDate: Nothing
                , selection: Nothing
                , disabled: false
                }
                (const Nothing)
              ]
            , Format.caption_ [ HH.text "Standard Disabled" ]
            , FormField.field_
              { label: HH.text "Start"
              , helpText: [ HH.text "Choose a start date and time." ]
              , error: []
              , inputId: "start-disabled"
              }
              [ HH.slot _dtp 2 DateTimePicker.component
                { targetDate: Nothing
                , selection: Nothing
                , disabled: true
                }
                (const Nothing)
              ]
            ]
          ]
        , content
          [ Card.card
            [ css "flex-1" ]
            [ Format.caption_ [ HH.text "Hydrated" ]
            , FormField.field_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end date and time." ]
              , error: []
              , inputId: "end"
              }
              [ HH.slot _dtp 1 DateTimePicker.component
                { targetDate: Nothing
                , selection: Just $ DateTime (unsafeMkDate 2019 1 1) (unsafeMkTime 0 0 0 0)
                , disabled: false
                }
                (const Nothing)
              ]
            , Format.caption_ [ HH.text "Hydrated Disabled" ]
            , FormField.field_
              { label: HH.text "End"
              , helpText: [ HH.text "Choose an end date and time." ]
              , error: []
              , inputId: "end-disabled"
              }
              [ HH.slot _dtp 3 DateTimePicker.component
                { targetDate: Nothing
                , selection: Just $ DateTime (unsafeMkDate 2019 1 1) (unsafeMkTime 0 0 0 0)
                , disabled: true
                }
                (const Nothing)
              ]
            ]
          ]
        ]
      ]
    ]
