module UIGuide.Component.DatePickers where

import Prelude

import Data.DateTime (DateTime(..))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Component.DatePicker as DatePicker
import Ocelot.Component.DateTimePicker as DateTimePicker
import Ocelot.Component.TimePicker as TimePicker
import Ocelot.Data.DateTime (unsafeMkDate, unsafeMkTime)
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation


----------
-- Component Types

type State = Unit

data Query a
  = NoOp a

----------
-- Child paths

type ChildSlot = Either3 Int Int Int

type ChildQuery = Coproduct3
  (DatePicker.Query)
  (TimePicker.Query)
  (DateTimePicker.Query)

----------
-- Component definition

component :: ∀ m
  . MonadAff m
 => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render
      :: State
      -> H.ParentHTML Query ChildQuery ChildSlot m
    render _ = cnDocumentationBlocks

    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval (NoOp next) = pure next


----------
-- HTML

content :: ∀ p i. Array (HH.HTML p (i Unit)) -> HH.HTML p (i Unit)
content = Backdrop.content [ css "flex" ]

cnDocumentationBlocks :: ∀ m
  . MonadAff m
 => H.ParentHTML Query ChildQuery ChildSlot m
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
              [ HH.slot' CP.cp1 0 DatePicker.component
                { targetDate: Nothing
                , selection: Nothing
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
              [ HH.slot' CP.cp1 1 DatePicker.component
                { targetDate: Nothing
                , selection: Just $ unsafeMkDate 2019 1 1
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
              [ HH.slot' CP.cp2 0 TimePicker.component
                { selection: Nothing
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
              [ HH.slot' CP.cp2 1 TimePicker.component
                { selection: Just $ unsafeMkTime 12 0 0 0
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
              [ HH.slot' CP.cp3 0 DateTimePicker.component
                { targetDate: Nothing
                , selection: Nothing
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
              [ HH.slot' CP.cp3 1 DateTimePicker.component
                { targetDate: Nothing
                , selection: Just $ DateTime (unsafeMkDate 2019 1 1) (unsafeMkTime 0 0 0 0)
                }
                (const Nothing)
              ]
            ]
          ]
        ]
      ]
    ]
