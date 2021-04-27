module UIGuide.Component.DatePickers where

import Prelude

import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Ocelot.Block.Button as Button
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

type State = 
  { disabled :: Boolean -- | Global enable/disable toggle
  }

data Query a
data Action 
  = ToggleDisabled

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
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    }
  }
  where
    handleAction = case _ of  
      ToggleDisabled -> do
        st <- H.modify \s -> s { disabled = not s.disabled }
        void $ H.query _datePicker 0 $ H.tell $ DatePicker.SetDisabled st.disabled
        void $ H.query _datePicker 1 $ H.tell $ DatePicker.SetDisabled st.disabled
        void $ H.query _timePicker 0 $ H.tell $ TimePicker.SetDisabled st.disabled
        void $ H.query _timePicker 1 $ H.tell $ TimePicker.SetDisabled st.disabled
        void $ H.query _dtp 0 $ H.tell $ DateTimePicker.SetDisabled st.disabled
        void $ H.query _dtp 1 $ H.tell $ DateTimePicker.SetDisabled st.disabled
        
    initialState :: Unit -> State
    initialState _ = { disabled: false }

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
    [ HH.h1
      [ css "font-normal mb-6" ] 
      [ HH.text "Date Pickers" ]
    , HH.h2
      [ css "font-medium text-grey-50 text-xl mb-6" ]
      [ HH.text "It's a date picker. Deal with it" ]
    , HH.div
      [ css "mb-6" ]
      [ HH.p
        [ css "inline mr-4" ]
        [ HH.text "You can toggle between enabled/disabled states" ]
      , Button.button
        [ HE.onClick $ const $ Just ToggleDisabled ]
        [ HH.text "Toggle" ]
      ]
    , HH.div_
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
                (const Nothing)
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
