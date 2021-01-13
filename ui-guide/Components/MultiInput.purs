module UIGuide.Component.MultiInput where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Components.MultiInput.Component as Ocelot.Components.MultiInput.Component
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type Component m = Halogen.Component Halogen.HTML.HTML Query Input Output m

type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m

type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State
  = {}

data Action
  = Initialize

data Query a

type Input
  = Unit

type Output
  = Void

type ChildSlots =
  ( multiInput :: Ocelot.Components.MultiInput.Component.Slot MultiInputSlot)

_multiInput = SProxy :: SProxy "multiInput"

data MultiInputSlot
  = NoItem
  | WithItems

derive instance eqMultiInputSlot :: Eq MultiInputSlot

derive instance ordMultiInputSlot :: Ord MultiInputSlot

component ::
  forall m.
  MonadAff m =>
  Component m
component =
  Halogen.mkComponent
    { initialState
    , render
    , eval:
      Halogen.mkEval
        Halogen.defaultEval
          { handleAction = handleAction
          , initialize = Just Initialize
          }
    }

initialState :: Input -> State
initialState _ = {}

handleAction :: forall m. Action -> ComponentM m Unit
handleAction = case _ of
  Initialize -> do
    void <<< Halogen.query _multiInput WithItems <<< Halogen.tell
      $ Ocelot.Components.MultiInput.Component.SetItems items

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML m
render state =
  Halogen.HTML.div_
  [ Documentation.customBlock_
    { header: "Multi Input"
    , subheader: "Text Input with Multiple Values"
    }
    [ Backdrop.backdrop_
      [ Backdrop.content_
        [ Card.card
          [ Ocelot.HTML.Properties.css "flex-1 w-2/3" ]
          [ Halogen.HTML.h3
              [ Halogen.HTML.Properties.classes Format.captionClasses ]
              [ Halogen.HTML.text "No Item" ]
          , FormField.field_
              { label: Halogen.HTML.text "Keywords*"
              , helpText: []
              , error: []
              , inputId: "keywords-no-item"
              }
              [ Halogen.HTML.slot _multiInput NoItem
                  Ocelot.Components.MultiInput.Component.component
                  { minWidth
                  , placeholder
                  }
                  (const Nothing)
              ]
          , Halogen.HTML.h3
              [ Halogen.HTML.Properties.classes Format.captionClasses ]
              [ Halogen.HTML.text "With Items" ]
          , FormField.field_
              { label: Halogen.HTML.text "Keywords*"
              , helpText: []
              , error: []
              , inputId: "keywords-with-items"
              }
              [ Halogen.HTML.slot _multiInput WithItems
                  Ocelot.Components.MultiInput.Component.component
                  { minWidth
                  , placeholder
                  }
                  (const Nothing)
              ]
          ]
        ]
      ]
    ]
  ]

minWidth :: Number
minWidth = 50.0

items :: Array String
items =
  [ "citizen"
  , "net"
  , "conde"
  ]

placeholder ::
  { primary :: String
  , secondary :: String
  }
placeholder =
  { primary: "At least one of these values..."
  , secondary: "Or..."
  }
