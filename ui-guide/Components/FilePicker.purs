module UIGuide.Component.FilePicker where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Card as Card
import Ocelot.Block.Format as Format
import Ocelot.FilePicker as Ocelot.FilePicker
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type Component m = Halogen.Component Halogen.HTML.HTML Query Input Output m

type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m

type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State
  = {}

data Action

data Query a

type Input
  = Unit

type Output
  = Void

type ChildSlots =
  ( filePicker :: Ocelot.FilePicker.Slot Unit
  )

_filePicker = SProxy :: SProxy "filePicker"

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
    }

initialState :: Input -> State
initialState _ = {}

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML m
render state =
  Halogen.HTML.div_
  [ Documentation.customBlock_
    { header: "File Picker"
    , subheader: "Select File(s) from File System"
    }
    [ Backdrop.backdrop_
      [ Backdrop.content_
        [ Card.card
          [ Ocelot.HTML.Properties.css "flex-1" ]
          [ Halogen.HTML.h3
              [ Halogen.HTML.Properties.classes Format.captionClasses ]
              [ Halogen.HTML.text "Single" ]
          , Halogen.HTML.slot _filePicker unit
              Ocelot.FilePicker.component
              unit
              (const Nothing)
          ]
        ]
      ]
    ]
  ]

