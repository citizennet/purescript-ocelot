module Ocelot.FilePicker
  ( Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Foreign.Object as Foreign.Object
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.HTML.Properties as Ocelot.HTMl.Properties

type Slot = Halogen.Slot Query Output

type Component m = Halogen.Component Halogen.HTML.HTML Query Input Output m
type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m
type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State = Unit

data Action

data Query a

type Input = Unit

type Output = Void

type ChildSlots = ()


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
initialState input = unit

render ::
  forall m.
  State ->
  ComponentHTML m
render state =
  Halogen.HTML.div_
    [ renderDropBox
    , renderInput
    ]

renderDropBox ::
  forall m.
  ComponentHTML m
renderDropBox =
  Halogen.HTML.div
    [ Ocelot.HTMl.Properties.css "bg-grey-70-a40 p-10"
    , Ocelot.HTMl.Properties.style <<< Foreign.Object.fromHomogeneous $
        { outline: "2px dashed #8f9eb3"
        , "outline-offset": "-10px"
        , transition: "outline-offset .15s ease-in-out, background-color .15s linear"
        }
    ]
    [ renderIcon
    , renderLabel
    ]

renderIcon ::
  forall m.
  ComponentHTML m
renderIcon =
  Halogen.HTML.div
    [ Ocelot.HTMl.Properties.css "text-grey-50 text-center w-full" ]
    [ Ocelot.Block.Icon.download
      [ Ocelot.HTMl.Properties.css "text-5xl" ]
    ]

renderInput ::
  forall m.
  ComponentHTML m
renderInput =
  Halogen.HTML.input
    [ Ocelot.HTMl.Properties.css "hidden"
    , Halogen.HTML.Properties.id_ _file
    , Halogen.HTML.Properties.type_ Halogen.HTML.Properties.InputFile
    ]

renderLabel ::
  forall m.
  ComponentHTML m
renderLabel =
  Halogen.HTML.label
    [ Ocelot.HTMl.Properties.css "group text-center"
    , Halogen.HTML.Properties.for _file
    ]
    [ Halogen.HTML.span
      [ Ocelot.HTMl.Properties.css "group-hover:text-blue-88" ]
      [ Halogen.HTML.text "Choose a file" ]
    , Halogen.HTML.text " or drag it here."
    ]

_file = "file" :: String
