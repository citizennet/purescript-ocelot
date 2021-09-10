module Ocelot.Clipboard
  ( Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import Prelude
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML

type Slot
  = Halogen.Slot Query Output

type Component m
  = Halogen.Component Query Input Output m

type ComponentHTML m
  = Halogen.ComponentHTML Action ChildSlot m

type ComponentM m
  = Halogen.HalogenM State Action ChildSlot Output m

type State
  = {}

data Action

data Query (a :: Type)

type Input
  = Unit

type Output
  = Void

type ChildSlot
  = () :: Row Type

component ::
  forall m.
  Component m
component =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
    , initialState
    , render
    }

initialState :: Input -> State
initialState _ = {}

render ::
  forall m.
  State ->
  ComponentHTML m
render _ = Halogen.HTML.text ""
