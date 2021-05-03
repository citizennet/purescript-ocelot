module Ocelot.FilePicker
  ( Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML

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
render state = Halogen.HTML.text ""

