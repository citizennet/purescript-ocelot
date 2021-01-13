module UIGuide.Component.MultiInput where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
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
  = NoOp

data Query a

type Input
  = Unit

type Output
  = Void

type ChildSlots =
  ( multiInput :: Ocelot.Components.MultiInput.Component.Slot Unit)

_multiInput = SProxy :: SProxy "multiInput"

component ::
  forall m.
  MonadAff m =>
  Component m
component =
  Halogen.mkComponent
    { initialState
    , render
    , eval: Halogen.mkEval $ Halogen.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState _ = {}

handleAction :: forall m. Action -> ComponentM m Unit
handleAction = case _ of
  NoOp -> do
    pure unit

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
      [ Halogen.HTML.div
          [ Ocelot.HTML.Properties.css "w-1/2" ]
          [ Halogen.HTML.slot _multiInput unit
            Ocelot.Components.MultiInput.Component.component
            { minWidth: 50.0 }
            (const Nothing)
          ]
      ]
    ]
  ]
