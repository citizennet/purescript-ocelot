module Ocelot.Clipboard
  ( Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import Prelude

import Effect.Aff as Effect.Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Propetires
import Ocelot.Block.Button as Ocelot.Block.Button
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.HTML.Properties as Ocelot.HTML.Properties

type Slot
  = Halogen.Slot Query Output

type Component m
  = Halogen.Component Query Input Output m

type ComponentHTML m
  = Halogen.ComponentHTML Action ChildSlot m

type ComponentM m
  = Halogen.HalogenM State Action ChildSlot Output m

type State
  = { copied :: Boolean }

data Action
  = Copy

data Query (a :: Type)

type Input
  = Unit

type Output
  = Void

type ChildSlot
  = () :: Row Type

component ::
  forall m.
  MonadAff m =>
  Component m
component =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction
            }
    , initialState
    , render
    }

initialState :: Input -> State
initialState _ =
  { copied: false
  }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  Copy -> copy

copy ::
  forall m.
  MonadAff m =>
  ComponentM m Unit
copy = do
  Halogen.modify_ _ { copied = true }
  void $ Halogen.fork do
    Halogen.liftAff
      $ Effect.Aff.delay (Effect.Aff.Milliseconds 1000.0)
    Halogen.modify_ _ { copied = false }

render ::
  forall m.
  State ->
  ComponentHTML m
render state
  | state.copied = renderDone
  | otherwise = renderActive

renderActive :: forall m. ComponentHTML m
renderActive =
  Ocelot.Block.Button.button
    [ Halogen.HTML.Events.onClick \_ -> Copy ]
    [ Ocelot.Block.Icon.dataSources_
    , Halogen.HTML.span
        [ Ocelot.HTML.Properties.css "ml-2" ]
        [ Halogen.HTML.text "Copy to Clipboard" ]
    ]

renderDone :: forall m. ComponentHTML m
renderDone =
  Ocelot.Block.Button.button
    [ Halogen.HTML.Propetires.disabled true ]
    [ Ocelot.Block.Icon.success
        [ Ocelot.HTML.Properties.css "text-green"]
    , Halogen.HTML.span
        [ Ocelot.HTML.Properties.css "ml-2" ]
        [ Halogen.HTML.text "Copied" ]
    ]
