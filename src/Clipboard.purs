module Ocelot.Clipboard
  ( Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Effect.Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Uncurried as Effect.Uncurried
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Propetires
import Ocelot.Block.Button as Ocelot.Block.Button
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Web.HTML as Web.HTML
import Web.HTML.Navigator as Web.HTML.Navigator
import Web.HTML.Window as Web.HTML.Window

type Slot
  = Halogen.Slot Query Output

type Component m
  = Halogen.Component Query Input Output m

type ComponentHTML m
  = Halogen.ComponentHTML Action ChildSlot m

type ComponentM m
  = Halogen.HalogenM State Action ChildSlot Output m

type State
  = { copied :: Boolean
    , input :: Input
    }

data Action
  = Copy
  | Receive Input

data Query (a :: Type)

type Input
  = { text :: String }

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
            , receive = Just <<< Receive
            }
    , initialState
    , render
    }

initialState :: Input -> State
initialState input =
  { copied: false
  , input
  }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  Copy -> copy
  Receive input -> receive input

copy ::
  forall m.
  MonadAff m =>
  ComponentM m Unit
copy = do
  input <- Halogen.gets _.input
  Halogen.liftEffect
    $ copyToClipboard input.text
  Halogen.modify_ _ { copied = true }
  void $ Halogen.fork do
    Halogen.liftAff
      $ Effect.Aff.delay (Effect.Aff.Milliseconds 1000.0)
    Halogen.modify_ _ { copied = false }

copyToClipboard ::
  String ->
  Effect Unit
copyToClipboard text = do
  window <- Web.HTML.window
  navigator <- Web.HTML.Window.navigator window
  writeText navigator text

receive ::
  forall m.
  Input ->
  ComponentM m Unit
receive input = do
  Halogen.modify_ _ { input = input }

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

foreign import _writeText ::
  Effect.Uncurried.EffectFn1
    { navigator :: Web.HTML.Navigator.Navigator
    , text :: String
    }
    Unit

writeText ::
  Web.HTML.Navigator.Navigator ->
  String ->
  Effect Unit
writeText navigator text =
  Effect.Uncurried.runEffectFn1 _writeText
    { navigator
    , text
    }
