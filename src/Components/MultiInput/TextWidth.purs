-- | A component to measure text width.
-- |
-- | Known issue:
-- | * If GetWidth is called with a high frequency, it may run into
-- |   race condition where `clientWidth` returns cached result before
-- |   new text is rendered and measured.
-- |   You may want to give every text its own slot if this happens.
module Ocelot.Components.MultiInput.TextWidth
  ( Query(..)
  , Output
  , Slot
  , component
  ) where

import Prelude

import Control.Monad.Maybe.Trans as Control.Monad.Maybe.Trans
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Web.DOM.Element as Web.DOM.Element
import Web.HTML.HTMLElement as Web.HTML.HTMLElement

type Slot = Halogen.Slot Query Output

type Component m = Halogen.Component Halogen.HTML.HTML Query Input Output m
type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m
type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State =
  { input :: Input
  , text :: String
  }

data Action
  = Receive Input

data Query a
  = GetWidth String (Number -> a)

type Input =
  { renderText :: String -> Halogen.HTML.PlainHTML
  }

type Output =
  Void

type ChildSlots =
  ()

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
            , handleQuery = handleQuery
            }
    }

initialState :: Input -> State
initialState input =
  { input
  , text: ""
  }

receive :: Input -> State -> State
receive input old =
  { input
  , text: old.text
  }

handleAction ::
  forall m.
  Action ->
  ComponentM m Unit
handleAction = case _ of
  Receive input -> Halogen.modify_ (receive input)

handleQuery ::
  forall a m.
  MonadAff m =>
  Query a ->
  ComponentM m (Maybe a)
handleQuery = case _ of
  GetWidth text reply -> do
    Halogen.modify_ _ { text = text }
    mWidth <- Control.Monad.Maybe.Trans.runMaybeT do
      htmlElement <-
        Control.Monad.Maybe.Trans.MaybeT
          $ Halogen.getHTMLElementRef ghostRef
      width <-
        Halogen.liftEffect
          $ Web.DOM.Element.clientWidth <<< Web.HTML.HTMLElement.toElement
          $ htmlElement
      pure width
    pure (reply <$> mWidth)

render ::
  forall m.
  State ->
  ComponentHTML m
render state =
  Halogen.HTML.div
    [ Halogen.HTML.Properties.classes containerClasses ]
    [ renderGhostElement state ]

renderGhostElement :: forall m. State -> ComponentHTML m
renderGhostElement state =
  Halogen.HTML.div
    [ Halogen.HTML.Properties.ref ghostRef
    , Halogen.HTML.Properties.classes ghostClasses
    ]
    [ Halogen.HTML.fromPlainHTML (state.input.renderText state.text) ]

containerClasses :: Array Halogen.ClassName
containerClasses =
  [ "relative"
  ]
    <#> Halogen.ClassName

ghostClasses :: Array Halogen.ClassName
ghostClasses =
  [ "absolute"
  , "h-0"
  , "inline-block"
  , "invisible"
  , "overflow-hidden"
  , "pin-t"
  , "whitespace-no-wrap"
  ]
    <#> Halogen.ClassName

-- | Ghost element to measure the minimum width for a no-wrap text input
ghostRef :: Halogen.RefLabel
ghostRef = Halogen.RefLabel "ghost"

