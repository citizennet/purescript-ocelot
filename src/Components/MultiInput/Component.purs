module Ocelot.Components.MultiInput.Component
  ( Output
  , Query(..)
  , Slot
  , component
  ) where

import Prelude

import Control.Monad.Maybe.Trans as Control.Monad.Maybe.Trans
import Data.Array as Data.Array
import Data.FunctorWithIndex as Data.FunctorWithIndex
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.Components.MultiInput.TextWidth as Ocelot.Components.MultiInput.TextWidth
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Web.Event.Event as Web.Event.Event
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.UIEvent.KeyboardEvent as Web.UIEvent.KeyboardEvent

type Slot = Halogen.Slot Query Output

type Component m = Halogen.Component Halogen.HTML.HTML Query Input Output m
type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m
type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State =
  { inputBox :: InputBox
  , selections :: Array String
  }

type InputBox =
  { text :: String
  , width :: Number
  }

data Action
  = OnInput String
  | OnKeyDown Web.UIEvent.KeyboardEvent.KeyboardEvent
  | RemoveOne Int

data Query a

type Input =
  Unit

type Output =
  Void

type ChildSlots =
  ( textWidth :: Ocelot.Components.MultiInput.TextWidth.Slot Unit
  )

_textWidth = SProxy :: SProxy "textWidth"

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
            }
    }

emptyInputBox :: InputBox
emptyInputBox =
  { text: ""
  , width: minWidth
  }

initialState :: Input -> State
initialState _ =
  { inputBox: emptyInputBox
  , selections: []
  }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  OnInput text -> handleOnInput text
  OnKeyDown keyboardEvent -> handleOnKeyDown keyboardEvent
  RemoveOne index -> handleRemoveOne index

handleOnInput ::
  forall m.
  MonadAff m =>
  String ->
  ComponentM m Unit
handleOnInput text = do
  Halogen.modify_ _ { inputBox { text = text } }
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    width <-
      Control.Monad.Maybe.Trans.MaybeT
        $ Halogen.query _textWidth unit <<< Halogen.request
        $ Ocelot.Components.MultiInput.TextWidth.GetWidth text
    Control.Monad.Maybe.Trans.lift
      $ Halogen.modify_ _ { inputBox { width = max minWidth width } }

handleOnKeyDown ::
  forall m.
  MonadAff m =>
  Web.UIEvent.KeyboardEvent.KeyboardEvent ->
  ComponentM m Unit
handleOnKeyDown keyboardEvent = do
  case Web.UIEvent.KeyboardEvent.key keyboardEvent of
    "Enter" -> do
      preventDefault keyboardEvent
      handlePressEnter
    _ -> pure unit

handlePressEnter ::
  forall m.
  MonadAff m =>
  ComponentM m Unit
handlePressEnter = do
  Halogen.modify_ \old ->
    old
      { inputBox = emptyInputBox
      , selections = old.selections `Data.Array.snoc` old.inputBox.text
      }
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    htmlElement <-
      Control.Monad.Maybe.Trans.MaybeT
        $ Halogen.getHTMLElementRef inputRef
    Halogen.liftEffect
      $ Web.HTML.HTMLElement.focus htmlElement

handleRemoveOne ::
  forall m.
  Int ->
  ComponentM m Unit
handleRemoveOne index = do
  Halogen.modify_ \old ->
    old
      { selections =
          Data.Array.deleteAt index old.selections
            # fromMaybe old.selections
      }

preventDefault ::
  forall m.
  MonadAff m =>
  Web.UIEvent.KeyboardEvent.KeyboardEvent ->
  ComponentM m Unit
preventDefault keyboardEvent = do
  Halogen.liftEffect
    $ Web.Event.Event.preventDefault <<< Web.UIEvent.KeyboardEvent.toEvent
    $ keyboardEvent

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML m
render state =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "bg-white border w-full rounded px-2" ]
    [ Halogen.HTML.div_
        ( [ Data.FunctorWithIndex.mapWithIndex renderItem state.selections
          , [ renderAutoSizeInput state.inputBox ]
          ]
            # join
        )
    , renderTextWidth
    ]

renderItem :: forall m. Int -> String -> ComponentHTML m
renderItem index str =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "inline-block mx-1" ]
    [ Halogen.HTML.text str
    , Halogen.HTML.button
        [ Halogen.HTML.Properties.classes closeButtonClasses
        , Halogen.HTML.Events.onClick \_ -> Just (RemoveOne index)
        ]
        [ Ocelot.Block.Icon.delete_ ]
    ]

renderAutoSizeInput :: forall m. InputBox -> ComponentHTML m
renderAutoSizeInput inputBox =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "inline-block" ]
    [ Halogen.HTML.input
        [ Halogen.HTML.Properties.attr (Halogen.HTML.AttrName "style") css
        , Halogen.HTML.Properties.classes inputClasses
        , Halogen.HTML.Events.onValueInput (Just <<< OnInput)
        , Halogen.HTML.Events.onKeyDown (Just <<< OnKeyDown)
        , Halogen.HTML.Properties.ref inputRef
        , Halogen.HTML.Properties.type_ Halogen.HTML.Properties.InputText
        , Halogen.HTML.Properties.value inputBox.text
        ]
    ]
  where
  css :: String
  css = "width: " <> show inputBox.width <> "px"

renderTextWidth ::
  forall m.
  MonadAff m =>
  ComponentHTML m
renderTextWidth =
  Halogen.HTML.slot _textWidth unit
    Ocelot.Components.MultiInput.TextWidth.component
    { renderText }
    absurd
  where
  renderText :: String -> Halogen.HTML.PlainHTML
  renderText str =
    Halogen.HTML.span
      [ Halogen.HTML.Properties.classes inputClasses ]
      [ Halogen.HTML.text str ]

closeButtonClasses :: Array Halogen.ClassName
closeButtonClasses =
  [ "!active:border-b"
  , "!disabled:cursor-pointer"
  , "active:border-t"
  , "bg-transparent"
  , "border-transparent"
  , "disabled:cursor-default"
  , "disabled:opacity-50"
  , "focus:text-grey-70-a30"
  , "hover:text-grey-70-a30"
  , "no-outline"
  , "px-1"
  , "text-grey-70"
  , "text-sm"
  ]
    <#> Halogen.ClassName

inputClasses :: Array Halogen.ClassName
inputClasses =
  [ "outline-none"
  , "px-1"
  ]
    <#> Halogen.ClassName

-- | Input element whose width is adjusted automatically
inputRef :: Halogen.RefLabel
inputRef = Halogen.RefLabel "input"

minWidth :: Number
minWidth = 50.0

