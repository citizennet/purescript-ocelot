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
import Data.Maybe as Data.Maybe
import Data.String as Data.String
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
  { items :: Array InputStatus
  }

data InputStatus
  = Display { text :: String }
  | Edit { inputBox :: InputBox, previous :: String }
  | New { inputBox :: InputBox }

type InputBox =
  { text :: String
  , width :: Number
  }

data Action
  = EditItem Int
  | OnBlur Int
  | OnInput Int String
  | OnKeyDown Int Web.UIEvent.KeyboardEvent.KeyboardEvent
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
  { items:
    [ Display { text: "abcdef" } -- TODO debug
    , New { inputBox: emptyInputBox }
    ]
  }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  EditItem index -> handleEditItem index
  OnBlur index -> handleOnBlur index
  OnInput index text -> handleOnInput index text
  OnKeyDown index keyboardEvent -> handleOnKeyDown index keyboardEvent
  RemoveOne index -> handleRemoveOne index

handleEditItem ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
handleEditItem index = do
  old <- Halogen.get
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    text <-
      Control.Monad.Maybe.Trans.MaybeT <<< pure $ do
        item <- Data.Array.index old.items index
        case item of
          Display { text } -> Just text
          Edit _ -> Nothing
          New _ -> Nothing
    width <-
      Control.Monad.Maybe.Trans.MaybeT
        $ measureTextWidth text
    void <<< Control.Monad.Maybe.Trans.lift
      $ updateItem index
        ( \item -> case item of
            Display _ -> Edit { inputBox: { text, width }, previous: text }
            Edit status -> item
            New status -> item
        )
    Control.Monad.Maybe.Trans.lift
      $ focusItem index

handleOnBlur ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
handleOnBlur index = do
  commitEditing index

handleOnInput ::
  forall m.
  MonadAff m =>
  Int ->
  String ->
  ComponentM m Unit
handleOnInput index text = do
  void
    $ updateItem index
      ( \item -> case item of
          Display _ -> item
          Edit status -> Edit status { inputBox { text = text } }
          New status -> New status { inputBox { text = text } }
      )
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    width <-
      Control.Monad.Maybe.Trans.MaybeT
        $ measureTextWidth text
    Control.Monad.Maybe.Trans.lift
      $ updateItem index
        ( \item -> case item of
              Display _ -> item
              Edit status -> Edit status { inputBox { width = width } }
              New status -> New status { inputBox { width = max minWidth width } }
        )

handleOnKeyDown ::
  forall m.
  MonadAff m =>
  Int ->
  Web.UIEvent.KeyboardEvent.KeyboardEvent ->
  ComponentM m Unit
handleOnKeyDown index keyboardEvent = do
  case Web.UIEvent.KeyboardEvent.key keyboardEvent of
    "Enter" -> do
      preventDefault keyboardEvent
      handlePressEnter index
    "Escape" -> do
      preventDefault keyboardEvent
      handlePressEsc index
    _ -> pure unit

handlePressEnter ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
handlePressEnter index = do
  old <- Halogen.get
  commitEditing index
  when (isLastIndex index old.items) do
    new <- Halogen.get
    focusItem (getLastIndex new.items)

handlePressEsc ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
handlePressEsc index = do
  cancelEditing index
  blurItem index

handleRemoveOne ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
handleRemoveOne index = do
  removeItem index

appendNewItem ::
  forall m.
  MonadAff m =>
  ComponentM m Unit
appendNewItem = do
  Halogen.modify_ \old ->
    old
      { items =
          old.items `Data.Array.snoc` New { inputBox: emptyInputBox }
      }

blurItem ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
blurItem index = do
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    htmlElement <-
      Control.Monad.Maybe.Trans.MaybeT
      $ Halogen.getHTMLElementRef (inputRef index)
    Halogen.liftEffect
      $ Web.HTML.HTMLElement.blur htmlElement

cancelEditing ::
  forall m.
  Int ->
  ComponentM m Unit
cancelEditing index = do
  old <- Halogen.get
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    item <-
      Control.Monad.Maybe.Trans.MaybeT <<< pure $ do
        Data.Array.index old.items index
    Control.Monad.Maybe.Trans.lift do
      case item of
        Display _ -> pure unit
        Edit { previous } -> do
          void $ updateItem index (\_ -> Display { text: previous })
        New { inputBox: { text } } -> do
          void $ updateItem index (\_ -> New { inputBox: emptyInputBox })

commitEditing ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
commitEditing index = do
  old <- Halogen.get
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    item <-
      Control.Monad.Maybe.Trans.MaybeT <<< pure $ do
        Data.Array.index old.items index
    Control.Monad.Maybe.Trans.lift do
      case item of
        Display _ -> pure unit
        Edit { inputBox: { text } }
          | Data.String.null text -> do
            removeItem index
          | otherwise -> do
            void $ updateItem index (\_ -> Display { text })
        New { inputBox: { text } }
          | Data.String.null text -> pure unit
          | otherwise -> do
            new <- updateItem index (\_ -> Display { text })
            when (isLastIndex index new.items) do
              appendNewItem

focusItem ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
focusItem index = do
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    htmlElement <-
      Control.Monad.Maybe.Trans.MaybeT
      $ Halogen.getHTMLElementRef (inputRef index)
    Halogen.liftEffect
      $ Web.HTML.HTMLElement.focus htmlElement

measureTextWidth ::
  forall m.
  MonadAff m =>
  String ->
  ComponentM m (Maybe Number)
measureTextWidth text = do
  Halogen.query _textWidth unit <<< Halogen.request
    $ Ocelot.Components.MultiInput.TextWidth.GetWidth text

preventDefault ::
  forall m.
  MonadAff m =>
  Web.UIEvent.KeyboardEvent.KeyboardEvent ->
  ComponentM m Unit
preventDefault keyboardEvent = do
  Halogen.liftEffect
    $ Web.Event.Event.preventDefault <<< Web.UIEvent.KeyboardEvent.toEvent
    $ keyboardEvent

removeItem ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
removeItem index = do
  new <- Halogen.modify \old ->
    old
      { items =
          Data.Array.deleteAt index old.items
            # fromMaybe old.items
      }
  when (Data.Array.null new.items) do
    appendNewItem

updateItem ::
  forall m.
  Int ->
  (InputStatus -> InputStatus) ->
  ComponentM m State
updateItem index f = do
  Halogen.modify \old ->
    old
      { items =
          Data.Array.modifyAt index f old.items
            # Data.Maybe.fromMaybe old.items
      }

isLastIndex :: forall a. Int -> Array a -> Boolean
isLastIndex index xs = index == getLastIndex xs

getLastIndex :: forall a. Array a -> Int
getLastIndex xs = Data.Array.length xs - 1

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML m
render state =
  Halogen.HTML.div
    [ Halogen.HTML.Properties.classes containerClasses  ]
    [ Halogen.HTML.div_
        (Data.FunctorWithIndex.mapWithIndex renderItem state.items)
    , renderTextWidth
    ]

renderItem :: forall m. Int -> InputStatus -> ComponentHTML m
renderItem index = case _ of
  Display { text } -> renderItemDisplay index text
  Edit { inputBox } -> renderItemEdit index inputBox
  New { inputBox } -> renderAutoSizeInput index inputBox

renderItemDisplay ::
  forall m.
  Int ->
  String ->
  ComponentHTML m
renderItemDisplay index text =
  Halogen.HTML.div
    [ Halogen.HTML.Properties.classes itemDisplayClasses ]
    [ Halogen.HTML.span
        [ Halogen.HTML.Events.onClick \_ -> Just (EditItem index) ]
        [ Halogen.HTML.text text ]
    , Halogen.HTML.button
        [ Halogen.HTML.Properties.classes closeButtonClasses
        , Halogen.HTML.Events.onClick \_ -> Just (RemoveOne index)
        ]
        [ Ocelot.Block.Icon.delete_ ]
    ]

renderItemEdit ::
  forall m.
  Int ->
  InputBox ->
  ComponentHTML m
renderItemEdit index inputBox =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "inline-block" ]
    [ renderAutoSizeInput index inputBox
    , Halogen.HTML.button
        [ Halogen.HTML.Properties.classes closeButtonClasses
        , Halogen.HTML.Events.onClick \_ -> Just (RemoveOne index)
        ]
        [ Ocelot.Block.Icon.delete_ ]
    ]

renderAutoSizeInput ::
  forall m.
  Int ->
  InputBox ->
  ComponentHTML m
renderAutoSizeInput index inputBox =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "inline-block" ]
    [ Halogen.HTML.input
        [ Halogen.HTML.Properties.attr (Halogen.HTML.AttrName "style") css
        , Halogen.HTML.Properties.classes inputClasses
        , Halogen.HTML.Events.onBlur \_ -> Just (OnBlur index)
        , Halogen.HTML.Events.onKeyDown (Just <<< OnKeyDown index)
        , Halogen.HTML.Events.onValueInput (Just <<< OnInput index)
        , Halogen.HTML.Properties.ref (inputRef index)
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
  , "align-middle"
  , "bg-transparent"
  , "border-transparent"
  , "disabled:cursor-default"
  , "disabled:opacity-50"
  , "focus:text-grey-70-a30"
  , "hover:text-grey-70-a30"
  , "no-outline"
  , "pl-1"
  , "text-grey-70"
  , "text-xs"
  ]
    <#> Halogen.ClassName

containerClasses :: Array Halogen.ClassName
containerClasses =
  [ "bg-white"
  , "border"
  , "px-2"
  , "rounded"
  , "w-full"
  ]
    <#> Halogen.ClassName

inputClasses :: Array Halogen.ClassName
inputClasses =
  [ "my-1"
  , "outline-none"
  , "px-1"
  ]
    <#> Halogen.ClassName

itemDisplayClasses :: Array Halogen.ClassName
itemDisplayClasses =
  [ "bg-grey-95"
  , "inline-block"
  , "m-1"
  , "px-2"
  , "rounded-lg"
  ]
    <#> Halogen.ClassName

-- | Input element whose width is adjusted automatically
inputRef :: Int -> Halogen.RefLabel
inputRef index = Halogen.RefLabel ("input-" <> show index)

minWidth :: Number
minWidth = 50.0

