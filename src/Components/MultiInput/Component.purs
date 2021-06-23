module Ocelot.Components.MultiInput.Component
  ( HTMLEvents(..)
  , Input
  , Output(..)
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
import Effect.Aff.Class (class MonadAff)
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.Components.MultiInput.TextWidth as Ocelot.Components.MultiInput.TextWidth
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Type.Proxy (Proxy(..))
import Web.Event.Event as Web.Event.Event
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.UIEvent.KeyboardEvent as Web.UIEvent.KeyboardEvent
import Web.UIEvent.MouseEvent as Web.UIEvent.MouseEvent

type Slot = Halogen.Slot Query Output

type Component m = Halogen.Component Query Input Output m
type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m
type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State =
  { input :: Input
  , items :: Array InputStatus
  , placeholder :: Placeholder
  , new :: InputStatus
  }

data InputStatus
  = Display { text :: String }
  | Edit { inputBox :: InputBox, previous :: String }
  | New { inputBox :: InputBox }

type InputBox =
  { text :: String
  , width :: Number
  }

type Placeholder =
  { primary :: InputBox
  , secondary :: InputBox
  }

data Action
  = EditItem Int
  | Initialize
  | OnBlur Int
  | OnFocus Int
  | OnInput Int String
  | OnKeyDown Int Web.UIEvent.KeyboardEvent.KeyboardEvent
  | OnMouseDown Int Web.UIEvent.MouseEvent.MouseEvent
  | Receive Input
  | RemoveOne Int

data Query a
  = GetItems (Array String -> a)
  | SetItems (Array String) a
  | SelectItem String a

-- | * minWidth: minimum width of input box for new item
-- | * placeholder: placeholder text in new item input
-- |   * primary: when there is no item
-- |   * secondary: when there are existing items
type Input =
  { minWidth :: Number {- px -}
  , placeholder ::
      { primary :: String
      , secondary :: String
      }
  }

data Output
  = ItemsUpdated (Array String) -- caused by user actions
  | On HTMLEvents

data HTMLEvents
  = Blur
  | Focus
  | KeyDown Web.UIEvent.KeyboardEvent.KeyboardEvent
  | MouseDown Web.UIEvent.MouseEvent.MouseEvent
  | ValueInput String

type ChildSlots =
  ( textWidth :: Ocelot.Components.MultiInput.TextWidth.Slot Unit
  )

_textWidth = Proxy :: Proxy "textWidth"

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
            , initialize = Just Initialize
            , receive = Just <<< Receive
            }
    }

emptyInputBox :: Number -> InputBox
emptyInputBox minWidth =
  { text: ""
  , width: minWidth
  }

initialState :: Input -> State
initialState input =
  { input
  , items: [ new ]
  , placeholder
  , new
  }
  where
  placeholder :: Placeholder
  placeholder =
    { primary: { text: input.placeholder.primary, width: input.minWidth }
    , secondary: { text: input.placeholder.secondary, width: input.minWidth }
    }

  new :: InputStatus
  new = New { inputBox: emptyInputBox input.minWidth }

receive :: Input -> State -> State
receive input old =
  { input
  , items: old.items
  , placeholder
  , new
  }
  where
  placeholder :: Placeholder
  placeholder =
    { primary: { text: input.placeholder.primary, width: input.minWidth }
    , secondary: { text: input.placeholder.secondary, width: input.minWidth }
    }

  new :: InputStatus
  new = New { inputBox: emptyInputBox input.minWidth }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  EditItem index -> handleEditItem index
  Initialize -> handleInitialize
  OnBlur index -> handleOnBlur index
  OnFocus index -> handleOnFocus index
  OnInput index text -> handleOnInput index text
  OnKeyDown index keyboardEvent -> handleOnKeyDown index keyboardEvent
  OnMouseDown index mouseEvent -> do
    focusItem index
    Halogen.raise (On (MouseDown mouseEvent))
  Receive input -> handleReceive input
  RemoveOne index -> handleRemoveOne index

handleQuery ::
  forall a m.
  MonadAff m =>
  Query a ->
  ComponentM m (Maybe a)
handleQuery = case _ of
  GetItems reply -> do
    items <- Halogen.gets (getTexts <<< _.items)
    pure (Just (reply items))
  SetItems items a -> do
    handleSetItems items
    pure (Just a)
  SelectItem text a -> selectItem text $> Just a

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
            Edit _ -> item
            New _ -> item
        )
    Control.Monad.Maybe.Trans.lift
      $ focusItem index

handleInitialize ::
  forall m.
  MonadAff m =>
  ComponentM m Unit
handleInitialize = do
  calibratePlaceholderWidth

handleOnBlur ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
handleOnBlur index = do
  commitEditing index
  Halogen.raise (On Blur)

handleOnFocus ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
handleOnFocus index = do
  old <- Halogen.get
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    item <-
      Control.Monad.Maybe.Trans.MaybeT <<< pure $ do
        Data.Array.index old.items index
    Control.Monad.Maybe.Trans.lift do
      case item of
        Display _ -> pure unit
        Edit { inputBox: { text } } -> do
          Halogen.raise (On (ValueInput text ))
          Halogen.raise (On Focus)
        New { inputBox: { text }} -> do
          Halogen.raise (On (ValueInput text ))
          Halogen.raise (On Focus)

handleOnInput ::
  forall m.
  MonadAff m =>
  Int ->
  String ->
  ComponentM m Unit
handleOnInput index text = do
  minWidth <- Halogen.gets _.input.minWidth
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
  Halogen.raise (On (ValueInput text))

handleOnKeyDown ::
  forall m.
  MonadAff m =>
  Int ->
  Web.UIEvent.KeyboardEvent.KeyboardEvent ->
  ComponentM m Unit
handleOnKeyDown index keyboardEvent = do
  Halogen.raise (On (KeyDown keyboardEvent))
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

handleReceive ::
  forall m.
  MonadAff m =>
  Input ->
  ComponentM m Unit
handleReceive input = do
  old <- Halogen.get
  -- NOTE a workaround to reduce rate of re-calibration
  -- see comments in TextWidth for details
  when (old.input /= input) do
    Halogen.modify_ (receive input)
    calibratePlaceholderWidth

handleRemoveOne ::
  forall m.
  MonadAff m =>
  Int ->
  ComponentM m Unit
handleRemoveOne index = do
  removeItem index
  raiseItemUpdated

handleSetItems ::
  forall m.
  Array String ->
  ComponentM m Unit
handleSetItems items = do
  Halogen.modify_ \old ->
    old { items = (display <$> items) `Data.Array.snoc` old.new }
  where
  display :: String -> InputStatus
  display text = Display { text }

appendNewItem ::
  forall m.
  MonadAff m =>
  ComponentM m Unit
appendNewItem = do
  Halogen.modify_ \old ->
    old
      { items =
          old.items `Data.Array.snoc` old.new
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

calibratePlaceholderWidth ::
  forall m.
  MonadAff m =>
  ComponentM m Unit
calibratePlaceholderWidth = do
  state <- Halogen.get
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    width <-
      Control.Monad.Maybe.Trans.MaybeT
      $ measureTextWidth state.placeholder.primary.text
    Halogen.modify _ { placeholder { primary { width = width } } }
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    width <-
      Control.Monad.Maybe.Trans.MaybeT
      $ measureTextWidth state.placeholder.secondary.text
    Halogen.modify_ _ { placeholder { secondary { width = width } } }

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
        New _ -> do
          void $ updateItem index (\_ -> old.new)

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
            raiseItemUpdated
          | otherwise -> do
            void $ updateItem index (\_ -> Display { text })
            raiseItemUpdated
        New { inputBox: { text } }
          | Data.String.null text -> pure unit
          | otherwise -> do
            new <- updateItem index (\_ -> Display { text })
            when (isLastIndex index new.items) do
              appendNewItem
            raiseItemUpdated

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
  Halogen.request _textWidth unit
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

raiseItemUpdated :: forall m. ComponentM m Unit
raiseItemUpdated = do
  items <- Halogen.gets (getTexts <<< _.items)
  Halogen.raise (ItemsUpdated items)

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

selectItem ::
  forall m.
  MonadAff m =>
  String ->
  ComponentM m Unit
selectItem text = do
  old <- Halogen.get
  void $ Control.Monad.Maybe.Trans.runMaybeT do
    index <-
      Control.Monad.Maybe.Trans.MaybeT <<< pure
        $ Data.Array.findIndex isEditable old.items
    width <-
      Control.Monad.Maybe.Trans.MaybeT
         $ measureTextWidth text
    void <<< Control.Monad.Maybe.Trans.lift
        $ updateItem index
          ( \item -> case item of
              Display _ -> item
              Edit status -> Edit status { inputBox = { width, text } }
              New status -> New status { inputBox = { width, text } }
          )
    Control.Monad.Maybe.Trans.lift do
      commitEditing index
      when (isLastIndex index old.items) do
        new <- Halogen.get
        focusItem (getLastIndex new.items)

  where
  isEditable :: InputStatus -> Boolean
  isEditable = case _ of
    Display _ -> false
    Edit _ -> true
    New _ -> true

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

getTexts :: Array InputStatus -> Array String
getTexts = Data.Array.mapMaybe getText

getText :: InputStatus -> Maybe String
getText = case _ of
  Display { text } -> Just text
  Edit { previous } -> Just previous
  New _ -> Nothing

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML m
render state =
  Halogen.HTML.div
    [ Halogen.HTML.Properties.classes containerClasses  ]
    [ Halogen.HTML.div_
        (Data.FunctorWithIndex.mapWithIndex (renderItem state.placeholder) state.items)
    , renderTextWidth
    ]

renderItem ::
  forall m.
  Placeholder ->
  Int ->
  InputStatus ->
  ComponentHTML m
renderItem placeholder index = case _ of
  Display { text } -> renderItemDisplay index text
  Edit { inputBox } -> renderItemEdit placeholder index inputBox
  New { inputBox } -> renderAutoSizeInput placeholder index true inputBox

renderItemDisplay ::
  forall m.
  Int ->
  String ->
  ComponentHTML m
renderItemDisplay index text =
  Halogen.HTML.div
    [ Halogen.HTML.Properties.classes itemDisplayClasses ]
    [ Halogen.HTML.span
        [ Halogen.HTML.Events.onClick \_ -> EditItem index ]
        [ Halogen.HTML.text text ]
    , Halogen.HTML.button
        [ Halogen.HTML.Properties.classes closeButtonClasses
        , Halogen.HTML.Events.onClick \_ -> RemoveOne index
        ]
        [ Ocelot.Block.Icon.delete_ ]
    ]

renderItemEdit ::
  forall m.
  Placeholder ->
  Int ->
  InputBox ->
  ComponentHTML m
renderItemEdit placeholder index inputBox =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "inline-block" ]
    [ renderAutoSizeInput placeholder index false inputBox
    , Halogen.HTML.button
        [ Halogen.HTML.Properties.classes closeButtonClasses
        , Halogen.HTML.Events.onClick \_ -> RemoveOne index
        ]
        [ Ocelot.Block.Icon.delete_ ]
    ]

renderAutoSizeInput ::
  forall m.
  Placeholder ->
  Int ->
  Boolean ->
  InputBox ->
  ComponentHTML m
renderAutoSizeInput placeholder index new inputBox =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "inline-block" ]
    [ Halogen.HTML.input
        [ Halogen.HTML.Properties.attr (Halogen.HTML.AttrName "style") css
        , Halogen.HTML.Properties.classes inputClasses
        , Halogen.HTML.Events.onBlur \_ -> OnBlur index
        , Halogen.HTML.Events.onFocus \_ -> OnFocus index
        , Halogen.HTML.Events.onKeyDown (OnKeyDown index)
        , Halogen.HTML.Events.onMouseDown (OnMouseDown index)
        , Halogen.HTML.Events.onValueInput (OnInput index)
        , Halogen.HTML.Properties.placeholder case new of
            false -> ""
            true
              | index == 0 -> placeholder.primary.text
              | otherwise -> placeholder.secondary.text
        , Halogen.HTML.Properties.ref (inputRef index)
        , Halogen.HTML.Properties.type_ Halogen.HTML.Properties.InputText
        , Halogen.HTML.Properties.value inputBox.text
        ]
    ]
  where
  css :: String
  css = "width: " <> show width <> "px"

  width :: Number
  width
    | Data.String.null inputBox.text && new =
      if index == 0 then
        placeholder.primary.width
      else
        placeholder.secondary.width
    | otherwise = inputBox.width

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

