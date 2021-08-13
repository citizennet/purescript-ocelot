module Ocelot.FilePicker
  ( Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import DOM.HTML.Indexed.InputAcceptType as DOM.HTML.Indexed.InputAcceptType
import Data.Array as Data.Array
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as Foreign.Object
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Ocelot.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.Data.InputAcceptType as Ocelot.Data.InputAcceptType
import Ocelot.HTML.Properties as Ocelot.HTMl.Properties
import Web.Event.Event as Web.Event.Event
import Web.File.File as Web.File.File
import Web.File.FileList as Web.File.FileList
import Web.HTML.Event.DataTransfer as Web.HTML.Event.DataTransfer
import Web.HTML.Event.DragEvent as Web.HTML.Event.DragEvent

-- * id: a *globally* unique identifier for the <input /> element
-- * multiple
--   * true: allow selecting multiple files
--   * false: only allow selecting a single file
type Config
  = { accept :: Maybe DOM.HTML.Indexed.InputAcceptType.InputAcceptType
    , id :: String
    , multiple :: Boolean
    }

type Slot = Halogen.Slot Query Output

type Component m = Halogen.Component Query Input Output m
type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m
type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State =
  { config :: Config
  , dragOver :: Boolean
  }

data Action
  = ChooseFile (Array Web.File.File.File)
  | DragEnter Web.HTML.Event.DragEvent.DragEvent
  | DragLeave Web.HTML.Event.DragEvent.DragEvent
  | DropFile Web.HTML.Event.DragEvent.DragEvent
  | PreventDefault Web.Event.Event.Event

data Query (a :: Type)

type Input = Unit

data Output
  = Selected (Array Web.File.File.File)

type ChildSlots = () :: Row Type

component ::
  forall m.
  MonadAff m =>
  Config ->
  Component m
component config =
  Halogen.mkComponent
    { initialState: initialState config
    , render
    , eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction
            }
    }

initialState :: Config -> Input -> State
initialState config input =
  { config
  , dragOver: false
  }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  ChooseFile files -> chooseFile files
  DragEnter dragEvent -> dragEnter dragEvent
  DragLeave dragEvent -> dragLeave dragEvent
  DropFile dragEvent -> dropFile dragEvent
  PreventDefault event -> preventDefault event

chooseFile ::
  forall m.
  Array Web.File.File.File ->
  ComponentM m Unit
chooseFile files = do
  config <- Halogen.gets _.config
  let
    selected :: Array Web.File.File.File
    selected =
      files
        # filterByAccept config
        # filterByMultiple config
  Halogen.raise (Selected selected)
  where
  filterByAccept ::
    Config ->
    Array Web.File.File.File ->
    Array Web.File.File.File
  filterByAccept config = case config.accept of
    Nothing -> identity
    Just accept -> Data.Array.filter (Ocelot.Data.InputAcceptType.validate accept)

  filterByMultiple ::
    Config ->
    Array Web.File.File.File ->
    Array Web.File.File.File
  filterByMultiple config
    | config.multiple = identity
    | otherwise = Data.Array.take 1

dragEnter ::
  forall m.
  MonadAff m =>
  Web.HTML.Event.DragEvent.DragEvent ->
  ComponentM m Unit
dragEnter dragEvent = do
  preventDefault (Web.HTML.Event.DragEvent.toEvent dragEvent)
  Halogen.modify_ _ { dragOver = true }

dragLeave ::
  forall m.
  MonadAff m =>
  Web.HTML.Event.DragEvent.DragEvent ->
  ComponentM m Unit
dragLeave dragEvent = do
  preventDefault (Web.HTML.Event.DragEvent.toEvent dragEvent)
  Halogen.modify_ _ { dragOver = false }

dropFile ::
  forall m.
  MonadAff m =>
  Web.HTML.Event.DragEvent.DragEvent ->
  ComponentM m Unit
dropFile dragEvent = do
  preventDefault (Web.HTML.Event.DragEvent.toEvent dragEvent)
  Halogen.modify_ _ { dragOver = false }
  case Web.HTML.Event.DataTransfer.files (Web.HTML.Event.DragEvent.dataTransfer dragEvent) of
    Nothing -> pure unit
    Just fileList -> do
      let
        files :: Array Web.File.File.File
        files = Web.File.FileList.items fileList
      chooseFile files

preventDefault ::
  forall m.
  MonadAff m =>
  Web.Event.Event.Event ->
  ComponentM m Unit
preventDefault event =
  Halogen.liftEffect do
    Web.Event.Event.preventDefault event
    Web.Event.Event.stopPropagation event

render ::
  forall m.
  State ->
  ComponentHTML m
render state =
  Halogen.HTML.div_
    [ renderDropBox state
    , renderInput state
    ]

renderDropBox ::
  forall m.
  State ->
  ComponentHTML m
renderDropBox state
  | state.dragOver =
    Halogen.HTML.div ipropDragOver
      [ Halogen.HTML.div
          [ Ocelot.HTMl.Properties.style <<< Foreign.Object.fromHomogeneous $
            { "pointer-events": "none" } -- NOTE prevent event firing from children
          ]
          (renderContent state)
      ]
  | otherwise =
    Halogen.HTML.div ipropIdle
      (renderContent state)

ipropIdle :: Array (Halogen.HTML.Properties.IProp HTMLdiv Action)
ipropIdle =
  [ Ocelot.HTMl.Properties.css cssIdle
  , Ocelot.HTMl.Properties.style styleIdle
  , Ocelot.HTML.Events.onDrag (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDragEnter DragEnter
  , Ocelot.HTML.Events.onDragOver (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDragStart (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  ]
  where
  cssIdle :: String
  cssIdle = "bg-grey-70-a40 p-10"

  styleIdle :: Foreign.Object.Object String
  styleIdle =
    Foreign.Object.fromHomogeneous
      { outline: "2px dashed #8f9eb3"
      , "outline-offset": "-10px"
      , transition: "outline-offset .15s ease-in-out, background-color .15s linear"
      }

ipropDragOver :: Array (Halogen.HTML.Properties.IProp HTMLdiv Action)
ipropDragOver =
  [ Ocelot.HTMl.Properties.css cssDragOver
  , Ocelot.HTMl.Properties.style styleDragOver
  , Ocelot.HTML.Events.onDragEnd (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDragLeave DragLeave
  , Ocelot.HTML.Events.onDragOver (PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDrop DropFile
  ]
  where
  cssDragOver :: String
  cssDragOver = "bg-grey-95 p-10"

  styleDragOver :: Foreign.Object.Object String
  styleDragOver =
    Foreign.Object.fromHomogeneous
    { outline: "2px dashed #8f9eb3"
    , "outline-offset": "-20px"
    , transition: "outline-offset .15s ease-in-out, background-color .15s linear"
    }

renderContent ::
  forall m.
  State ->
  Array (ComponentHTML m)
renderContent state =
  [ renderIcon
  , renderLabel state
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
  State ->
  ComponentHTML m
renderInput state =
  Halogen.HTML.input
    ( [ Ocelot.HTMl.Properties.css "hidden"
      , Halogen.HTML.Properties.id_ state.config.id
      , Halogen.HTML.Properties.type_ Halogen.HTML.Properties.InputFile
      , Ocelot.HTML.Events.onFileUpload ChooseFile
      , Halogen.HTML.Properties.multiple state.config.multiple
      ]
        <> case state.config.accept of
          Nothing -> []
          Just inputType -> [ Halogen.HTML.Properties.accept inputType ]
    )

renderLabel ::
  forall m.
  State ->
  ComponentHTML m
renderLabel state =
  Halogen.HTML.label
    [ Ocelot.HTMl.Properties.css "group text-center w-full"
    , Halogen.HTML.Properties.for state.config.id
    ]
    [ Halogen.HTML.span
      [ Ocelot.HTMl.Properties.css "group-hover:text-blue-88" ]
      [ Halogen.HTML.text
          if state.config.multiple then
            "Choose file(s)"
          else
            "Choose a file"
      ]
    , Halogen.HTML.text " or drag it here."
    ]
