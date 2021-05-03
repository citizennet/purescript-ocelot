module Ocelot.FilePicker
  ( Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Effect.Class.Console
import Foreign.Object as Foreign.Object
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Ocelot.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.HTML.Properties as Ocelot.HTMl.Properties
import Web.Event.Event as Web.Event.Event
import Web.File.File as Web.File.File
import Web.File.FileList as Web.File.FileList
import Web.HTML.Event.DataTransfer as Web.HTML.Event.DataTransfer
import Web.HTML.Event.DragEvent as Web.HTML.Event.DragEvent

type Slot = Halogen.Slot Query Output

type Component m = Halogen.Component Halogen.HTML.HTML Query Input Output m
type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m
type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State =
  { dragOver :: Boolean
  }

data Action
  = DragEnter Web.HTML.Event.DragEvent.DragEvent
  | DragLeave Web.HTML.Event.DragEvent.DragEvent
  | DropFile Web.HTML.Event.DragEvent.DragEvent
  | PreventDefault Web.Event.Event.Event

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
            { handleAction = handleAction
            }
    }

initialState :: Input -> State
initialState input =
  { dragOver: false
  }

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  DragEnter dragEvent -> dragEnter dragEvent
  DragLeave dragEvent -> dragLeave dragEvent
  DropFile dragEvent -> dropFile dragEvent
  PreventDefault event -> preventDefault event

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
      Halogen.liftEffect -- TODO debug
        $ Effect.Class.Console.logShow
        $ map Web.File.File.name
        $ files
      pure unit -- TODO handle file

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
    , renderInput
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
          renderContent
      ]
  | otherwise =
    Halogen.HTML.div ipropIdle
      renderContent

ipropIdle :: Array (Halogen.HTML.Properties.IProp HTMLdiv Action)
ipropIdle =
  [ Ocelot.HTMl.Properties.css cssIdle
  , Ocelot.HTMl.Properties.style styleIdle
  , Ocelot.HTML.Events.onDrag (Just <<< PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDragEnter (Just <<< DragEnter)
  , Ocelot.HTML.Events.onDragOver (Just <<< PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDragStart (Just <<< PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
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
  , Ocelot.HTML.Events.onDragEnd (Just <<< PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDragLeave (Just <<< DragLeave)
  , Ocelot.HTML.Events.onDragOver (Just <<< PreventDefault <<< Web.HTML.Event.DragEvent.toEvent)
  , Ocelot.HTML.Events.onDrop (Just <<< DropFile)
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
  Array (ComponentHTML m)
renderContent =
  [ renderIcon
  , renderLabel
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
  ComponentHTML m
renderInput =
  Halogen.HTML.input
    [ Ocelot.HTMl.Properties.css "hidden"
    , Halogen.HTML.Properties.id_ _file
    , Halogen.HTML.Properties.type_ Halogen.HTML.Properties.InputFile
    ]

renderLabel ::
  forall m.
  ComponentHTML m
renderLabel =
  Halogen.HTML.label
    [ Ocelot.HTMl.Properties.css "group text-center"
    , Halogen.HTML.Properties.for _file
    ]
    [ Halogen.HTML.span
      [ Ocelot.HTMl.Properties.css "group-hover:text-blue-88" ]
      [ Halogen.HTML.text "Choose a file" ]
    , Halogen.HTML.text " or drag it here."
    ]

_file = "file" :: String
