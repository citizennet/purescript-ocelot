module UIGuide.Component.FilePicker where

import Prelude

import DOM.HTML.Indexed.InputAcceptType as DOM.HTML.Indexed.InputAcceptType
import Data.Maybe (Maybe(..))
import Data.MediaType.Common as Data.MediaType.Common
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Effect.Class.Console
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Card as Card
import Ocelot.Block.Format as Format
import Ocelot.Data.InputAcceptType as Ocelot.Data.InputAcceptType
import Ocelot.FilePicker as Ocelot.FilePicker
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation
import Web.File.File as Web.File.File

type Component m = Halogen.Component Query Input Output m

type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m

type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State
  = {}

data Action
  = HandleFilePicker Ocelot.FilePicker.Output

data Query a

type Input
  = Unit

type Output
  = Void

type ChildSlots =
  ( filePicker :: Ocelot.FilePicker.Slot String
  )

_filePicker = Proxy :: Proxy "filePicker"

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
initialState _ = {}

handleAction ::
  forall m.
  MonadAff m =>
  Action ->
  ComponentM m Unit
handleAction = case _ of
  HandleFilePicker output -> case output of
    Ocelot.FilePicker.Selected files -> do
      Halogen.liftEffect <<< Effect.Class.Console.log $
        "file selected: " <> show (map Web.File.File.name $ files)

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML m
render state =
  Halogen.HTML.div_
  [ Documentation.customBlock_
    { header: "File Picker"
    , subheader: "Select File(s) from File System"
    }
    [ Backdrop.backdrop_
      [ Backdrop.content_
        [ Card.card
          [ Ocelot.HTML.Properties.css "flex-1" ]
          [ Halogen.HTML.h3
            [ Halogen.HTML.Properties.classes Format.captionClasses ]
            [ Halogen.HTML.text "Single" ]
          , Halogen.HTML.slot _filePicker "Single"
            ( Ocelot.FilePicker.component
                { accept: Nothing
                , id: "file-single"
                , multiple: false
                }
            )
            unit
            (Just <<< HandleFilePicker)
          ]
        , Card.card
          [ Ocelot.HTML.Properties.css "flex-1" ]
          [ Halogen.HTML.h3
            [ Halogen.HTML.Properties.classes Format.captionClasses ]
            [ Halogen.HTML.text "Multiple" ]
          , Halogen.HTML.slot _filePicker "Multiple"
            ( Ocelot.FilePicker.component
                { accept: Nothing
                , id: "file-multiple"
                , multiple: true
                }
            )
            unit
            (Just <<< HandleFilePicker)
          ]
        , Card.card
          [ Ocelot.HTML.Properties.css "flex-1" ]
          [ Halogen.HTML.h3
            [ Halogen.HTML.Properties.classes Format.captionClasses ]
            [ Halogen.HTML.text "Single - CSV" ]
          , Halogen.HTML.slot _filePicker "Single - CSV"
            ( Ocelot.FilePicker.component
              { accept:
                  Just
                    $ DOM.HTML.Indexed.InputAcceptType.mediaType
                        Data.MediaType.Common.textCSV
              , id: "file-single-csv"
              , multiple: true
              }
            )
            unit
            (Just <<< HandleFilePicker)
          ]
        , Card.card
          [ Ocelot.HTML.Properties.css "flex-1" ]
          [ Halogen.HTML.h3
            [ Halogen.HTML.Properties.classes Format.captionClasses ]
            [ Halogen.HTML.text "Multiple - Image" ]
          , Halogen.HTML.slot _filePicker "Multiple - Image"
            ( Ocelot.FilePicker.component
              { accept: Just Ocelot.Data.InputAcceptType.allImages
              , id: "file-multiple-image"
              , multiple: true
              }
            )
            unit
            (Just <<< HandleFilePicker)
          ]
        ]
      ]
    ]
  ]

