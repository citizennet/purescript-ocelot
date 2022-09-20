module UIGuide.Component.Button
  ( Message
  , Query
  , Slot
  , component
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format as Format
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.Button as Button
import Ocelot.Clipboard as Ocelot.Clipboard
import Ocelot.HTML.Properties (css)
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type Slot
  = H.Slot Query Message

type Component m
  = H.Component Query Input Message m

type ComponentHTML m
  = H.ComponentHTML Action ChildSlot m

type ComponentM m
  = H.HalogenM State Action ChildSlot Message m

type State = Unit

data Query (a :: Type)

type Action = Unit

type Input = Unit

type Message = Void

type ChildSlot =
  ( clipboard :: Ocelot.Clipboard.Slot Unit
  )

_clipboard = Proxy :: Proxy "clipboard"

----------
-- HTML

component ::
  forall m.
  MonadAff m =>
  Component m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML m
render _ =
  HH.div_
  [ renderButtons
  , renderButtonGroups
  , renderClipboardButtons
  , renderPills
  ]

renderButtons :: forall m. ComponentHTML m
renderButtons =
  Documentation.block_
  { header: "Buttons"
  , subheader: "Perform actions."
  }
  [ Backdrop.backdrop_
    [ Backdrop.content_
      [ HH.div
        [ css "mb-6 flex" ]
        [ HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Standard" ]
          , Button.button_
            [ HH.text "Cancel" ]
          ]
        , HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Disabled" ]
          , Button.button
            [ HP.disabled true ]
            [ HH.text "Cancel" ]
          ]
        ]
      , HH.div
        [ css "mb-6 flex" ]
        [ HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Primary" ]
          , Button.buttonPrimary_
            [ HH.text "Submit" ]
          ]
        , HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Disabled" ]
          , Button.buttonPrimary
            [ HP.disabled true ]
            [ HH.text "Submit" ]
          ]
        ]
      ]
    ]
  , Backdrop.backdropWhite_
    [ Backdrop.content_
      [ HH.div
        [ css "mb-6 flex" ]
        [ HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Standard" ]
          , Button.button_
            [ HH.text "Cancel" ]
          ]
        , HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Disabled" ]
          , Button.button
            [ HP.disabled true ]
            [ HH.text "Cancel" ]
          ]
        ]
      , HH.div
        [ css "mb-6 flex" ]
        [ HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Primary" ]
          , Button.buttonPrimary_
            [ HH.text "Submit" ]
          ]
        , HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Disabled" ]
          , Button.buttonPrimary
            [ HP.disabled true ]
            [ HH.text "Submit" ]
          ]
        ]
      ]
    ]
  , Backdrop.backdropDark_
    [ Backdrop.content_
      [ HH.div
        [ css "mb-6 flex" ]
        [ HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Standard" ]
          , Button.buttonDark_
            [ HH.text "Cancel" ]
          ]
        , HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Disabled" ]
          , Button.buttonDark
            [ HP.disabled true ]
            [ HH.text "Cancel" ]
          ]
        ]
      , HH.div
        [ css "mb-6 flex" ]
        [ HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Primary" ]
          , Button.buttonPrimary_
            [ HH.text "Submit" ]
          ]
        , HH.div
          [ css "flex-1" ]
          [ Format.caption_
            [ HH.text "Disabled" ]
          , Button.buttonPrimary
            [ HP.disabled true ]
            [ HH.text "Submit" ]
          ]
        ]
      ]
    ]
  ]

renderButtonGroups :: forall m. ComponentHTML m
renderButtonGroups =
  Documentation.customBlock_
    { header: "Button Groups"
    , subheader: "Group related actions."
    }
    [ Documentation.callout_
      [ Backdrop.backdrop_
        [ Backdrop.content_
          [ HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Two Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonLeft_
                [ HH.text "Back" ]
              , Button.buttonRight_
                [ HH.text "Forward" ]
              ]
            ]
          , HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Two Disabled Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonLeft
                [ HP.disabled true ]
                [ HH.text "Back" ]
              , Button.buttonRight
                [ HP.disabled true ]
                [ HH.text "Forward" ]
              ]
            ]
          ]
        ]
      , Backdrop.backdropWhite_
        [ Backdrop.content_
          [ HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Two Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonLeft_
                [ HH.text "Back" ]
              , Button.buttonRight_
                [ HH.text "Forward" ]
              ]
            ]
          , HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Two Disabled Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonLeft
                [ HP.disabled true ]
                [ HH.text "Back" ]
              , Button.buttonRight
                [ HP.disabled true ]
                [ HH.text "Forward" ]
              ]
            ]
          ]
        ]
      , Backdrop.backdropDark_
        [ Backdrop.content_
          [ HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Two Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonDarkLeft_
                [ HH.text "Back" ]
              , Button.buttonDarkRight_
                [ HH.text "Forward" ]
              ]
            ]
          , HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Two Disabled Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonDarkLeft
                [ HP.disabled true ]
                [ HH.text "Back" ]
              , Button.buttonDarkRight
                [ HP.disabled true ]
                [ HH.text "Forward" ]
              ]
            ]
          ]
        ]
      ]
    , Documentation.callout_
      [ Backdrop.backdrop_
        [ Backdrop.content_
          [ HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Three Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonPrimaryLeft_
                [ HH.text "Rewind" ]
              , Button.buttonPrimaryCenter_
                [ HH.text "Play" ]
              , Button.buttonPrimaryRight_
                [ HH.text "Fast-Forward" ]
              ]
            ]
          , HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Three Disabled Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonPrimaryLeft
                [ HP.disabled true ]
                [ HH.text "Rewind" ]
              , Button.buttonPrimaryCenter
                [ HP.disabled true ]
                [ HH.text "Play" ]
              , Button.buttonPrimaryRight
                [ HP.disabled true ]
                [ HH.text "Fast-Forward" ]
              ]
            ]
          ]
        ]
      , Backdrop.backdropWhite_
        [ Backdrop.content_
          [ HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Three Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonPrimaryLeft_
                [ HH.text "Rewind" ]
              , Button.buttonPrimaryCenter_
                [ HH.text "Play" ]
              , Button.buttonPrimaryRight_
                [ HH.text "Fast-Forward" ]
              ]
            ]
          , HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Three Disabled Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonPrimaryLeft
                [ HP.disabled true ]
                [ HH.text "Rewind" ]
              , Button.buttonPrimaryCenter
                [ HP.disabled true ]
                [ HH.text "Play" ]
              , Button.buttonPrimaryRight
                [ HP.disabled true ]
                [ HH.text "Fast-Forward" ]
              ]
            ]
          ]
        ]
      , Backdrop.backdropDark_
        [ Backdrop.content_
          [ HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Three Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonPrimaryLeft_
                [ HH.text "Rewind" ]
              , Button.buttonPrimaryCenter_
                [ HH.text "Play" ]
              , Button.buttonPrimaryRight_
                [ HH.text "Fast-Forward" ]
              ]
            ]
          , HH.div
            [ css "mb-6" ]
            [ Format.caption_
              [ HH.text "Three Disabled Buttons" ]
            , Button.buttonGroup_
              [ Button.buttonPrimaryLeft
                [ HP.disabled true ]
                [ HH.text "Rewind" ]
              , Button.buttonPrimaryCenter
                [ HP.disabled true ]
                [ HH.text "Play" ]
              , Button.buttonPrimaryRight
                [ HP.disabled true ]
                [ HH.text "Fast-Forward" ]
              ]
            ]
          ]
        ]
      ]
    ]

renderClipboardButtons ::
  forall m.
  MonadAff m =>
  ComponentHTML m
renderClipboardButtons =
  Documentation.block_
    { header: "Clipboard Buttons"
    , subheader: "copy to clipboard"
    }
    [ Backdrop.backdrop_
      [ Backdrop.content_
        [ HH.div
          [ css "mb-6" ]
          [ Format.caption_
            [ HH.text "Standard" ]
          , HH.slot_ _clipboard unit
              Ocelot.Clipboard.component
              { text: "copied from UIGuide" }
          ]
        ]
      ]
    ]

renderPills :: forall m. ComponentHTML m
renderPills =
  Documentation.customBlock_
    { header: "Pills"
    , subheader: "These might be used for displaying selected filters."
    }
    [ Documentation.callout_
      [ Backdrop.backdrop_
        [ Backdrop.content_
          [ HH.div
            [ css "mb-6 flex" ]
            [ HH.div 
              [ css "flex-1" ]
              [ Format.caption_
                [ HH.text "Standard Button" ]
              , Button.pillButton
                { decorator: Button.Left Ocelot.Block.Icon.plus_ }
                [ HH.text "Add Filter" ]
              ]
            , HH.div 
              [ css "flex-1" ]
              [ Format.caption_
                [ HH.text "Disabled Button" ]
              , Button.pillButton
                { decorator: Button.Left Ocelot.Block.Icon.plus_ 
                , props: [ HP.disabled true ]
                }
                [ HH.text "Add Filter" ]
              ]
            ]
          , HH.div
            [ css "mb-6 flex" ]
            [ HH.div 
              [ css "flex-1" ]
              [ Format.caption_
                [ HH.text "Primary Anchor" ]
              , Button.pillAnchor
                { decorator: Button.Right Ocelot.Block.Icon.delete_
                , style: Button.Primary
                }
                [ HH.text "Color: Primary" ]
              ]
            , HH.div 
              [ css "flex-1" ]
              []
            ]
          ]
        ]
      , Backdrop.backdropWhite_
        [ Backdrop.content_
          [ HH.div
            [ css "mb-6 flex" ]
            [ HH.div 
              [ css "flex-1" ]
              [ Format.caption_
                [ HH.text "Standard Button" ]
              , Button.pillButton
                { decorator: Button.Left Ocelot.Block.Icon.plus_ }
                [ HH.text "Add Filter" ]
              ]
            , HH.div 
              [ css "flex-1" ]
              [ Format.caption_
                [ HH.text "Disabled Button" ]
              , Button.pillButton
                { decorator: Button.Left Ocelot.Block.Icon.plus_ 
                , props: [ HP.disabled true ]
                }
                [ HH.text "Add Filter" ]
              ]
            ]
          , HH.div
            [ css "mb-6 flex" ]
            [ HH.div 
              [ css "flex-1" ]
              [ Format.caption_
                [ HH.text "Primary Anchor" ]
              , Button.pillAnchor
                { decorator: Button.Right Ocelot.Block.Icon.delete_
                , style: Button.Primary
                }
                [ HH.text "Color: Primary" ]
              ]
            , HH.div 
              [ css "flex-1" ]
              []
            ]
          ]
        ]
      ]
    ]
