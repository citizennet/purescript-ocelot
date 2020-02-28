module UIGuide.Component.Button where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a
type Action = Unit

type Input = Unit

type Message = Void


----------
-- HTML

component :: ∀ m. H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
    render :: State -> H.ComponentHTML Action () m
    render _ =
      HH.div_
      [ Documentation.block_
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
      , Documentation.customBlock_
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
      ]
