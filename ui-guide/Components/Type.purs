module UIGuide.Component.Type where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties (css)
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a
type Action = Unit

type Input = Unit

type Message = Void

component :: ∀ m. H.Component HH.HTML Query Input Message m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }
    where

      render :: State -> H.ComponentHTML Action () m
      render _ = cnDocumentationBlocks


----------
-- HTML

cnDocumentationBlocks :: forall p i. HH.HTML p i
cnDocumentationBlocks =
  HH.div_
    [ Documentation.block_
      { header: "Type"
      , subheader: "Various typography styles."
      }
      [ Backdrop.backdrop_
        [ Backdrop.content_
          [ HH.div
            [ css "flex-1 flex flex-col justify-between" ]
            [ Format.heading_
              [ HH.text "H1 — heading" ]
            , Format.subHeading_
              [ HH.text "H2 — subHeading" ]
            , Format.contentHeading_
              [ HH.text "H3 — contentHeading" ]
            , Format.caption_
              [ HH.text "H4 — caption" ]
            , Format.p_
              [ HH.text "p — plain pragraph" ]
            , Format.p
              [ HP.classes Format.mutedClasses ]
              [ HH.text ".muted" ]
            , Format.p
              [ HP.classes Format.linkClasses ]
              [ HH.text ".link" ]
            ]
          ]
        ]
      , Backdrop.backdropDark_
        [ Backdrop.content_
          [ HH.div
            [ css "flex-1 flex flex-col justify-between" ]
            [ Format.headingDark_
              [ HH.text "H1 — headingDark" ]
            , Format.subHeadingDark_
              [ HH.text "H2 — subHeadingDark" ]
            , Format.contentHeading_
              [ HH.text "H3 — contentHeading" ]
            , Format.caption_
              [ HH.text "H4 — caption" ]
            , Format.p_
              [ HH.text "p — plain pragraph" ]
            , Format.p
              [ HP.classes Format.mutedClasses ]
              [ HH.text ".muted" ]
            , Format.p
              [ HP.classes Format.linkDarkClasses ]
              [ HH.text ".linkDark" ]
            ]
          ]
        ]
      ]
    ]
