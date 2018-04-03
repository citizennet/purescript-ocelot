module UIGuide.Components.Type where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Type as Type
import Ocelot.Core.Utils ((<&>))
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type State = Unit

data Query a = NoOp a

type Input = Unit

type Message = Void

component :: ∀ m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
    where
      eval :: Query ~> H.ComponentDSL State Query Message m
      eval = case _ of
        NoOp a -> pure a

      render :: State -> H.ComponentHTML Query
      render _ = cnDocumentationBlocks


----------
-- HTML

css :: ∀ p i. String -> H.IProp ( "class" :: String | p ) i
css = HP.class_ <<< HH.ClassName

cnDocumentationBlocks :: H.ComponentHTML Query
cnDocumentationBlocks =
  HH.div_
    [ Documentation.documentation_
      { header: "Type"
      , subheader: "Various typography styles."
      }
      [ Backdrop.backdrop_
        [ Backdrop.content_
          [ HH.div
            [ css "flex-1 flex flex-col justify-between" ]
            [ Type.heading_
              [ HH.text "H1 — heading" ]
            , Type.subHeading_
              [ HH.text "H2 — subHeading" ]
            , Type.contentHeading_
              [ HH.text "H3 — contentHeading" ]
            , Type.caption_
              [ HH.text "H4 — caption" ]
            , Type.p_
              [ HH.text "p — plain pragraph" ]
            , Type.p
              [ HP.classes Type.mutedClasses ]
              [ HH.text ".muted" ]
            , Type.p
              [ HP.classes Type.linkClasses ]
              [ HH.text ".link" ]
            ]
          ]
        ]
      , Backdrop.backdropDark_
        [ Backdrop.content_
          [ HH.div
            [ css "flex-1 flex flex-col justify-between" ]
            [ Type.headingDark_
              [ HH.text "H1 — headingDark" ]
            , Type.subHeadingDark_
              [ HH.text "H2 — subHeadingDark" ]
            , Type.contentHeading_
              [ HH.text "H3 — contentHeading" ]
            , Type.caption_
              [ HH.text "H4 — caption" ]
            , Type.p_
              [ HH.text "p — plain pragraph" ]
            , Type.p
              [ HP.classes Type.mutedClasses ]
              [ HH.text ".muted" ]
            , Type.p
              [ HP.classes Type.linkDarkClasses ]
              [ HH.text ".linkDark" ]
            ]
          ]
        ]
      ]
    ]
