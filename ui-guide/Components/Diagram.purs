module UIGuide.Component.Diagram where

import Prelude

import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.Svg.Attributes as Halogen.Svg.Attributes
import Halogen.Svg.Elements as Halogen.Svg.Elements
import Ocelot.Block.Card as Card
import Ocelot.Block.Diagram as Ocelot.Block.Diagram
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import UIGuide.Block.Backdrop as Backdrop
import UIGuide.Block.Documentation as Documentation

type Component m = Halogen.Component Halogen.HTML.HTML Query Input Output m

type ComponentHTML m = Halogen.ComponentHTML Action ChildSlots m

type ComponentM m a = Halogen.HalogenM State Action ChildSlots Output m a

type State = Unit

data Query a

type Action = Unit

type Input = Unit

type Output = Void

type ChildSlots =
  ()

----------
-- HTML

component :: forall m. Component m
component =
  Halogen.mkComponent
    { initialState: const unit
    , render
    , eval: Halogen.mkEval Halogen.defaultEval
    }

render :: forall m. State -> ComponentHTML m
render _ =
  Halogen.HTML.div_
  [ Documentation.block_
    { header: "Percentage Bar Graph"
    , subheader: "Visualize a number in percentage"
    }
    [ Backdrop.backdrop_
      [ Backdrop.content_
        [ Card.card_
          [ Halogen.HTML.p
            [ Ocelot.HTML.Properties.css "flex items-center" ]
            [ Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "min-w-5"]
              [ Halogen.HTML.text "A:" ]
            , Ocelot.Block.Diagram.percentBar percentA
              [ Ocelot.HTML.Properties.css "h-4 pl-2 w-1/3" ]
              [ Ocelot.HTML.Properties.css "bg-red"]
            , Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "ml-4 text-red" ]
              [ Halogen.HTML.text (show percentA <> "%")]
            ]
          , Halogen.HTML.p
            [ Ocelot.HTML.Properties.css "flex items-center" ]
            [ Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "min-w-5"]
              [ Halogen.HTML.text "B:" ]
            , Ocelot.Block.Diagram.percentBar percentB
              [ Ocelot.HTML.Properties.css "h-4 pl-2 w-1/3" ]
              [ Ocelot.HTML.Properties.css "bg-blue-82"]
            , Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "ml-4 text-blue-82" ]
              [ Halogen.HTML.text (show percentB <> "%")]
            ]
          ]
        , Card.card_
          [ Halogen.HTML.p
            [ Ocelot.HTML.Properties.css "flex items-center" ]
            [ Halogen.Svg.Elements.svg
              [ Halogen.Svg.Attributes.width 400.0
              , Halogen.Svg.Attributes.viewBox 0.0 0.0 viewBoxWidth viewBoxHeight
              ]
              [ Halogen.Svg.Elements.g
                [ Halogen.Svg.Attributes.transform
                  [ Halogen.Svg.Attributes.Translate margin margin ]
                ]
                [ Halogen.Svg.Elements.g
                  [ Halogen.Svg.Attributes.transform
                    [ Halogen.Svg.Attributes.Translate thumbRadius (betweenTopAndThumb + thumbRadius - trackRadius) ]
                  ]
                  [ Halogen.Svg.Elements.rect
                    [ Halogen.Svg.Attributes.fill (pure (Halogen.Svg.Attributes.RGB 229 229 229))
                    , Halogen.Svg.Attributes.height trackHeight
                    , Halogen.Svg.Attributes.width trackWidth
                    ]
                  , Halogen.Svg.Elements.rect
                    [ Halogen.Svg.Attributes.fill (pure (Halogen.Svg.Attributes.RGB 126 135 148))
                    , Halogen.Svg.Attributes.height trackHeight
                    , Halogen.Svg.Attributes.width (trackWidth * (percentB - percentA) / 100.0)
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate (trackWidth * percentA / 100.0) 0.0 ]
                    ]
                  ]
                , Halogen.Svg.Elements.g
                  [ Halogen.Svg.Attributes.transform
                    [ Halogen.Svg.Attributes.Translate thumbRadius (betweenTopAndThumb + thumbRadius) ]
                  ]
                  [ Halogen.Svg.Elements.circle
                    [ Halogen.Svg.Attributes.r 12.5
                    , Halogen.Svg.Attributes.fill (pure (Halogen.Svg.Attributes.RGB 255 255 255))
                    , Halogen.Svg.Attributes.stroke (pure (Halogen.Svg.Attributes.RGB 0 0 0))
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate (trackWidth * percentA / 100.0) 0.0 ]
                    ]
                  , Halogen.Svg.Elements.circle
                    [ Halogen.Svg.Attributes.r 12.5
                    , Halogen.Svg.Attributes.fill (pure (Halogen.Svg.Attributes.RGB 255 255 255))
                    , Halogen.Svg.Attributes.stroke (pure (Halogen.Svg.Attributes.RGB 0 0 0))
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate (trackWidth * percentB / 100.0) 0.0 ]
                    ]
                  ]
                , Halogen.Svg.Elements.g
                  [ Halogen.Svg.Attributes.transform
                    [ Halogen.Svg.Attributes.Translate thumbRadius (betweenTopAndThumb + thumbRadius * 2.0 + betweenThumbAndAxis) ]
                  ]
                  [ Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 0.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "0%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 40.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "1%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 80.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "2%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 120.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "3%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 160.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "4%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 200.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "5%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 240.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "6%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 280.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "7%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 320.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "8%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 360.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "9%" ]
                  , Halogen.Svg.Elements.text
                    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
                    , Halogen.Svg.Attributes.transform
                      [ Halogen.Svg.Attributes.Translate 400.0 0.0 ]
                    ]
                    [ Halogen.HTML.text "10%" ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
  where
  percentA :: Number
  percentA = 30.0

  percentB :: Number
  percentB = 100.0

  -- | NOTE assume thumbRadius > trackRadius
  viewBoxHeight :: Number
  viewBoxHeight = margin + betweenTopAndThumb + thumbRadius * 2.0 + betweenThumbAndAxis + axisHeight + margin

  viewBoxWidth :: Number
  viewBoxWidth = margin + thumbRadius + trackWidth + thumbRadius + margin

  margin :: Number
  margin = 5.0

  betweenTopAndThumb :: Number
  betweenTopAndThumb = 20.0

  betweenThumbAndAxis :: Number
  betweenThumbAndAxis = 20.0

  axisHeight :: Number
  axisHeight = 20.0

  thumbRadius :: Number
  thumbRadius = 10.0

  trackWidth :: Number
  trackWidth = 400.0

  trackHeight :: Number
  trackHeight = trackRadius * 2.0

  trackRadius :: Number
  trackRadius = 2.5
