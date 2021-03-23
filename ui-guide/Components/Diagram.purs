module UIGuide.Component.Diagram where

import Prelude

import Data.Array as Data.Array
import Data.Int as Data.Int
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.Svg.Attributes as Halogen.Svg.Attributes
import Ocelot.Block.Card as Card
import Ocelot.Block.Diagram as Ocelot.Block.Diagram
import Ocelot.Slider.Render as Ocelot.Slider.Render
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
            [ Ocelot.Slider.Render.frame config
                [ Halogen.Svg.Attributes.width 400.0 ]
                [ Ocelot.Slider.Render.trackContainer config
                  [ Ocelot.Slider.Render.track config
                    [ Halogen.Svg.Attributes.fill (pure (Halogen.Svg.Attributes.RGB 229 229 229))
                    ]
                  , Ocelot.Slider.Render.markContainer config
                    [ Ocelot.Slider.Render.mark config { percent: 0.0 } []
                    , Ocelot.Slider.Render.mark config { percent: 10.0 } []
                    , Ocelot.Slider.Render.mark config { percent: 20.0 } []

                    ]
                  , Ocelot.Slider.Render.interval config
                      { start: percentA , end: percentB }
                      [ Halogen.Svg.Attributes.fill (pure (Halogen.Svg.Attributes.RGB 126 135 148)) ]
                  ]
                , Ocelot.Slider.Render.thumbContainer config
                  [ Ocelot.Slider.Render.thumb config { percent: percentA } []
                  , Ocelot.Slider.Render.thumb config { percent: percentB } []
                  ]
                , Ocelot.Slider.Render.axisContainer config
                    (Ocelot.Slider.Render.axis config axisData)
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

  trackHeight :: Number
  trackHeight = trackRadius * 2.0

  -- | NOTE assume thumbRadius > trackRadius
  config :: Ocelot.Slider.Render.Config
  config = { axisHeight, betweenThumbAndAxis, betweenTopAndThumb, margin, trackWidth, trackRadius, thumbRadius }

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

  trackRadius :: Number
  trackRadius = 2.5

axisData :: Array { label :: String, percent :: Number }
axisData = toLabel <$> Data.Array.range 0 10
  where
  toLabel :: Int -> { label :: String, percent :: Number }
  toLabel index =
    { label: show index <> "%"
    , percent: (Data.Int.toNumber index) * 10.0
    }
