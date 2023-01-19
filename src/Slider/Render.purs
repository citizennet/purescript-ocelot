module Ocelot.Slider.Render where

import Prelude

import Halogen.HTML as Halogen.HTML
import Halogen.Svg.Attributes as Halogen.Svg.Attributes
import Halogen.Svg.Elements as Halogen.Svg.Elements
import Halogen.Svg.Indexed as Halogen.Svg.Indexed

-- | frameWidth: the width of the SVG in pixel
-- |
-- | SVG layout (in user space unit)
-- | * top to bottom: margin, betweenTopAndThumb, thumbRadius * 2.0,
-- |   betweenThumbAndAxis, axisHeight, margin
-- | * left to right: margin, thumbRadius, trackWidth, thumbRadius, margin
type Config =
  { axisHeight :: Number
  , betweenThumbAndAxis :: Number
  , betweenTopAndThumb :: Number
  , frameWidth :: { px :: Number }
  , margin :: Number
  , thumbRadius :: Number
  , trackRadius :: Number
  , trackWidth :: Number
  }

axisContainer ::
  forall a config p.
  { betweenThumbAndAxis :: Number
  , betweenTopAndThumb :: Number
  , thumbRadius :: Number
  | config
  } ->
  Array (Halogen.HTML.HTML p a) ->
  Halogen.HTML.HTML p a
axisContainer { betweenThumbAndAxis, betweenTopAndThumb, thumbRadius } =
  Halogen.Svg.Elements.g
    [ Halogen.Svg.Attributes.transform
        [ Halogen.Svg.Attributes.Translate
            thumbRadius
            (betweenTopAndThumb + thumbRadius * 2.0 + betweenThumbAndAxis)
        ]
    ]

axis ::
  forall a config p.
  { trackWidth :: Number
  | config
  } ->
  Array { label :: String, percent :: Number } ->
  Array (Halogen.HTML.HTML p a)
axis { trackWidth } = map mapper
  where
  mapper :: { label :: String, percent :: Number } -> Halogen.HTML.HTML p a
  mapper { label, percent } = axisLabel label (trackWidth * percent / 100.0)

axisLabel ::
  forall a p.
  String ->
  Number ->
  Halogen.HTML.HTML p a
axisLabel label x =
  Halogen.Svg.Elements.text
    [ Halogen.Svg.Attributes.text_anchor Halogen.Svg.Attributes.AnchorMiddle
    , Halogen.Svg.Attributes.transform
        [ Halogen.Svg.Attributes.Translate x 0.0 ]
    ]
    [ Halogen.HTML.text label ]

-- | NOTE assume thumbRadius > trackRadius
frame ::
  forall a config p.
  { axisHeight :: Number
  , betweenThumbAndAxis :: Number
  , betweenTopAndThumb :: Number
  , frameWidth :: { px :: Number }
  , margin :: Number
  , thumbRadius :: Number
  , trackWidth :: Number
  | config
  } ->
  Array (Halogen.HTML.IProp Halogen.Svg.Indexed.SVGsvg a) ->
  Array (Halogen.HTML.HTML p a) ->
  Halogen.HTML.HTML p a
frame { axisHeight, betweenThumbAndAxis, betweenTopAndThumb, frameWidth, margin, trackWidth, thumbRadius } iprops html =
  Halogen.Svg.Elements.svg
    ( [ Halogen.Svg.Attributes.viewBox 0.0 0.0 viewBoxWidth viewBoxHeight
      , Halogen.Svg.Attributes.width frameWidth.px
      ]
        <> iprops
    )
    [ Halogen.Svg.Elements.g
        [ Halogen.Svg.Attributes.transform
            [ Halogen.Svg.Attributes.Translate margin margin ]
        ]
        html
    ]
  where
  viewBoxHeight :: Number
  viewBoxHeight = margin + betweenTopAndThumb + thumbRadius * 2.0 + betweenThumbAndAxis + axisHeight + margin

  viewBoxWidth :: Number
  viewBoxWidth = margin + thumbRadius + trackWidth + thumbRadius + margin

interval ::
  forall a config p.
  { trackRadius :: Number
  , trackWidth :: Number
  | config
  } ->
  { end :: { percent :: Number }
  , start :: { percent :: Number }
  } ->
  Array (Halogen.HTML.IProp Halogen.Svg.Indexed.SVGrect a) ->
  Halogen.HTML.HTML p a
interval { trackRadius, trackWidth } percent iprops =
  Halogen.Svg.Elements.rect
    ( [ Halogen.Svg.Attributes.height (trackRadius * 2.0)
      , Halogen.Svg.Attributes.width (trackWidth * (percent.end.percent - percent.start.percent) / 100.0)
      , Halogen.Svg.Attributes.transform
          [ Halogen.Svg.Attributes.Translate (trackWidth * percent.start.percent / 100.0) 0.0 ]
      ]
        <> iprops
    )

markContainer ::
  forall a config p.
  { trackRadius :: Number
  | config
  } ->
  Array (Halogen.HTML.HTML p a) ->
  Halogen.HTML.HTML p a
markContainer { trackRadius } =
  Halogen.Svg.Elements.g
    [ Halogen.Svg.Attributes.transform
        [ Halogen.Svg.Attributes.Translate 0.0 trackRadius ]
    ]

mark ::
  forall a config p.
  { trackRadius :: Number
  , trackWidth :: Number
  | config
  } ->
  { percent :: Number } ->
  Array (Halogen.HTML.IProp Halogen.Svg.Indexed.SVGcircle a) ->
  Halogen.HTML.HTML p a
mark { trackRadius, trackWidth } { percent } iprops =
  Halogen.Svg.Elements.circle
    ( [ Halogen.Svg.Attributes.r trackRadius
      , Halogen.Svg.Attributes.transform
          [ Halogen.Svg.Attributes.Translate (trackWidth * percent / 100.0) 0.0 ]
      ]
        <> iprops
    )

thumbContainer ::
  forall a config p.
  { betweenTopAndThumb :: Number
  , thumbRadius :: Number
  | config
  } ->
  Array (Halogen.HTML.HTML p a) ->
  Halogen.HTML.HTML p a
thumbContainer { betweenTopAndThumb, thumbRadius } =
  Halogen.Svg.Elements.g
    [ Halogen.Svg.Attributes.transform
        [ Halogen.Svg.Attributes.Translate
            thumbRadius
            (betweenTopAndThumb + thumbRadius)
        ]
    ]

thumb ::
  forall a config p.
  { thumbRadius :: Number
  , trackWidth :: Number
  | config
  } ->
  { percent :: Number } ->
  Array (Halogen.HTML.IProp Halogen.Svg.Indexed.SVGcircle a) ->
  Halogen.HTML.HTML p a
thumb { trackWidth, thumbRadius } { percent } iprops =
  Halogen.Svg.Elements.circle
    ( [ Halogen.Svg.Attributes.r thumbRadius
      , Halogen.Svg.Attributes.fill (pure (Halogen.Svg.Attributes.RGB 255 255 255))
      , Halogen.Svg.Attributes.stroke (pure (Halogen.Svg.Attributes.RGB 0 0 0))
      , Halogen.Svg.Attributes.transform
          [ Halogen.Svg.Attributes.Translate (trackWidth * percent / 100.0) 0.0 ]
      ]
        <> iprops
    )

trackContainer ::
  forall a config p.
  { betweenTopAndThumb :: Number
  , thumbRadius :: Number
  , trackRadius :: Number
  | config
  } ->
  Array (Halogen.HTML.HTML p a) ->
  Halogen.HTML.HTML p a
trackContainer { betweenTopAndThumb, thumbRadius, trackRadius } =
  Halogen.Svg.Elements.g
    [ Halogen.Svg.Attributes.transform
        [ Halogen.Svg.Attributes.Translate thumbRadius (betweenTopAndThumb + thumbRadius - trackRadius) ]
    ]

track ::
  forall a config p.
  { trackRadius :: Number
  , trackWidth :: Number
  | config
  } ->
  Array (Halogen.HTML.IProp Halogen.Svg.Indexed.SVGrect a) ->
  Halogen.HTML.HTML p a
track config = interval config { start: { percent: 0.0 }, end: { percent: 100.0 } }

pixelToPercent ::
  forall config.
  { frameWidth :: { px :: Number }
  , margin :: Number
  , thumbRadius :: Number
  , trackWidth :: Number
  | config
  } ->
  { px :: Number } ->
  { percent :: Number }
pixelToPercent { frameWidth, margin, thumbRadius, trackWidth } x = { percent }
  where
  frameWidthInUnit :: Number
  frameWidthInUnit = margin + thumbRadius + trackWidth + thumbRadius + margin

  scale :: Number
  scale = frameWidthInUnit / frameWidth.px

  percent :: Number
  percent = x.px * scale / trackWidth * 100.0

