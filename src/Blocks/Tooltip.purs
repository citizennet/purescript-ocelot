module Ocelot.Block.Tooltip where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties (css, (<&>))

-- | A tooltip which allows styling on the outer container
tooltip
  :: ∀ p i
   . String
  -> Array (HH.IProp HTMLdiv i)
  -> HH.HTML p i
  -> HH.HTML p i
tooltip msg props html =
  HH.div
    ([ css "group cursor-pointer inline-block" ] <&> props)
    [ HH.div
      [ HP.classes tooltipClasses ]
      [ HH.text msg ]
    , html
    ]

-- | A tooltip with no extra styling
tooltip_
  :: ∀ p i
   . String
  -> HH.HTML p i
  -> HH.HTML p i
tooltip_ = flip tooltip []

-- | A tooltip which gives access to inner and outer styling
tooltip' ::
  ∀ p i.
  String ->
  Array (HH.IProp HTMLdiv i) ->
  Array (HH.IProp HTMLdiv i) ->
  HH.HTML p i ->
  HH.HTML p i
tooltip' msg outerProps innerProps html =
  HH.div
    ([ css "group cursor-pointer inline-block" ] <&> outerProps)
    [ html
    , HH.div
        ([ css "relative" ])
        [ HH.div
            ([ HP.classes tooltipClasses' ] <&> innerProps)
            [ HH.text msg ]
        ]
    ]

tooltipClasses :: Array HH.ClassName
tooltipClasses = HH.ClassName <$> classesArr <> [ "-my-8" ]

tooltipClasses' :: Array HH.ClassName
tooltipClasses' = HH.ClassName <$> classesArr

classesArr :: Array String
classesArr =
  [ "absolute"
  , "invisible"
  , "group-hover:visible"
  , "text-white"
  , "bg-grey-50"
  , "px-2"
  , "rounded"
  , "z-60"
  ]
