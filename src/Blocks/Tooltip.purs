module Ocelot.Block.Tooltip where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties (css, (<&>))

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

tooltip_
  :: ∀ p i
   . String
  -> HH.HTML p i
  -> HH.HTML p i
tooltip_ = flip tooltip []

tooltipClasses :: Array HH.ClassName
tooltipClasses = HH.ClassName <$>
  [ "absolute"
  , "invisible"
  , "group-hover:visible"
  , "text-white"
  , "bg-grey-50"
  , "px-2"
  , "rounded"
  , "z-60"
  , "-my-8"
  ]
