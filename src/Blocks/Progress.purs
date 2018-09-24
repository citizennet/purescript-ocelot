module Ocelot.Block.Progress where

import Prelude

import DOM.HTML.Indexed (HTMLspan, HTMLdiv)
import Data.Ratio (Ratio, denominator, numerator)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties (css, (<&>))

-- Passed props reach the <span>.
bar :: ∀ p i. Ratio Number -> Array (HH.IProp HTMLdiv i) -> Array (HH.IProp HTMLspan i) -> HH.HTML p i
bar r divProps spanProps =
  HH.div
    ( [ css "bg-grey-light relative" ] <&> divProps )
    [ HH.span
      ( [ css "block", HP.attr (HH.AttrName "style") ("width: " <> value <> "%; text-indent: -9999px;") ] <&> spanProps )
      [ HH.text $ "Progress: " <> value ]
    ]
  where
    value :: String
    value = show $ if percentage > 100.0 then 100.0 else percentage

    percentage :: Number
    percentage = numerator r / denominator r * 100.0

-- Defaults
bar_ :: ∀ p i. Ratio Number -> HH.HTML p i
bar_ r = bar r [] [ css "bg-blue" ]

