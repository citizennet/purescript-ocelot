module Ocelot.Block.Progress where

import Prelude

import DOM.HTML.Indexed (HTMLspan, HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties (css, (<&>))

-- Always assumes value is out of 100; will normalize to 100 if a value is passed > 100
-- Passed props reach the <span>.
bar :: ∀ p i. Int -> Array (HH.IProp HTMLdiv i) -> Array (HH.IProp HTMLspan i) -> HH.HTML p i
bar i divProps spanProps =
  HH.div
    ( [ css "bg-grey-light relative" ] <&> divProps )
    [ HH.span
      ( [ css "block", HP.attr (HH.AttrName "style") ("width: " <> value <> "%; text-indent: -9999px;") ] <&> spanProps )
      [ HH.text $ "Progress: " <> value ]
    ]
  where
    value :: String
    value = show $ if i > 100 then 100 else i

-- Defaults
bar_ :: ∀ p i. Int -> HH.HTML p i
bar_ i = bar i [] [ css "bg-blue" ]

