module Ocelot.Block.Loading where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Either (either)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))
import Svg.Parser.Halogen (parse)

svgString :: String
svgString =
  "<svg class=\"circular\" viewBox=\"25 25 50 50\">" <>
  "<circle class=\"path\" cx=\"50\" cy=\"50\" r=\"20\" fill=\"none\" stroke-width=\"4\" stroke-miterlimit=\"10\"/>" <>
  "</svg>"

svgElem :: ∀ p i. HH.HTML p i
svgElem = either (const $ HH.text "") identity $ parse svgString

spinner :: ∀ p i. Array (HH.IProp HTMLdiv i) -> HH.HTML p i
spinner props =
  HH.div
    ( [ HP.class_ $ HH.ClassName "loader" ] <&> props )
    [ svgElem ]

spinner_ :: ∀ p i. HH.HTML p i
spinner_ = spinner []
