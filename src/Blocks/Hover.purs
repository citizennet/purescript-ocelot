module Ocelot.Block.Hover where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties (css, (<&>))

data HoverAnchor = Left | Right | Top | Bottom

hover
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array HoverAnchor
  -> HH.HTML p i
  -> HH.HTML p i
  -> HH.HTML p i
hover props anchors html hoverHtml =
  HH.div
    ([ css $ "inline-block group cursor-pointer relative " ] <&> props)
    [ HH.div
      [ HP.classes $ hoverClasses <> anchorClasses anchors ]
      [ hoverHtml ]
    , html
    ]
  where
    anchorClasses :: Array HoverAnchor -> Array HH.ClassName
    anchorClasses = map $ HH.ClassName <<< anchorClass
    anchorClass Left = "pin-r-full"
    anchorClass Right = "pin-l-full"
    anchorClass Top = "pin-b-full"
    anchorClass Bottom = "pin-t-full"

hover_
  :: ∀ p i
   . Array HoverAnchor
  -> HH.HTML p i
  -> HH.HTML p i
  -> HH.HTML p i
hover_ = hover []

hoverClasses :: Array HH.ClassName
hoverClasses = HH.ClassName <$>
  [ "absolute"
  , "invisible"
  , "group-hover:visible"
  , "z-60"
  ]
