module Ocelot.Block.Popover where

import Prelude

import Ocelot.Block.Hover as Hover
import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

popover
  :: forall p i
   . Array (HH.IProp HTMLdiv i)
  -> Array Hover.HoverAnchor
  -> HH.HTML p i
  -> Array (HH.HTML p i)
  -> HH.HTML p i
popover props anchors trigger hoverHtml =
  Hover.hover
    props
    anchors
    trigger
    ( HH.div
      [ HP.classes hoverClasses ]
      hoverHtml
    )

popover_
  :: forall p i
   . Array Hover.HoverAnchor
  -> HH.HTML p i
  -> Array (HH.HTML p i)
  -> HH.HTML p i
popover_ =
  popover []

hoverClasses :: Array HH.ClassName
hoverClasses = HH.ClassName <$>
  [ "p-4"
  , "my-1"
  , "bg-white"
  , "border"
  , "border-grey-80"
  , "rounded"
  , "shadow-md"
  , "items-center"
  ]
