module Ocelot.Block.Card where

import Prelude

import DOM.HTML.Indexed (HTMLh3, HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Core.Utils ((<&>))

data ExpansionStatus
  = Collapsed
  | Expanded

cardClasses :: Array HH.ClassName
cardClasses = HH.ClassName <$>
  [ "bg-white"
  , "h-auto"
  , "p-6"
  , "mb-10"
  , "rounded"
  ]

card
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
card iprops html =
  HH.div
    ( [ HP.classes cardClasses ] <&> iprops )
    html

card_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
card_ = card []
