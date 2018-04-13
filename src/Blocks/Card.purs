module Ocelot.Block.Card where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Core.Utils ((<&>))

data ExpansionStatus
  = Collapsed
  | Expanded

baseCardClasses :: Array HH.ClassName
baseCardClasses = HH.ClassName <$>
  [ "bg-white"
  , "mb-6"
  , "rounded"
  , "clearfix"
  ]

cardClasses :: Array HH.ClassName
cardClasses = baseCardClasses <>
  ( HH.ClassName <$>
    [ "px-6"
    , "pt-6"
    ]
  )

baseCard
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
baseCard iprops html =
  HH.div
    ( [ HP.classes baseCardClasses ] <&> iprops )
    html

baseCard_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
baseCard_ = baseCard []

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
