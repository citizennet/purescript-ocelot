module Ocelot.Block.Card where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

baseCardClasses :: Array HH.ClassName
baseCardClasses = HH.ClassName <$>
  [ "bg-white"
  , "mb-6"
  , "rounded"
  , "clearfix"
  ]

innerCardClasses :: Array HH.ClassName
innerCardClasses = HH.ClassName <$>
  [ "m-6"
  ]

baseCard
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
baseCard iprops =
  HH.div
    ( [ HP.classes baseCardClasses ] <&> iprops )

baseCard_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
baseCard_ = baseCard []


innerCard
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
innerCard iprops =
  HH.div
    ( [ HP.classes innerCardClasses ] <&> iprops )

innerCard_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
innerCard_ = innerCard []

card
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
card iprops html =
  baseCard iprops [ innerCard_ html ]

card_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
card_ = card []
