module Ocelot.Block.Card where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

cardClasses :: Array HH.ClassName
cardClasses = HH.ClassName <$>
  [ "bg-white"
  , "h-auto"
  , "w-full"
  , "p-6"
  , "shadow"
  , "w-auto"
  , "mb-10"
  ]

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "mb-4"
  , "font-medium"
  , "text-black-20"
  , "text-lg"
  , "flex"
  , "items-center"
  ]

card
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
card html =
  HH.aside
    [ HP.classes cardClasses ]
    html

header
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
header html =
  HH.header_
    [ HH.h3
      [ HP.classes headerClasses ]
      html
    ]
