module Ocelot.Block.Card (card) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type CardProps =
  { title :: String }

cardClasses :: Array HH.ClassName
cardClasses = HH.ClassName <$>
  [ "bg-white"
  , "h-auto"
  , "w-full"
  , "p-6"
  , "shadow"
  , "w-auto"
  ]

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "mb-4" ]

titleClasses :: Array HH.ClassName
titleClasses = HH.ClassName <$>
  [ "font-medium"
  , "text-black-20"
  , "text-lg"
  ]

card
  :: ∀ p i
  . CardProps
  -> Array (HH.HTML p i)
  -> HH.HTML p i
card props html =
  HH.aside
    [ HP.classes cardClasses ]
    [ HH.header
        [ HP.classes headerClasses ]
        [ HH.h3
            [ HP.classes titleClasses ]
            [ HH.text props.title ]
        ]
    , HH.div_
        html
    ]
