module Ocelot.Block.Range (range) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type RangeConfig =
  { min :: String
  , max :: String
  }

range
  :: ∀ p i
   . RangeConfig
  -> Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
range { min, max } iprops =
  HH.div
    [ HP.class_ $ HH.ClassName "flex items-center w-full" ]
    [ HH.span
      [ HP.class_ $ HH.ClassName "flex-no-grow mr-4" ]
      [ HH.text min ]
    , HH.input
      ( iprops <>
        [ HP.type_ HP.InputRange
        , HP.class_ $ HH.ClassName "bg-transparent flex-1"
        ]
      )
    , HH.span
      [ HP.class_ $ HH.ClassName "flex-no-grow ml-4" ]
      [ HH.text max ]
    ]

