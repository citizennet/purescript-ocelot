module CN.UI.Block.Range (range) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

range
  :: ∀ p i
   . String
  -> Number
  -> Number
  -> String
  -> String
  -> Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
range inputId min max minLabel maxLabel iprops =
  HH.div
    [ HP.class_ $ HH.ClassName "flex items-center w-full" ]
    [ HH.span
      [ HP.class_ $ HH.ClassName "flex-no-grow text-sm mr-4" ]
      [ HH.text minLabel ]
    , HH.input
      ( iprops <>
        [ HP.type_ HP.InputRange
        , HP.min min
        , HP.max max
        , HP.id_ inputId
        , HP.class_ $ HH.ClassName "bg-transparent flex-1"
        ]
      )
    , HH.span
      [ HP.class_ $ HH.ClassName "flex-no-grow text-sm ml-4" ]
      [ HH.text maxLabel ]
    ]

