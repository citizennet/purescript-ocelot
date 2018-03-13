module Ocelot.Block.Input where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

inputOuterClasses :: Array HH.ClassName
inputOuterClasses = HH.ClassName <$>
  [ "bg-white"
  , "font-light"
  , "rounded-sm"
  , "w-full"
  , "flex"
  ]

inputInnerClasses :: Array HH.ClassName
inputInnerClasses = HH.ClassName <$>
  [ "cc-blue-88"
  , "disabled:bg-grey-light"
  , "focus:no-outline"
  , "py-3"
  , "rounded-sm"
  , "flex-auto"
  ]

inputClasses :: Array HH.ClassName
inputClasses = inputOuterClasses <> inputInnerClasses

inputCenterClasses :: Array HH.ClassName
inputCenterClasses = HH.ClassName <$>
  [ "px-4"
  , "self-center"
  ]

inputLeftClasses :: Array HH.ClassName
inputLeftClasses = HH.ClassName <$>
  [ "pl-4"
  , "pr-1"
  , "self-center"
  ]

inputLeftBorderClasses :: Array HH.ClassName
inputLeftBorderClasses = HH.ClassName <$>
  [ "border-r"
  , "px-4"
  , "self-center"
  ]

inputRightClasses :: Array HH.ClassName
inputRightClasses = HH.ClassName <$>
  [ "pr-4"
  , "pl-1"
  , "self-center"
  ]

inputRightBorderClasses :: Array HH.ClassName
inputRightBorderClasses = HH.ClassName <$>
  [ "border-l"
  , "self-center"
  , "px-4"
  ]

addonClasses :: Array HH.ClassName
addonClasses = HH.ClassName <$>
  [ "text-grey"
  , "cursor-pointer"
  , "py-3"
  , "bg-white"
  , "rounded-sm"
  ]

input
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
input iprops =
  HH.input (iprops <> [ HP.classes $ inputClasses <> inputCenterClasses ])

percentage
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
percentage iprops =
  HH.label
  [ HP.class_ $ HH.ClassName "flex" ]
  [ HH.input (iprops <> [ HP.classes $ inputClasses <> inputLeftClasses ])
  , HH.span
    [ HP.classes $ addonClasses <> inputRightClasses ]
    [ HH.text "%" ]
  ]

currency
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
currency iprops =
  HH.label
  [ HP.class_ $ HH.ClassName "flex" ]
  [ HH.span
    [ HP.classes $ addonClasses <> inputLeftClasses ]
    [ HH.text "$" ]
  , HH.input (iprops <> [ HP.classes $ inputClasses <> inputRightClasses ])
  ]
