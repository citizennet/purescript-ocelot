module CN.UI.Block.Input (input, inputClasses) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  [ "bg-white"
  , "cc-blue-88"
  , "disabled:bg-grey-light"
  , "focus:no-outline"
  , "font-light"
  , "px-4"
  , "py-2"
  , "rounded-sm"
  , "text-sm"
  , "w-full"
  ]

input
  :: ∀ p i
  . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
input iprops =
  HH.input (iprops <> [ HP.classes inputClasses ])
