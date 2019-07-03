module Ocelot.Block.ToggleButton where

import Prelude

import DOM.HTML.Indexed (HTMLinput, HTMLspan)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

toggleButton
  :: ∀ p i
   . Array (HH.IProp HTMLspan i)
  -> Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
toggleButton iprops inprops html =
  HH.label_
  [ HH.input inputProps
  , HH.span labelProps html
  ]
  where
  inputProps =
    [ HP.classes inputClasses
    , HP.type_ HP.InputRadio
    ] <&> inprops

  labelProps =
    [ HP.classes toggleButtonClasses ] <&> iprops

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  [ "checked:neighbor:bg-grey-50"
  , "checked:neighbor:text-white"
  , "checked:neighbor:border-grey-50"
  , "!checked:neighbor:hover:bg-grey-80"
  , "!checked:neighbor:hover:text-black-10!"
  , "offscreen"
  ]

toggleButtonClasses :: Array HH.ClassName
toggleButtonClasses = HH.ClassName <$>
  [ "no-outline"
  , "px-4"
  , "py-2"
  , "disabled:opacity-50"
  , "disabled:cursor-default"
  , "!disabled:cursor-pointer"
  , "bg-white"
  , "border-grey-80"
  , "border-2"
  , "focus:bg-grey-50-a30"
  , "text-black-20"
  , "inline-block"
  ]
