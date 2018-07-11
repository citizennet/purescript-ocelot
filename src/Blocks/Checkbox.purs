module Ocelot.Block.Checkbox where

import Prelude

import DOM.HTML.Indexed (HTMLinput, HTMLlabel)
import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "flex"
  , "flex-row"
  , "inline-block"
  , "py-2"
  , "cursor-pointer"
  , "text-black-20"
  , "items-center"
  , "text-left" -- styles get messed up otherwise
  ]

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  -- start shared custom classes defined for radios --
  [ "!disabled:sibling:bg-white"
  , "disabled:sibling:bg-grey-95"
  , "checked:sibling:before:opacity-100"
  , "checked:sibling:before:scale-1"
  , "checked:!disabled:sibling:border-blue-88"
  , "focus:sibling:border-blue-88"
  , "!checked:sibling:before:opacity-0"
  , "!checked:sibling:before:scale-0"
  , "!focus:hover:!checked:!disabled:sibling:border-grey-70"
  , "focus:sibling:shadow"
  , "checked:!disabled:sibling:before:bg-blue-88"
  , "checked:disabled:sibling:before:bg-grey-80"
  , "checked:disabled:sibling:border-grey-80"
  , "offscreen"
  -- end shared custom radio classes --
  , "checked:sibling:after:opacity-100"
  , "checked:sibling:after:scale-1"
  , "!checked:sibling:after:opacity-0"
  , "!checked:sibling:after:scale-0"
  ]

checkboxClasses :: Array HH.ClassName
checkboxClasses = HH.ClassName <$>
  [ "relative"
  , "content-box"
  , "border-2"
  , "border-solid"
  , "h-5"
  , "w-5"
  , "flex-none"
  , "no-content"
  , "mr-3"
  , "rounded"
  , "before:transition-1/4-bounce"
  , "before:absolute"
  , "before:h-full"
  , "before:w-full"
  , "before:no-content"
  , "after:transition-1/4-bounce"
  , "after:absolute"
  , "after:w-full"
  , "after:h-2"
  , "after:border-l-2"
  , "after:border-b-2"
  , "after:border-white"
  , "after:no-content"
  , "after:rotate-315"
  , "after:shadow"
  ]

checkbox
  :: ∀ p i
   . Array (HH.IProp HTMLlabel i)
  -> Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
checkbox iprops inprops html =
  HH.label
    ( [ HP.classes labelClasses ] <&> iprops )
    ( [ HH.input
        ( [ HP.classes inputClasses
          , HP.type_ InputCheckbox
          ] <&> inprops
        )
      , HH.span [ HP.classes checkboxClasses ] []
      ]
      <> html
    )

checkbox_
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
checkbox_ = checkbox []
