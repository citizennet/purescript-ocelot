module Ocelot.Block.Radio where

import Prelude

import DOM.HTML.Indexed (HTMLinput, HTMLlabel)
import DOM.HTML.Indexed.InputType (InputType(InputRadio))
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
  ]

radioClasses :: Array HH.ClassName
radioClasses = HH.ClassName <$>
  [ "inline-flex"
  , "justify-center"
  , "items-center"
  , "content-box"
  , "border-2"
  , "border-solid"
  , "h-4"
  , "w-4"
  , "p-1"
  , "flex-none"
  , "no-content"
  , "rounded-full"
  , "mr-3"
  , "before:transition-1/4-bounce"
  , "before:h-full"
  , "before:w-full"
  , "before:bg-blue-88"
  , "before:no-content"
  , "before:rounded-full"
  , "before:shadow"
  ]

radio
  :: ∀ p i
   . Array (HH.IProp HTMLlabel i)
  -> Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
radio iprops inprops html =
  HH.label
    ( [ HP.classes labelClasses ] <&> iprops )
    ( [ HH.input
        ( [ HP.classes inputClasses
          , HP.type_ InputRadio
          ] <&> inprops
        )
      , HH.span [ HP.classes radioClasses ] []
      ]
      <> html
    )

radio_
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
radio_ = radio []
