module Ocelot.Block.Toggle (toggle) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(InputCheckbox))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type ToggleProps =
  { label :: String }

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "flex"
  , "flex-row"
  , "items-center"
  , "inline-block"
  , "py-1"
  , "cursor-pointer"
  , "leading-loose"
  , "text-black"
  ]

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  [ "checked:sibling:bg-blue-88"
  , "checked:sibling:pl-4"
  , "!checked:sibling:bg-grey-light"
  , "!checked:sibling:pr-4"
  , "offscreen"
  ]

toggleClasses :: Array HH.ClassName
toggleClasses = HH.ClassName <$>
  [ "transition-1/4"
  , "inline-flex"
  , "justify-center"
  , "items-center"
  , "content-box"
  , "h-4"
  , "w-4"
  , "p-1"
  , "rounded-full"
  , "mr-3"
  , "before:bg-white"
  , "before:h-full"
  , "before:w-full"
  , "before:rounded-full"
  , "before:no-content"
  , "before:shadow"
  ]

toggle
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
toggle iprops =
  HH.label
    [ HP.classes labelClasses ]
    [ HH.input iprops'
    , HH.span [ HP.classes toggleClasses ] []
    ]
    where
      iprops' = iprops <>
        [ HP.classes inputClasses
        , HP.type_ InputCheckbox
        ]
