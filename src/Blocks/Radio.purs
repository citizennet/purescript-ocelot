module Ocelot.Block.Radio (radio) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(InputRadio))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type RadioProps =
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
  [ "checked:sibling:after:opacity-100"
  , "checked:sibling:after:scale-1"
  , "checked:sibling:border-blue-88"
  , "!checked:sibling:after:opacity-0"
  , "!checked:sibling:after:scale-0"
  , "!checked:sibling:border-grey-70-a40"
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
  , "h-3"
  , "w-3"
  , "p-1"
  , "no-content"
  , "rounded-full"
  , "mr-3"
  , "after:all-02s-ease"
  , "after:h-full"
  , "after:w-full"
  , "after:bg-blue-88"
  , "after:no-content"
  , "after:rounded-full"
  , "after:shadow"
  ]

radio
  :: ∀ p i
   . RadioProps
  -> Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
radio props iprops =
  HH.label
    [ HP.classes labelClasses ]
    [ HH.input iprops'
    , HH.span [ HP.classes radioClasses ] []
    , HH.text props.label
    ]
    where
     iprops' = iprops <>
       [ HP.classes inputClasses
       , HP.type_ InputRadio
       ]
