module CN.UI.Block.Radio (radio) where

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
  , "flex-col"
  , "py-1"
  ]

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  [ "checked:sibling:after:opacity-100"
  , "checked:sibling:after:scale-1"
  , "checked:sibling:before:border-teal"
  , "hidden"
  , "not:checked:sibling:after:opacity-0"
  , "not:checked:sibling:after:scale-0"
  ]

spanClasses :: Array HH.ClassName
spanClasses = HH.ClassName <$>
  [ "after:absolute"
  , "after:bg-teal"
  , "after:all-02s-ease"
  , "after:h-3"
  , "after:left-w-6/2--w-4/2"
  , "after:no-content"
  , "after:rounded-full"
  , "after:top-w-6/2--w-4/2"
  , "after:w-3"
  , "before:absolute"
  , "before:border"
  , "before:border-color-grey"
  , "before:border-solid"
  , "before:h-6"
  , "before:no-content"
  , "before:pin-l"
  , "before:pin-t"
  , "before:rounded-full"
  , "before:w-6"
  , "cursor-pointer"
  , "inline-block"
  , "leading-normal"
  , "pl-8"
  , "relative"
  , "text-black"
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
    , HH.span
      [ HP.classes spanClasses ]
      [ HH.text props.label ]
    ]
    where 
     iprops' = 
       iprops <> [ HP.type_ InputRadio, HP.classes inputClasses ]
