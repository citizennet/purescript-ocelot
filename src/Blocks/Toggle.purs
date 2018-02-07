module CN.UI.Block.Toggle (toggle) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(InputCheckbox))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

labelClasses :: Array HH.ClassName
labelClasses = HH.ClassName <$>
  [ "cursor-pointer"
  , "select-none"
  ]

inputClasses :: Array HH.ClassName
inputClasses = HH.ClassName <$>
  [ "checked:sibling:bg-teal"
  , "checked:sibling:after:transform-w-4-0"
  , "offscreen"
  ]

spanClasses :: Array HH.ClassName
spanClasses = HH.ClassName <$>
  [ "after:absolute"
  , "after:bg-grey-darkest"
  , "after:block"
  , "after:h-4"
  , "after:no-content"
  , "after:pin-l"
  , "after:rounded-lg"
  , "after:w-4"
  , "before:absolute"
  , "before:no-content"
  , "before:"
  , "bg-grey-light"
  , "items-center"
  , "flex"
  , "h-3"
  , "relative"
  , "rounded-lg"
  , "shadow-inner"
  , "w-8"
  ]

toggle
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
toggle iprops =
  HH.label
    [ HP.classes labelClasses ]
    [ HH.input iprops' 
    , HH.span 
      [ HP.classes spanClasses ] 
      []
    ]
    where
      iprops' = 
        iprops <> [ HP.classes inputClasses , HP.type_ InputCheckbox , HP.checked false ]

