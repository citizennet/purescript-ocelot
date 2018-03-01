module CN.UI.Block.Input where

import Prelude

import DOM.HTML.Indexed (HTMLinput, HTMLlabel)
import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

inputOuterClasses :: Array HH.ClassName
inputOuterClasses = HH.ClassName <$>
  [ "bg-white"
  , "font-light"
  -- , "border-b"
  -- , "border-grey-lighter"
  , "rounded-sm"
  , "w-full"
  , "flex"
  ]

inputInnerClasses :: Array HH.ClassName
inputInnerClasses = HH.ClassName <$>
  [ "cc-blue-88"
  , "disabled:bg-grey-light"
  , "focus:no-outline"
  , "px-4"
  , "py-3"
  , "rounded-sm"
  , "flex-auto"
  ]

inputClasses :: Array HH.ClassName
inputClasses = inputOuterClasses <> inputInnerClasses

inputLeftClasses :: Array HH.ClassName
inputLeftClasses = HH.ClassName <$>
  [ "border-r"
  , "ml-r"
  , "px-3"
  , "cursor-pointer"
  , "self-center"
  ]

inputRightClasses :: Array HH.ClassName
inputRightClasses = HH.ClassName <$>
  [ "border-l"
  , "ml-1"
  , "px-3"
  , "cursor-pointer"
  , "self-center"
  ]

input
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
input iprops =
  HH.input (iprops <> [ HP.classes inputClasses ])

-- inputGroup
  -- :: ∀ p i
   -- . Maybe (Array (HH.HTML p i))
  -- -> Maybe (Array (HH.HTML p i))
  -- -> Array (HH.IProp HTMLinput i)
  -- -> HH.HTML p i
-- inputGroup l r iprops =
  -- HH.label
    -- [ HP.classes textFieldClasses ]
    -- (  (maybe [] (pure <<< (HH.span [ HP.classes inputLeftClasses ])) l)
    -- <> [ HH.input (iprops <> [ HP.classes inputClasses ]) ]
    -- <> (maybe [] (pure <<< (HH.span [ HP.classes inputRightClasses ])) r)
    -- )
