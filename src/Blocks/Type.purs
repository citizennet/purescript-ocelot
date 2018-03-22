module Ocelot.Block.Type where

import Prelude

import DOM.HTML.Indexed (HTMLh3)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Core.Utils ((<&>))

contentHeaderClasses :: Array HH.ClassName
contentHeaderClasses = HH.ClassName <$>
  [ "mb-4"
  , "font-medium"
  , "text-black-20"
  , "text-lg"
  , "flex"
  , "items-center"
  ]


captionClasses :: Array HH.ClassName
captionClasses = HH.ClassName <$>
  [ "block"
  , "font-light"
  , "my-6"
  , "text-grey-70"
  , "text-sm"
  , "tracking-wide"
  , "uppercase"
  ]

linkClasses :: Array HH.ClassName
linkClasses = HH.ClassName <$>
  [ "text-blue-88"
  , "font-medium"
  , "cursor-pointer"
  ]

mutedClasses :: Array HH.ClassName
mutedClasses = HH.ClassName <$>
  [ "text-grey-dark"
  ]

contentHeader
  :: ∀ p i
   . Array (HH.IProp HTMLh3 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
contentHeader iprops html =
  HH.h3
    ( [ HP.classes contentHeaderClasses ] <&> iprops )
    html

contentHeader_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
contentHeader_ = contentHeader []
