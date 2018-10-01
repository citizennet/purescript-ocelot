module Ocelot.Block.Builder where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties (IProp, (<&>))

blockBuilder
  :: ∀ r p i
   . ( Array (IProp r i)
       -> Array (HH.HTML p i)
       -> HH.HTML p i
     )
  -> Array HH.ClassName
  -> Array (IProp r i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
blockBuilder elem classes iprops =
  elem $ [ HP.classes classes ] <&> iprops
