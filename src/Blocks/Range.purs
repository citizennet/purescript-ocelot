module Ocelot.Block.Range (range) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


range
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
range iprops =
  HH.input $ iprops <> [ HP.type_ HP.InputRange ]
