module CN.UI.Block.Input (input) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

cnInput :: HH.ClassName
cnInput = HH.ClassName "cn-input"

input
  :: ∀ p i
  . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
input iprops =
  HH.input (iprops <> [ HP.class_ cnInput ])
