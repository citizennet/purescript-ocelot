module Ocelot.Component.Typeahead.Render where

import Prelude

import Data.Fuzzy (Fuzzy(..))
import Data.Maybe (isJust)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Conditional (conditional_)
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Input as Input
import Ocelot.Block.Loading as Loading
import Ocelot.Component.Typeahead as TA
import Ocelot.HTML.Properties (css)
import Select as Select

----------
-- Overall Rendering

renderSingle
  :: ∀ pq item m
   . TA.State Maybe item m
  -> Select.State (Fuzzy item)
  -> Select.ComponentHTML (TA.Query pq Maybe item) (Fuzzy item)
renderSingle pst cst =
  HH.label_
    [ Input.inputGroup
      [ css $ maybe "offscreen" (const "") pst.selected ]
      ( conditional (isJust pst.selected)
        [ HP.classes if isDisabled then disabledClasses else Input.mainLeftClasses ]


----------
-- Container Rendering


----------
-- Item Rendering


----------
-- Shared Rendering

error :: ∀ p i. HH.HTML p i
error =
  HH.div
    [ css "flex items-center mt-1" ]
    [ Icon.error
      [ css "text-2xl text-yellow" ]
    , HH.p
      [ css "ml-3 text-grey-50 font-light" ]
      [ HH.text "Some data could not be retrieved here." ]
    ]

spinner :: ∀ p i. HH.HTML p i
spinner = Loading.spinner [ css "w-6 text-blue-88" ]
