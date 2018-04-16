module Ocelot.Block.Modal where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Core.Utils ((<&>))

modalBackgroundClasses :: Array HH.ClassName
modalBackgroundClasses = HH.ClassName <$>
  [ "fixed"              -- position absolute
  , "pin"                -- pins to all corners to fill screen
  , "z-50"               -- ensure container is top-most
  , "overflow-auto"      -- allow scrolling if modal > viewport size
  , "bg-black-modal-a90" -- transparency background
  , "flex"
  ]

modalClasses :: Array HH.ClassName
modalClasses = HH.ClassName <$>
  [ "relative"      -- position in normal flow of page
  , "bg-grey-95"
  , "rounded"
  , "mt-20"         -- moderate top margin
  , "w-full"        -- allow full width
  , "max-w-2xl"     -- but never above breakpoint
  , "m-auto"        -- centered
  , "flex-col"
  , "flex"
  ]

background
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
background iprops html =
  HH.div
    ( [ HP.classes modalBackgroundClasses ] <&> iprops )
    html

background_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
background_ = background []

modal
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
modal iprops html =
  HH.div
    ( [ HP.classes modalClasses ] <&> iprops )
    html

modal_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
modal_ = modal []
