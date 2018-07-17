module Ocelot.Block.Toast where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

-- Necessary for centering the toast
toastContainerClasses :: Array HH.ClassName
toastContainerClasses = HH.ClassName <$>
  [ "flex"
  , "items-center"
  , "fixed"
  , "pin-b"
  , "pin-l"
  , "pin-r"
  ]

toastClasses :: Array HH.ClassName
toastClasses = HH.ClassName <$>
  [ "transition-1/4-in"
  , "transition-1/2-out"
  , "shadow-md"
  , "p-4"
  , "ml-auto"
  , "mr-auto"
  , "mb-8"
  , "flex"
  , "items-center"
  , "border"
  , "border-grey-80"
  , "bg-white"
  , "rounded"
  ]

toast
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
toast iprops html =
  HH.div
  [ HP.classes toastContainerClasses ]
  [ HH.div
    ( [ HP.classes toastClasses ] <&> iprops )
    html
  ]
