module Ocelot.Block.Toast where

import Prelude

import Data.Array (foldr, snoc)
import Data.Bifunctor (lmap, rmap)
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

toastClasses :: Array HH.ClassName
toastClasses = HH.ClassName <$>
  [ "fixed"
  , "pin-b"
  , "h-20"
  , "shadow-inner"
  , "transition-1/4-out"
  , "p-5"
  , "flex"
  , "items-center"
  ]

toast
  :: ∀ p i
   . Array (HH.IProp p i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
toast iprops html = HH.div
  ( [ HP.classes toastClasses ] <&> iprops )
  html
