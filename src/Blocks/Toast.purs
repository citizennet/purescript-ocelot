module Ocelot.Block.Toast where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Array (snoc)
import Data.Bifunctor (lmap, rmap)
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))
import Unsafe.Coerce (unsafeCoerce)

type Toast r = ( visible :: Boolean | r )

type HTMLtoast = Toast HTMLdiv

visible :: ∀ r i. Boolean -> HP.IProp ( visible :: Boolean | r ) i
visible = HP.prop (HH.PropName "visible")

-- Necessary for centering the toast
toastContainerClasses :: Array HH.ClassName
toastContainerClasses = HH.ClassName <$>
  [ "flex"
  , "transition-1/4-in"
  , "transition-1/2-out"
  , "items-center"
  , "fixed"
  , "pin-l"
  , "pin-r"
  , "pin-b"
  ]

containerVisibleClasses :: Array HH.ClassName
containerVisibleClasses = HH.ClassName <$>
  [ "mb-8" ]

containerClosedClasses :: Array HH.ClassName
containerClosedClasses = HH.ClassName <$>
  [ "-mb-40" ]

toastClasses :: Array HH.ClassName
toastClasses = HH.ClassName <$>
  [ "shadow-md"
  , "p-4"
  , "ml-auto"
  , "mr-auto"
  , "items-center"
  , "border"
  , "border-grey-80"
  , "bg-white"
  , "rounded"
  , "flex"
  ]

toast
  :: ∀ p i
   . Array (HH.IProp HTMLtoast i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
toast iprops html =
  HH.div
  [ HP.classes $ toastContainerClasses <> containerClasses' ]
  [ HH.div
    ( [ HP.classes toastClasses ] <&> iprops' )
    html
  ]
  where
    Tuple visible iprops' = pullVisibleProp iprops
    containerClasses' =
      if visible
        then containerVisibleClasses
        else containerClosedClasses


pullVisibleProp
  :: ∀ r i
   . Array (HH.IProp ( visible :: Boolean | r ) i)
  -> Tuple Boolean (Array (HH.IProp r i))
pullVisibleProp = foldr f (Tuple false [])
  where
    f (HP.IProp (HC.Property "visible" x)) =
      lmap $ const $ coerceExpanded x
    f iprop = rmap $ (flip snoc) $ coerceR iprop

    coerceExpanded :: HC.PropValue -> Boolean
    coerceExpanded = unsafeCoerce

    coerceR :: HH.IProp ( visible :: Boolean | r ) i -> HH.IProp r i
    coerceR = unsafeCoerce
