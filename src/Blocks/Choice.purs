module Ocelot.Blocks.Choice where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

choiceClasses :: Array HH.ClassName
choiceClasses = HH.ClassName <$>
  [ "absolute"
  , "bg-white"
  , "rounded-lg"
  , "border"
  , "border-grey-90"
  , "shadow"
  , "overflow-hidden"
  ]

choice
  :: ∀ p i
   .  Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
choice iprops =
  HH.div
    ( [ HP.classes choiceClasses ] <&> iprops )

choice_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
choice_ =
  choice []

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "flex"
  , "h-10"
  , "justify-center"
  , "items-center"
  , "border-b"
  , "border-grey-90"
  ]

header
  :: ∀ p i
   .  Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
header iprops =
  HH.div
    ( [ HP.classes headerClasses ] <&> iprops )

header_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
header_ =
  header []

-- Blocks for a horizontally oriented Choice element

hBodyClasses :: Array HH.ClassName
hBodyClasses = HH.ClassName <$>
  [ "flex"
  , "mt-px"
  ]

hBody
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
hBody iprops =
  HH.div
    ( [ HP.classes hBodyClasses ] <&> iprops )

hBody_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
hBody_ =
  hBody []

hOptionClasses :: Array HH.ClassName
hOptionClasses = HH.ClassName <$>
  [ "bg-white"
  , "flex"
  , "flex-col"
  , "items-center"
  , "h-30"
  , "justify-center"
  , "shadow"
  , "w-40"
  , "cursor-pointer"
  ]

hOption
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
hOption iprops =
  HH.div
    ( [ HP.classes hOptionClasses ] <&> iprops )

hOption_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
hOption_ =
  hOption []

-- Blocks for a vertically oriented Choice element

vBodyClasses :: Array HH.ClassName
vBodyClasses = hBodyClasses <> (HH.ClassName <$> [ "flex-col", "w-90" ])

vBody
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
vBody iprops =
  HH.div
    ( [ HP.classes vBodyClasses ] <&> iprops )

vBody_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
vBody_ =
  vBody []

vOptionClasses :: Array HH.ClassName
vOptionClasses = HH.ClassName <$>
  [ "bg-white"
  , "flex"
  , "items-center"
  , "justify-center"
  , "cursor-pointer"
  ]

vOption
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
vOption iprops =
  HH.div
    ( [ HP.classes vOptionClasses ] <&> iprops )

vOption_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
vOption_ =
  vOption []

highlightedOptionClasses :: Array HH.ClassName
highlightedOptionClasses = HH.ClassName <$>
  [ "bg-grey-97" ]

