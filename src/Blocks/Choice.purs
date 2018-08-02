module Ocelot.Block.Choice where

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

bodyClasses :: Array HH.ClassName
bodyClasses = HH.ClassName <$> [ "flex" ]

body
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
body iprops =
  HH.div
    ( [ HP.classes bodyClasses ] <&> iprops )

body_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
body_ =
  body []

optionClasses :: Array HH.ClassName
optionClasses = HH.ClassName <$>
  [ "bg-white"
  , "flex"
  , "flex-col"
  , "items-center"
  , "h-30"
  , "justify-center"
  , "w-40"
  , "cursor-pointer"
  ]

option
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
option iprops =
  HH.div
    ( [ HP.classes optionClasses ] <&> iprops )

option_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
option_ =
  option []

highlightedOptionClasses :: Array HH.ClassName
highlightedOptionClasses = HH.ClassName <$>
  [ "bg-grey-97" ]

