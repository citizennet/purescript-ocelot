module Ocelot.Blocks.Table where

import Prelude

import DOM.HTML.Indexed (HTMLtbody, HTMLth, HTMLthead, HTMLtd)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

theadClasses :: Array HH.ClassName
theadClasses = HH.ClassName <$>
  [ "bg-grey-90"
  , "h-16"
  , "shadow"
  , "w-full"
  ]

thead
  :: ∀ p i
   . Array (HH.IProp HTMLthead i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
thead iprops =
  HH.thead
    ( [ HP.classes theadClasses ] <&> iprops )

thead_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
thead_ =
  thead []

thClasses :: Array HH.ClassName
thClasses = HH.ClassName <$>
  [ "font-medium"
  , "text-black-20"
  ]

th
  :: ∀ p i
   . Array (HH.IProp HTMLth i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
th iprops =
  HH.th
    ( [ HP.classes thClasses ] <&> iprops )

th_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
th_ =
  th []

tbodyClasses :: Array HH.ClassName
tbodyClasses = HH.ClassName <$>
  [ "shadow" ]

tbody
  :: ∀ p i
   . Array (HH.IProp HTMLtbody i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
tbody iprops =
  HH.tbody
    ( [ HP.classes tbodyClasses ] <&> iprops )

tbody_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
tbody_ =
  tbody []

tdClasses :: Array HH.ClassName
tdClasses = HH.ClassName <$>
  [ "bg-white"
  , "h-20"
  , "my-pixel"
  , "shadow" 
  ]

td
  :: ∀ p i
   . Array (HH.IProp HTMLtd i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
td iprops =
  HH.td
    ( [ HP.classes tdClasses ] <&> iprops )

td_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
td_ =
  td []
