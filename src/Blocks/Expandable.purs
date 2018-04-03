module Ocelot.Block.Expandable where

import Prelude

import DOM.HTML.Indexed (HTMLheader, HTMLspan, HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
import Ocelot.Block.Type as Type
import Ocelot.Core.Utils ((<&>))

data Status
  = Collapsed
  | Expanded

instance heytingAlgebraStatus :: HeytingAlgebra Status where
  ff = Collapsed
  tt = Expanded
  implies a b = not a || b
  conj Expanded Expanded = Expanded
  conj _ _ = Collapsed
  disj Expanded _ = Expanded
  disj _ Expanded = Expanded
  disj _ _ = Collapsed
  not Expanded = Collapsed
  not Collapsed = Expanded

fromBoolean :: Boolean -> Status
fromBoolean false = Collapsed
fromBoolean true = Expanded

toBoolean :: Status -> Boolean
toBoolean Collapsed = false
toBoolean Expanded = true

headingClasses :: Array HH.ClassName
headingClasses = HH.ClassName <$>
  [ "flex"
  , "justify-between"
  , "cursor-pointer"
  ]

headingInnerClasses :: Array HH.ClassName
headingInnerClasses = HH.ClassName <$>
  [ "flex-initial"
  ]

chevronClasses :: Array HH.ClassName
chevronClasses = HH.ClassName <$>
  [ "text-grey-70"
  , "text-lg"
  , "leading-loose"
  ]

contentSharedClasses :: Array HH.ClassName
contentSharedClasses = HH.ClassName <$>
  []

contentClasses :: Status -> Array HH.ClassName
contentClasses status = contentSharedClasses <>
  ( case status of
    Collapsed -> HH.ClassName <$>
      [ "max-h-0"
      , "opacity-0"
      , "overflow-hidden"
      , "transition-1/4-in"
      ]
    Expanded -> HH.ClassName <$>
      [ "max-h-full"
      , "opacity-1"
      , "transition-1/2-out"
      ]
  )

heading
  :: ∀ p i
   . Status
  -> Array (HH.IProp HTMLheader i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
heading status iprops html =
  HH.header
    ( [ HP.classes headingClasses ] <&> iprops )
    [ HH.div
      [ HP.classes headingInnerClasses ]
      html
    , HH.div_
      [ chevron_ status ]
    ]

heading_
  :: ∀ p i
   . Status
  -> Array (HH.HTML p i)
  -> HH.HTML p i
heading_ status = heading status []

chevron
  :: ∀ p i
   . Status
  -> Array (HH.IProp HTMLspan i)
  -> HH.HTML p i
chevron status iprops =
  ( case status of
    Collapsed -> Icon.expand
    Expanded  -> Icon.collapse
  )
  ( [ HP.classes chevronClasses ] <&> iprops )

chevron_
  :: ∀ p i
   . Status
  -> HH.HTML p i
chevron_ status = chevron status []

content
  :: ∀ p i
   . Status
  -> Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
content status iprops =
  HH.div
    ( [ HP.classes $ contentClasses status ] <&> iprops )

content_
  :: ∀ p i
   . Status
  -> Array (HH.HTML p i)
  -> HH.HTML p i
content_ status = content status []
