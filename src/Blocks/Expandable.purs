module Ocelot.Block.Expandable where

import Prelude

import DOM.HTML.Indexed (HTMLheader, HTMLspan, HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
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
  , "items-center"
  , "justify-between"
  ]

heaadingInnerClasses :: Array HH.ClassName
heaadingInnerClasses = HH.ClassName <$>
  [ "flex-initial"
  ]

contentClasses :: Status -> Array HH.ClassName
contentClasses Collapsed = HH.ClassName <$>
  [ "max-h-0"
  , "opacity-0"
  , "overflow-hidden"
  -- transition when collapsing
  -- the browser doesn't calculate the animation based on the
  -- content height, but some really high number, so we use
  -- "ease-in" so it starts out fast while it's collapsing all
  -- the unused "max" height, then slows down as it nears our
  -- actual height. otherwise there will appear to be a long lag
  -- before our content suddenly collapses
  , "transition-1/4-in"
  ]
contentClasses Expanded = HH.ClassName <$>
  [ "max-h-full"
  , "opacity-100"
  , "overflow-hidden"
  -- transition while expanding
  -- the inverse behavior described in the comment for when
  -- it's collapsing
  , "transition-1-out"
  ]

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
      [ HP.classes heaadingInnerClasses ]
      html
    , chevron_ status
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
chevron Collapsed = Icon.expand
chevron Expanded = Icon.collapse

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
