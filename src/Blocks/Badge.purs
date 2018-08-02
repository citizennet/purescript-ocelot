module Ocelot.Block.Badge where

import Prelude

import DOM.HTML.Indexed (HTMLspan)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

baseClasses :: Array HH.ClassName
baseClasses = HH.ClassName <$>
  [ "rounded-full"
  , "relative"
  , "before:no-content"
  , "before:w-full"
  , "before:h-full"
  , "before:absolute"
  , "before:pin-t"
  , "before:pin-l"
  , "flex"
  , "justify-center"
  , "items-center"
  , "bg-blue-88"
  , "text-white"
  ]

badgeClasses :: Array HH.ClassName
badgeClasses = baseClasses <>
  ( HH.ClassName <$>
    [ "w-8"
    , "h-8"
    ]
  )

badge
  :: ∀ p i
   . Array (IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
badge = blockBuilder HH.span badgeClasses

badge_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
badge_ = badge []

badgeSmallClasses :: Array HH.ClassName
badgeSmallClasses = baseClasses <>
  ( HH.ClassName <$>
    [ "w-6"
    , "h-6"
    , "text-sm"
    ]
  )

badgeSmall
  :: ∀ p i
   . Array (IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
badgeSmall = blockBuilder HH.span badgeSmallClasses

badgeSmall_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
badgeSmall_ = badgeSmall []

badgeLargeClasses :: Array HH.ClassName
badgeLargeClasses = baseClasses <>
  ( HH.ClassName <$>
    [ "w-12"
    , "h-12"
    ]
  )

badgeLarge
  :: ∀ p i
   . Array (IProp HTMLspan i)
  -> Array (HTML p i)
  -> HTML p i
badgeLarge = blockBuilder HH.span badgeLargeClasses

badgeLarge_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
badgeLarge_ = badgeLarge []
