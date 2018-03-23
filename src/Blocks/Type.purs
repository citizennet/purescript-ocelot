module Ocelot.Block.Type where

import Prelude

import DOM.HTML.Indexed (HTMLh1, HTMLh2, HTMLh3, HTMLh4)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Core.Utils ((<&>))

headingClasses :: Array HH.ClassName
headingClasses = HH.ClassName <$>
  [ "mb-6"
  , "text-3xl"
  , "font-normal"
  , "leading-loose"
  , "flex"
  , "items-center"
  ]

headingDarkClasses :: Array HH.ClassName
headingDarkClasses = headingClasses <>
  ( HH.ClassName <$>
    [ "text-white"
    ]
  )

subHeadingClasses :: Array HH.ClassName
subHeadingClasses = HH.ClassName <$>
  [ "mb-6"
  , "text-xl"
  , "font-light"
  , "leading-loose"
  , "flex"
  , "items-center"
  ]

subHeadingDarkClasses :: Array HH.ClassName
subHeadingDarkClasses = subHeadingClasses <>
  ( HH.ClassName <$>
    [ "text-white"
    ]
  )

contentHeadingClasses :: Array HH.ClassName
contentHeadingClasses = HH.ClassName <$>
  [ "mb-6"
  , "text-lg"
  , "font-light"
  , "leading-loose"
  , "flex"
  , "items-center"
  ]

captionClasses :: Array HH.ClassName
captionClasses = HH.ClassName <$>
  [ "block"
  , "font-light"
  , "mb-6"
  , "text-grey-70"
  , "text-sm"
  , "tracking-wide"
  , "uppercase"
  ]

linkClasses :: Array HH.ClassName
linkClasses = HH.ClassName <$>
  [ "text-blue-88"
  , "hover:text-blue-70"
  , "font-medium"
  , "cursor-pointer"
  ]

linkDarkClasses :: Array HH.ClassName
linkDarkClasses = HH.ClassName <$>
  [ "text-grey-lightest"
  , "hover:text-white"
  , "font-medium"
  , "cursor-pointer"
  ]

mutedClasses :: Array HH.ClassName
mutedClasses = HH.ClassName <$>
  [ "text-grey-dark"
  ]

heading
  :: ∀ p i
   . Array (HH.IProp HTMLh1 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
heading iprops html =
  HH.h1
    ( [ HP.classes headingClasses ] <&> iprops )
    html

heading_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
heading_ = heading []

headingDark
  :: ∀ p i
   . Array (HH.IProp HTMLh1 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
headingDark iprops html =
  HH.h1
    ( [ HP.classes headingDarkClasses ] <&> iprops )
    html

headingDark_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
headingDark_ = headingDark []

subHeading
  :: ∀ p i
   . Array (HH.IProp HTMLh2 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
subHeading iprops html =
  HH.h2
    ( [ HP.classes subHeadingClasses ] <&> iprops )
    html

subHeading_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
subHeading_ = subHeading []

subHeadingDark
  :: ∀ p i
   . Array (HH.IProp HTMLh2 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
subHeadingDark iprops html =
  HH.h2
    ( [ HP.classes subHeadingDarkClasses ] <&> iprops )
    html

subHeadingDark_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
subHeadingDark_ = subHeadingDark []

contentHeading
  :: ∀ p i
   . Array (HH.IProp HTMLh3 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
contentHeading iprops html =
  HH.h3
    ( [ HP.classes contentHeadingClasses ] <&> iprops )
    html

contentHeading_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
contentHeading_ = contentHeading []

caption
  :: ∀ p i
   . Array (HH.IProp HTMLh4 i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
caption iprops html =
  HH.h4
    ( [ HP.classes captionClasses ] <&> iprops )
    html

caption_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
caption_ = caption []
