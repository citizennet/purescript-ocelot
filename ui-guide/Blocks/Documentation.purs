module UIGuide.Block.Documentation where

import Prelude

import DOM.HTML.Indexed (HTMLsection, HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Type as Type
import Ocelot.Core.Utils ((<&>))

type DocumentationProps =
  { header :: String
  , subheader :: String
  }

documentationClasses :: Array HH.ClassName
documentationClasses = HH.ClassName <$>
  [ "my-20"
  ]

introClasses :: Array HH.ClassName
introClasses = HH.ClassName <$>
  [ "my-12"
  ]

headingClasses :: Array HH.ClassName
headingClasses = HH.ClassName <$>
  [ "w-1/2"
  ]

subHeadingClasses :: Array HH.ClassName
subHeadingClasses = HH.ClassName <$>
  [ "w-1/2"
  , "font-light"
  , "text-grey-50"
  ]

calloutClasses :: Array HH.ClassName
calloutClasses = HH.ClassName <$>
  [ "border-dotted"
  , "border"
  , "rounded"
  , "overflow-hidden"
  , "flex"
  , "items-stretch"
  , "my-6"
  ]

callout
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
callout iprops html =
  HH.div
    ( [ HP.classes calloutClasses ] <&> iprops )
    html

callout_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
callout_ = callout []

documentation'
  :: ∀ p i
   . DocumentationProps
  -> Array (HH.IProp HTMLsection i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
documentation' config iprops html =
  HH.section
    ( [ HP.classes documentationClasses ] <&> iprops )
    [ HH.header
      [ HP.classes introClasses ]
      [ Type.heading
          [ HP.classes headingClasses ]
          [ HH.text config.header ]
      , Type.subHeading
        [ HP.classes subHeadingClasses ]
        [ HH.text config.subheader ]
      ]
    , HH.div_
      html
    ]

documentation'_
  :: ∀ p i
   . DocumentationProps
  -> Array (HH.HTML p i)
  -> HH.HTML p i
documentation'_ config = documentation' config []

documentation
  :: ∀ p i
   . DocumentationProps
  -> Array (HH.IProp HTMLsection i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
documentation config iprops html =
  documentation' config iprops [ callout_ html ]

documentation_
  :: ∀ p i
   . DocumentationProps
  -> Array (HH.HTML p i)
  -> HH.HTML p i
documentation_ config = documentation config []
