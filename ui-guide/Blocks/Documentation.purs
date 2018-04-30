module UIGuide.Block.Documentation where

import Prelude

import DOM.HTML.Indexed (HTMLdiv, HTMLsection, HTMLheader)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format as Format
import Ocelot.HTML.Properties ((<&>))

type DocumentationConfig =
  { header :: String
  , subheader :: String
  }

blockClasses :: Array HH.ClassName
blockClasses = HH.ClassName <$>
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

intro
  :: ∀ p i
   . DocumentationConfig
  -> Array (HH.IProp HTMLheader i)
  -> HH.HTML p i
intro config iprops =
  HH.header
    ( [ HP.classes introClasses ] <&> iprops )
    [ Format.heading
      [ HP.classes headingClasses ]
      [ HH.text config.header ]
    , Format.subHeading
      [ HP.classes subHeadingClasses ]
      [ HH.text config.subheader ]
    ]

intro_
  :: ∀ p i
   . DocumentationConfig
  -> HH.HTML p i
intro_ config = intro config []

customBlock
  :: ∀ p i
   . DocumentationConfig
  -> Array (HH.IProp HTMLsection i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
customBlock config iprops html =
  HH.section
    ( [ HP.classes blockClasses ] <&> iprops )
    [ intro_ config
    , HH.div_
      html
    ]

customBlock_
  :: ∀ p i
   . DocumentationConfig
  -> Array (HH.HTML p i)
  -> HH.HTML p i
customBlock_ config = customBlock config []

block
  :: ∀ p i
   . DocumentationConfig
  -> Array (HH.IProp HTMLsection i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
block config iprops html =
  customBlock config iprops [ callout_ html ]

block_
  :: ∀ p i
   . DocumentationConfig
  -> Array (HH.HTML p i)
  -> HH.HTML p i
block_ config = block config []
