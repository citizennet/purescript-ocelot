module UIGuide.Block.Documentation
  ( documentation
  , documentation_
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLsection)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Core.Utils ((<&>))

type DocumentationProps =
  { header :: String
  , subheader :: String
  }

documentationClasses :: Array HH.ClassName
documentationClasses = HH.ClassName <$>
  [ "my-16"
  ]

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "font-normal"
  , "leading-loose"
  , "text-black-20"
  , "text-4xl"
  , "my-4"
  ]

subheaderClasses :: Array HH.ClassName
subheaderClasses = HH.ClassName <$>
  [ "font-light"
  , "leading-normal"
  , "text-grey-50"
  , "text-xl"
  , "mb-12"
  ]

documentation
  :: ∀ p i
   . DocumentationProps
  -> Array (HH.IProp HTMLsection i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
documentation config iprops html =
  HH.section
    ( [ HP.classes documentationClasses ] <&> iprops )
    [ HH.h1
        [ HP.classes headerClasses ]
        [ HH.text config.header ]
    , HH.h2
        [ HP.classes subheaderClasses ]
        [ HH.text config.subheader ]
    , HH.div
      [ HP.class_ (HH.ClassName "mt-5") ]
      html
    ]

documentation_
  :: ∀ p i
   . DocumentationProps
  -> Array (HH.HTML p i)
  -> HH.HTML p i
documentation_ config = documentation config []
