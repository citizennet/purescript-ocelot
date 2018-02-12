module UIGuide.Block.Documentation (documentation) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type DocumentationProps =
  { header :: String
  , subheader :: String
  }

documentationClasses :: Array HH.ClassName
documentationClasses = HH.ClassName <$>
  [ ]

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "font-light"
  , "leading-loose"
  , "text-black-20" 
  ]

subheaderClasses :: Array HH.ClassName
subheaderClasses = HH.ClassName <$>
  [ "font-light"
  , "leading-normal"
  , "text-grey-50"
  , "text-sm"
  ]

documentation 
  :: ∀ p i
   . DocumentationProps
  -> Array (HH.HTML p i)
  -> HH.HTML p i
documentation props html =
  HH.div
    [ HP.classes documentationClasses ]
    [ HH.h1
        [ HP.classes headerClasses ] 
        [ HH.text props.header ]
    , HH.h2
        [ HP.classes subheaderClasses ]
        [ HH.text props.subheader ]
    , HH.div
      [ HP.class_ (HH.ClassName "mt-5") ]
      html
    ]

