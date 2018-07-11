module UIGuide.Block.Backdrop where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))


backdropClasses :: Array HH.ClassName
backdropClasses = HH.ClassName <$>
  [ "p-6"
  , "flex"
  , "flex-1"
  ]

backdropDefaultClasses :: Array HH.ClassName
backdropDefaultClasses = backdropClasses <>
  ( HH.ClassName <$>
    [ "bg-grey-95"
    ]
  )

backdropWhiteClasses :: Array HH.ClassName
backdropWhiteClasses = backdropClasses <>
  ( HH.ClassName <$>
    [ "bg-white"
    ]
  )

backdropDarkClasses :: Array HH.ClassName
backdropDarkClasses = backdropClasses <>
  ( HH.ClassName <$>
    [ "bg-black"
    , "text-grey-lighter"
    ]
  )

contentClasses :: Array HH.ClassName
contentClasses = HH.ClassName <$>
  [ "flex-1"
  , "mx-6"
  , "mt-6"
  ]

backdrop
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
backdrop iprops html =
  HH.div
    ( [ HP.classes backdropDefaultClasses ] <&> iprops )
    html

backdrop_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
backdrop_ = backdrop []

backdropWhite
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
backdropWhite iprops html =
  HH.div
    ( [ HP.classes backdropWhiteClasses ] <&> iprops )
    html

backdropWhite_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
backdropWhite_ = backdropWhite []

backdropDark
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
backdropDark iprops html =
  HH.div
    ( [ HP.classes backdropDarkClasses ] <&> iprops )
    html

backdropDark_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
backdropDark_ = backdropDark []

content
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
content iprops html =
  HH.div
    ( [ HP.classes contentClasses ] <&> iprops )
    html

content_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
content_ = content []
