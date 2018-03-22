module UIGuide.Block.Backdrop
  ( backdrop
  , backdrop_
  , content
  , content_
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Core.Utils ((<&>))

backdropClasses :: Array HH.ClassName
backdropClasses = HH.ClassName <$>
  [ "bg-grey-95"
  , "border-dotted"
  , "border"
  , "rounded-sm"
  , "flex"
  , "items-stretch"
  , "my-6"
  , "p-6"
  ]

contentClasses :: Array HH.ClassName
contentClasses = HH.ClassName <$>
  [ "flex-1"
  , "flex"
  , "m-6"
  ]

backdrop
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
backdrop iprops html =
  HH.div
    ( [ HP.classes backdropClasses ] <&> iprops )
    html

backdrop_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
backdrop_ = backdrop []

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
