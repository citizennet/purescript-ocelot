module UIGuide.Block.Component (component) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type ComponentProps =
  { title :: String }


componentClasses :: Array HH.ClassName
componentClasses = HH.ClassName <$>
  [ "bg-grey-95"
  , "flex"
  , "flex-col"
  , "items-center"
  , "justify-center"
  , "my-6"
  , "p-10"
  ]

titleClasses :: Array HH.ClassName
titleClasses = HH.ClassName <$>
  [ "block"
  , "font-light"
  , "mb-10"
  , "text-grey-70"
  , "text-sm"
  , "tracking-wide"
  , "uppercase"
  ]

contentClasses :: Array HH.ClassName
contentClasses = HH.ClassName <$>
  [ "flex"
  , "flex-col"
  , "items-center"
  , "w-full"
  ]

component
  :: ∀ p i
   . ComponentProps
  -> Array (HH.HTML p i)
  -> HH.HTML p i
component props html =
  HH.div
    [ HP.classes componentClasses ]
    [ HH.h3
        [ HP.classes titleClasses ]
        [ HH.text props.title ]
    , HH.div
        [ HP.classes contentClasses ]
        html
    ]

