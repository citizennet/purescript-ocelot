module Ocelot.Block.Modal where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format as Format
import Ocelot.Core.Utils (css, (<&>))

type HeaderProps p i =
  { buttons :: Array (HH.HTML p i)
  , title :: Array (HH.HTML p i)
  }

backgroundClasses :: Array HH.ClassName
backgroundClasses = HH.ClassName <$>
  [ "fixed"
  , "pin"
  , "bg-black-modal-a90"
  ]

modalClasses :: Array HH.ClassName
modalClasses = HH.ClassName <$>
  [ "fixed"
  , "pin"
  , "pb-20"
  , "m-20"
  ]

modal
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
modal iprops html =
  HH.div_
    [ HH.div
        [ HP.classes backgroundClasses ]
        []
    , HH.div
        ( [ HP.classes modalClasses ] <&> iprops )
        html
    ]

modal_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
modal_ = modal []

bodyClasses :: Array HH.ClassName
bodyClasses = HH.ClassName <$>
  [ "relative"
  , "bg-grey-95"
  , "overflow-auto"
  , "max-h-full"
  , "w-full"
  , "max-w-lg"
  , "m-auto"
  , "flex-col"
  , "flex"
  ]

body
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
body iprops html =
  HH.div
    ( [ HP.classes bodyClasses ] <&> iprops )
    html

body_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
body_ = body []

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "h-24"
  , "max-w-lg"
  , "m-auto"
  , "flex"
  ]

outerHeaderClasses :: Array HH.ClassName
outerHeaderClasses = HH.ClassName <$>
  [ "bg-white"
  , "w-full"
  , "px-6"
  , "items-center"
  , "flex"
  ]

innerHeaderClasses :: Array HH.ClassName
innerHeaderClasses = HH.ClassName <$>
  [ "container"
  , "items-center"
  , "mx-auto"
  , "flex"
  ]

header
  :: ∀ p i
   . HeaderProps p i
  -> HH.HTML p i
header props =
  HH.div
    [ HP.classes headerClasses ]
    [ HH.header
      [ HP.classes outerHeaderClasses ]
      ( [ HH.div
        [ HP.classes innerHeaderClasses ]
          [ Format.subHeading
            [ css "mb-0" ]
            props.title
          ]
        ]
        <> props.buttons
      )
    ]
