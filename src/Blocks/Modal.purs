module Ocelot.Block.Modal where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Type as Type
import Ocelot.Core.Utils ((<&>))

modalBackgroundClasses :: Array HH.ClassName
modalBackgroundClasses = HH.ClassName <$>
  [ "fixed"              -- position absolute
  , "pin"                -- pins to all corners to fill screen
  , "bg-black-modal-a90" -- transparency background
  ]

background
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
background iprops html =
  HH.div
    ( [ HP.classes modalBackgroundClasses ] <&> iprops )
    html

background_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
background_ = background []

modalContainerClasses :: Array HH.ClassName
modalContainerClasses = HH.ClassName <$>
  [ "fixed"
  , "pin"
  , "pb-20"
  , "m-20"
  ]

modalContainer
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
modalContainer iprops html =
  HH.div
    ( [ HP.classes modalContainerClasses ] <&> iprops )
    html

modalContainer_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
modalContainer_ = modalContainer []

modalClasses :: Array HH.ClassName
modalClasses = HH.ClassName <$>
  [ "relative"      -- position in normal flow of page
  , "bg-grey-95"
  , "overflow-auto" -- allow scrolling if modal > viewport size
  , "max-h-full"
  , "w-full"        -- allow full width
  , "max-w-2xl"     -- but never above breakpoint
  , "m-auto"        -- centered
  , "flex-col"
  , "flex"
  ]

modal
  :: ∀ p i
   . Array (HH.IProp HTMLdiv i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
modal iprops html =
  HH.div
    ( [ HP.classes modalClasses ] <&> iprops )
    html

modal_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
modal_ = modal []


type ModalHeaderProps p i =
  { buttons :: Array (HH.HTML p i)
  , title :: Array (HH.HTML p i)
  }

headerContainerClasses :: Array HH.ClassName
headerContainerClasses = HH.ClassName <$>
  [ "h-24"
  , "max-w-2xl"
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

modalHeader
  :: ∀ p i
   . ModalHeaderProps p i
  -> HH.HTML p i
modalHeader config =
  HH.div
    [ HP.classes headerContainerClasses ]
    [ HH.header
      [ HP.classes outerHeaderClasses ]
      ( [ HH.div
        [ HP.classes innerHeaderClasses ]
          [ HH.h2
            [ HP.classes Type.subHeadingClassesNoMargin ]
            config.title
          ]
        ]
        <> config.buttons
      )
    ]
