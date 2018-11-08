module Ocelot.Block.Input where

import Prelude

import DOM.HTML.Indexed (GlobalAttributes, HTMLinput, HTMLlabel, HTMLspan, HTMLtextarea)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.HTML.Properties ((<&>))

inputSharedClasses :: Array HH.ClassName
inputSharedClasses = HH.ClassName <$>
  [ "bg-white"
  , "border-t-2"
  , "border-b-2"
  , "font-light"
  , "cc-blue-88"
  , "border-grey-80"
  , "disabled:bg-grey-95"
  , "disabled:text-grey-70"
  , "focus:no-outline"
  , "py-2"
  ]

inputClasses :: Array HH.ClassName
inputClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "border-l-2"
    , "border-r-2"
    , "w-full"
    , "px-3"
    , "focus:border-blue-88"
    , "!focus:!disabled:hover:border-grey-70"
    ]
  )

inputGroupClasses :: Array HH.ClassName
inputGroupClasses = HH.ClassName <$>
  [ "flex"
  , "group"
  , "w-full"
  , "items-center"
  ]

mainItemClasses :: Array HH.ClassName
mainItemClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "w-full"
    , "focus:border-blue-88"
    , "focus:sibling:border-blue-88"
    , "group-hover:!focus:!disabled:border-grey-70"
    , "group-hover:!focus:!disabled:sibling:border-grey-70"
    , "disabled:sibling:bg-grey-95"
    ]
  )

centerClasses :: Array HH.ClassName
centerClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "pl-1"
    , "pr-1"
    ]
  )

leftClasses :: Array HH.ClassName
leftClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "border-l-2"
    , "pl-3"
    , "pr-1"
    ]
  )

rightClasses :: Array HH.ClassName
rightClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "border-r-2"
    , "pr-3"
    , "pl-1"
    ]
  )

mainCenterClasses :: Array HH.ClassName
mainCenterClasses = mainItemClasses <> centerClasses

mainLeftClasses :: Array HH.ClassName
mainLeftClasses = mainItemClasses <> leftClasses

mainRightClasses :: Array HH.ClassName
mainRightClasses = mainItemClasses <> rightClasses

addonClasses :: Array HH.ClassName
addonClasses = inputSharedClasses <>
  ( HH.ClassName <$>
    [ "cursor-pointer"
    , "text-grey-70"
    ]
  )

addonCenterClasses :: Array HH.ClassName
addonCenterClasses = addonClasses <> centerClasses

addonLeftClassess :: Array HH.ClassName
addonLeftClassess = addonClasses <> leftClasses <>
  ( HH.ClassName <$>
    [ "order-start"
    ]
  )

addonRightClasses :: Array HH.ClassName
addonRightClasses = addonClasses <> rightClasses

borderClasses :: Array HH.ClassName
borderClasses = []

borderLeftClasses :: Array HH.ClassName
borderLeftClasses = borderClasses <>
  ( HH.ClassName <$>
    [ "border-r"
    , "pr-3"
    , "order-start"
    ]
  )

borderRightClasses :: Array HH.ClassName
borderRightClasses = borderClasses <>
  ( HH.ClassName <$>
    [ "border-l"
    , "pl-3"
    ]
  )

textareaClasses :: Array HH.ClassName
textareaClasses = inputClasses <>
  ( HH.ClassName <$>
    [ "min-h-40"
    ]
  )

input
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
input iprops =
  HH.input ( [ HP.classes inputClasses ] <&> iprops )

inputGroup
  :: ∀ p i
   . Array (HH.IProp HTMLlabel i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
inputGroup = inputGroup' HH.label

inputGroup'
  :: ∀ p r i
   . (Array (HH.IProp (GlobalAttributes r) i) -> Array (HH.HTML p i) -> HH.HTML p i)
  -> Array (HH.IProp (GlobalAttributes r) i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
inputGroup' elem iprops html =
  elem
    ( [ HP.classes inputGroupClasses ] <&> iprops )
    html

inputGroup_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
inputGroup_ = inputGroup []

inputCenter
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
inputCenter iprops =
  HH.input
    ( [ HP.classes mainCenterClasses ] <&> iprops )

inputLeft
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
inputLeft iprops =
  HH.input
    ( [ HP.classes mainLeftClasses ] <&> iprops )

inputRight
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
inputRight iprops =
  HH.input
    ( [ HP.classes mainRightClasses ] <&> iprops )

addonCenter
  :: ∀ p i
   . Array (HH.IProp HTMLspan i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
addonCenter iprops html =
  HH.span
    ( [ HP.classes addonCenterClasses ] <&> iprops )
    html

addonCenter_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
addonCenter_ = addonCenter []

addonLeft
  :: ∀ p i
   . Array (HH.IProp HTMLspan i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
addonLeft iprops html =
  HH.span
    ( [ HP.classes addonLeftClassess ] <&> iprops )
    html

addonLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
addonLeft_ = addonLeft []

addonRight
  :: ∀ p i
   . Array (HH.IProp HTMLspan i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
addonRight iprops html =
  HH.span
    ( [ HP.classes addonRightClasses ] <&> iprops )
    html

addonRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
addonRight_ = addonRight []

borderLeft
  :: ∀ p i
   . Array (HH.IProp HTMLspan i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
borderLeft iprops html =
  addonLeft_
    [ HH.span
      ( [ HP.classes borderLeftClasses ] <&> iprops )
      html
    ]

borderLeft_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
borderLeft_ = borderLeft []

borderRight
  :: ∀ p i
   . Array (HH.IProp HTMLspan i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
borderRight iprops html =
  addonRight_
    [ HH.span
      ( [ HP.classes borderRightClasses ] <&> iprops )
      html
    ]

borderRight_
  :: ∀ p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
borderRight_ = borderRight []

percentage
  :: ∀ p i
   . Array (HH.IProp HTMLlabel i)
  -> Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
percentage lprops iprops =
  inputGroup lprops [ inputLeft iprops, addonRight_ [ HH.text "%" ] ]

percentage_
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
percentage_ = percentage []

currency
  :: ∀ p i
   . Array (HH.IProp HTMLlabel i)
  -> Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
currency lprops iprops =
  inputGroup lprops [ inputRight iprops, addonLeft_ [ HH.text "$" ] ]

currency_
  :: ∀ p i
   . Array (HH.IProp HTMLinput i)
  -> HH.HTML p i
currency_ = currency []

textarea
  :: ∀ p i
   . Array (HH.IProp HTMLtextarea i)
  -> HH.HTML p i
textarea iprops =
  HH.textarea
    ( [ HP.classes textareaClasses ] <&> iprops )
